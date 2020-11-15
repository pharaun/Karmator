use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::runtime::Runtime;
use tokio::sync::mpsc;

// Alternative if need tokio 0.3
// use tokio_compat_02::FutureExt;

use serde::{Deserialize, Serialize};
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::sync::Arc;
use std::default::Default;
use std::env;
use std::result::Result;


// Type alias for msg_id
type MsgId = Arc<RelaxedCounter>;

fn main() {
    Runtime::new()
        .expect("Failed to create Tokio runtime")
        .block_on(async_main());
}

async fn async_main() -> Result<(), Box<dyn std::error::Error>> {
    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    {
        // Post a message
        let msg = slack::chat::PostMessageRequest {
            channel: "karmator-devs",
            text: "Pogger, i'm starting up!",
            as_user: Some(true),
            ..Default::default()
        };
        slack::chat::post_message(&client, &token, &msg).await;


        // We use .compat() from FutureExt specifically because the api client is on reqwest which
        // uses tokio 0.2 and we are going to be using tokio 0.3 (for tungstein for websockets)
        //
        // One possibility could be to:
        // You can provide your own client by implementing the async or sync versions of SlackWebRequestSender.
        //
        // Which would let me get off requwest onto something that is on tokio 0.3 (this may have
        // merit)
        //let request = slack::rtm::StartRequest::default();
        //let response = slack::rtm::start(&client, &token, &request).compat().await;
        let response = slack::rtm::connect(&client, &token).await;

        if let Ok(response) = response {
            println!("Control - Got an ok reply");

            if let Some(ws_url) = response.url {
                println!("Control - \tGot a WS url: {:?}", ws_url);

                let ws_response = tungstenite::connect_async(ws_url).await;

                if let Ok((mut ws_stream, ws_response)) = ws_response {
                    println!("Control - \t\tWS connection established");

                    // Setup the mpsc channel
                    let (mut tx, mut rx) = mpsc::channel(32);

                    // Split the stream
                    let (mut ws_write, mut ws_read) = ws_stream.split();

                    // Spawn the inbound ws stream processor
                    let inbound = tokio::spawn(async move {
                        loop {
                            if let Some(Ok(ws_msg)) = ws_read.next().await {
                                let msg_id = msg_id.clone();
                                let tx2 = tx.clone();

                                tokio::spawn(async move {
                                    process_inbound_message(msg_id, ws_msg, tx2).await;
                                });

                            }
                        }
                    });

                    // Spawn outbound ws processor
                    // TODO: need to make sure to wait on sending till we recieve the hello event
                    // could be a oneshot or some sort of flag which it waits till inbound has
                    // gotten the hello
                    let outbound = tokio::spawn(async move {
                        loop {
                            while let Some(message) = rx.recv().await {
                                println!("Outbound - {:?}", message);

                                // TODO: present some way to do plain vs fancy message, and if
                                // fancy do a webapi post, otherwise dump into the WS
                                //
                                // TODO: look into tracking the sent message with a confirmation
                                // that it was sent (via msg id) and if it fails, resend with
                                // backoff
                                //
                                // TODO: find a way to handle the ping/pong cycle and monitor
                                ws_write.send(message).await;
                            }
                        }
                    });

                    // Wait till either exits (error) then begin recovery
                    let res = tokio::try_join!(inbound, outbound);

                    match res {
                        Ok((first, second)) => println!("Control - \t\t\tBoth exited fine"),
                        Err(err) => println!("Control - \t\t\tSomething failed: {:?}", err),
                    }
                } else {
                    println!("Control - \t\t{:?}", ws_response.err());
                }
            } else {
                println!("Control - \tNo WS url");
            }
        } else {
            println!("Control - {:?}", response);
        }
    }
    Ok(())
}


async fn process_inbound_message(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>
) -> Result<(), Box<dyn std::error::Error>>
{
    let id = msg_id.inc();

    println!("Inbound - id = {:?}", id);

    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::tungstenite::Message::Text(x) => Some(x),
        _ => {
            println!("Inbound - \tUnsupported ws message type - {:?}", msg);
            None
        },
    };
    println!("Inbound - raw event = {:?}", raw_msg);

    if let Some(s) = raw_msg {
        if let Some(e) = parse_event(s) {
            match e {
                Event::UserEvent(event) => {
                    // Check if its a message/certain string, if so, reply
                    match event {
                        UserEvent::Message {
                            channel: Some(c),
                            text: t,
                            subtype: _,
                            hidden: _,
                            user: _,
                            ts: _,
                        } if (c == "CAF6S4TRT".to_string()) && (t == "!kappa".to_string()) => {
                            // Send out a message
                            let ws_msg = json!({
                                "id": 1,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": "mod4 kappa",
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;

                            println!("Inbound - \t\tKappa was sent");
                        },
                        _ => println!("Inbound - \t\tNo action taken"),
                    }
                },

                Event::SystemControl(sc) => (),
                Event::MessageControl(mc) => (),
            }
        }
    }
    Ok(())
}


fn parse_event(s: String) -> Option<Event> {
    // Try to parse an System Control or Message Control, then an Event (to pass on downstream)
    let sc: Result<SystemControl, serde_json::Error> = serde_json::from_str(&s);
    match sc {
        Ok(sc) => {
            println!("Inbound - \tSystem Control = {:?}", sc);
            Some(Event::SystemControl(sc))
        },
        Err(_) => {
            // Try to parse a Message Control
            let mc: Result<MessageControl, serde_json::Error> = serde_json::from_str(&s);
            match mc {
                Ok(mc) => {
                    println!("Inbound - \tMessage Control = {:?}", mc);
                    Some(Event::MessageControl(mc))
                },
                Err(_) => {
                    // Try to parse an event
                    let event: Result<UserEvent, serde_json::Error> = serde_json::from_str(&s);
                    match event {
                        Ok(event) => {
                            println!("Inbound - \tEvent = {:?}", event);
                            Some(Event::UserEvent(event))
                        },
                        Err(_) => {
                            // TODO: for now print to stderr what didn't get deserialized
                            // later can have config option to log to file the error or not
                            println!("Inbound - \t\tFail parse = {:?}", s);
                            None
                        },
                    }
                },
            }
        },
    }
}


// Containing enum to ease the result chains
#[derive(Debug)]
enum Event {
    UserEvent(UserEvent),
    SystemControl(SystemControl),
    MessageControl(MessageControl),
}


#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum UserEvent {
    // These events are what plugins will care for:
    //
    // Slack messages
    // TODO: more involved here for now basics
    Message {
        subtype: Option<String>,
        hidden: Option<bool>,
        channel: Option<String>,
        user: String,
        text: String,
        ts: String,
    },

    // TODO: consider looking at slack_api for types to reuse here
    ReactionAdded {
        user: String,
        reaction: String,
        item_user: Option<String>,
        // item: ListResponseItem
        event_ts: String,
    },
    ReactionRemoved {
        user: String,
        reaction: String,
        item_user: Option<String>,
        // item: ListResponseItem
        event_ts: String,
    },
}


#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum SystemControl {
    // TODO: separate the slack Control messages (hello/bye/ping/pong)
    // from the other messages that will be of interest to plugins
    //
    // First message upon successful connection establishment
    Hello,

    // Client should reconnect after getting such message
    Goodbye,

    // Reply to Ping = { type: ping, id: num }
    Pong { reply_to: usize },
}


#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum MessageControl {
    MessageSent {
        ok: bool,
        reply_to: usize,
        text: String,
        ts: String
    },

    MessageError {
        ok: bool,
        reply_to: usize,
        error: ErrorDetail,
    },
}

#[derive(Debug, Deserialize)]
struct ErrorDetail {
    code: usize,
    msg: String,
}
