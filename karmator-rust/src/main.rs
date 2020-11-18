use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::sync::Arc;
use std::default::Default;
use std::env;
use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;

// SQlite worker thread
use std::thread;
use futures::executor::block_on_stream;


// Use of a mod or pub mod is not actually necessary.
pub mod build_info {
   // The file has been placed there by the build script.
   include!(concat!(env!("OUT_DIR"), "/built.rs"));
}


// Type alias for msg_id
type MsgId = Arc<RelaxedCounter>;


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    // Post a message
    let msg = slack::chat::PostMessageRequest {
        channel: "karmator-devs",
        text: "Pogger, i'm starting up!",
        as_user: Some(true),
        ..Default::default()
    };
    slack::chat::post_message(&client, &token, &msg).await;

    // Work to establish the WS connection
    let response = slack::rtm::connect(&client, &token).await.map_err(|e| format!("Control - {:?}", e))?;
    println!("Control - Got an ok reply");

    let ws_url = response.url.ok_or(format!("Control - \tNo Ws url"))?;
    println!("Control - \tGot a WS url: {:?}", ws_url);

    let (ws_stream, _) = tungstenite::connect_async(ws_url).await.map_err(|e| format!("Control - \t\t{:?}", e))?;
    println!("Control - \t\tWS connection established");

    // Setup the mpsc channel
    let (tx, mut rx) = mpsc::channel(32);

    // Split the stream
    let (mut ws_write, mut ws_read) = ws_stream.split();


    // Setup the mpsc channel for sqlite and spawn the sqlite worker thread
    // TODO: define a data format for asking the db to query something on your behalf, for now
    // pass in the raw query string?
    let (sql_tx, sql_rx) = mpsc::channel(32);

    let sql_worker = thread::spawn(move || {
        println!("Sql Worker - Launching");
        process_queries(sql_rx);
        println!("Sql Worker - Exiting");
    });

    // Spawn the inbound ws stream processor
    let inbound = tokio::spawn(async move {
        loop {
            if let Some(Ok(ws_msg)) = ws_read.next().await {
                let msg_id = msg_id.clone();
                let tx2 = tx.clone();
                let sql_tx2 = sql_tx.clone();

                tokio::spawn(async move {
                    process_inbound_message(msg_id, ws_msg, tx2, sql_tx2, start_time).await;
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
    match tokio::try_join!(inbound, outbound) {
        Ok((first, second)) => println!("Control - \t\t\tBoth exited fine"),
        Err(err) => println!("Control - \t\t\tSomething failed: {:?}", err),
    }

    // The sql_tx got moved into the inbound tokio async, so when that dies....
    let res = sql_worker.join();
    println!("Control - \t\t\tSql worker: {:?}", res);

    Ok(())
}


fn process_queries(
    sql_rx: mpsc::Receiver<String>
) {
    let mut block_sql_rx = block_on_stream(sql_rx);

    while let Some(query) = block_sql_rx.next() {
        println!("Sql Worker - Query {:?}", query);
    }
}



async fn process_inbound_message(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<String>,
    start_time: DateTime<Utc>
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

    if let Some(e) = raw_msg.and_then(parse_event) {
        match e {
            Event::UserEvent(event) => {
                // Check if its a message/certain string, if so, reply
                match event {
                    // !version
                    UserEvent::Message {
                        channel: Some(c),
                        text: t,
                        subtype: _,
                        hidden: _,
                        user: u,
                        ts: _,
                    } if c == "CAF6S4TRT".to_string() => {
                        if t == "!version".to_string() {
                            let ver = build_info::PKG_VERSION;
                            let dat = build_info::BUILT_TIME_UTC;
                            let sha = build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": format!("<@{}>: Cargo Version: {} - Build Date: {}, - Build SHA: {}", u, ver, dat, sha),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!version");

                        } else if t == "!uptime".to_string() {
                            let end_time: DateTime<Utc> = Utc::now();
                            let durt = end_time.signed_duration_since(start_time).to_std().unwrap_or(
                                Duration::from_secs(3122064000));

                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": format!("<@{}>: {}", u, format_duration(durt).to_string()),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!uptime");

                        } else if t == "!help".to_string() {
                            let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote";
                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": format!("<@{}>: {}", u, help),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!help");

                        } else if t == "!github".to_string() {
                            let github = "Github repo: https://github.com/pharaun/Karmator";
                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": format!("<@{}>: {}", u, github),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!github");

                        } else if t == "!run_sql_query".to_string() {
                            sql_tx.send("select * from votes;".to_string()).await;


                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": "CAF6S4TRT",
                                "text": format!("<@{}>: Ok running query", u),
                            }).to_string();
                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;

                            println!("Inbound - \t\t!run_sql_query");
                        }
                    },

                    _ => println!("Inbound - \t\tNo action taken"),
                }
            },

            Event::SystemControl(sc) => (),
            Event::MessageControl(mc) => (),
        }
    }
    Ok(())
}


fn parse_event(s: String) -> Option<Event> {
    // Return User Events first, then the other two
    serde_json::from_str(&s).and_then(|ue| {
        println!("Inbound - \tUser Event = {:?}", ue);
        Ok(Event::UserEvent(ue))
    }).or_else(|_| {
        serde_json::from_str(&s).and_then(|sc| {
            println!("Inbound - \tSystem Control = {:?}", sc);
            Ok(Event::SystemControl(sc))
        }).or_else(|_| {
            serde_json::from_str(&s).and_then(|mc| {
                println!("Inbound - \tMessage Control = {:?}", mc);
                Ok(Event::MessageControl(mc))
            }).or_else(|x| {
                // TODO: for now print to stderr what didn't get deserialized
                // later can have config option to log to file the error or not
                eprintln!("Inbound - \tFail parse = {:?}", s);
                Err(x)
            })
        })
    }).ok()
}

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
    // Reply to message sent
    MessageSent {
        ok: bool,
        reply_to: usize,
        text: String,
        ts: String
    },

    // Reply to failed message sent
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
