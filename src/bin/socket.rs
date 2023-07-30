use serde::Deserialize;
use serde_json::json;
use tokio_tungstenite as tungstenite;
use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use std::env;
use std::result::Result;

#[derive(Deserialize, Debug)]
struct Conn {
    #[allow(dead_code)]
    ok: bool,
    url: String
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
#[allow(dead_code)]
enum Event {
    // First message upon successful connection establishment
    Hello {
        num_connections: u32
    },

    // Client should reconnect after this
    Disconnect {
        reason: String,
    },

    // For now messages too
    EventsApi {
        envelope_id: String,
        accepts_response_payload: bool,
        // payload: blah
    },
//{
//    "envelope_id":"<id>",
//    "type":"events_api",
//    "accepts_response_payload":false,
//
//    "payload":{
//        "event":{
//            "type":"message",
//            "text":"asdf",
//            "user":"<user-id>",
//            "ts":"<timestamp>",
//            "channel":"<channel-id>",
//        },
//        "type":"event_callback",
//    },
//}
}


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app_token = env::var("SLACK_APP_TOKEN").map_err(|_| "SLACK_APP_TOKEN env var must be set")?;

    let client = reqwest::Client::new();
    let res = client.post("https://slack.com/api/apps.connections.open")
        .header("Content-type", "application/x-www-form-urlencoded")
        .header("Authorization", format!("Bearer {}", app_token))
        .send()
        .await.map_err(
            |x| format!("{:?}", x)
        )?
        .text()
        .await.map_err(
            |x| format!("{:?}", x)
        )?;

    let conn = serde_json::from_str::<Conn>(
        &res
    ).map_err(
        |x| format!("{:?}", x)
    )?;

    println!("body: {:?}", conn);

    let (ws_stream, _) = tungstenite::connect_async(
            format!("{}&debug_reconnects=true", conn.url)
        ).await.map_err(|e| format!("Control - \t\t{:?}", e))?;








    // Split the stream
    let (mut ws_write, mut ws_read) = ws_stream.split();

    // Message Tx/Rx from the message loop to the downstream handlers
    let (tx, mut rx) = mpsc::channel(32);

    loop {
        tokio::select! {
            ws_msg = ws_read.next() => {
                match ws_msg {
                    Some(Ok(ws_msg)) => {
                        println!("rcv - {:?}", ws_msg);

                        let ue = process_control_message(
                            tx.clone(),
                            ws_msg,
                        ).await;

                        // If there's an user event returned spawn off the user event processor
                        match ue {
                            Ok(Some(event)) => {
                                println!("event -> {:?}", event);
                            },
                            Ok(None) => {
                                println!("UE is none");
                            },
                            Err(e) => {
                                println!("UE is err -> {:?}", e);
                            },
                        }
                    },

                    Some(Err(e)) => {
                        println!("error - {:?}", e);
                    },

                    // Stream is done/closed
                    None => {
                        println!("Terminated by stream closing");
                    },
                }
            },

            Some(message) = rx.recv() => {
                match ws_write.send(message.clone()).await {
                    Ok(_) => {
                        println!("send -> {:?}", message);
                    },
                    Err(e) => {
                        eprintln!("SYSTEM [Slack RTM]: Connection error: {:?}", e);
                    },
                }
            },
        }
    }
}


fn parse_event(s: String) -> Result<Event, String> {
    serde_json::from_str(&s).and_then(|sc| { Ok(sc) }).or_else(|x| { Err(format!("{:?}", x)) })
}

async fn process_control_message(
    tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    msg: tungstenite::tungstenite::Message,
) -> Result<Option<&'static str>, Box<dyn std::error::Error>>
{
    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::tungstenite::Message::Text(x) => {
            Some(x)
        },

        tungstenite::tungstenite::Message::Ping(x) => {
            let _ = tx.send(tungstenite::tungstenite::Message::Pong(x)).await;
            None
        },

        tungstenite::tungstenite::Message::Close(reason) => {
            println!("SYSTEM [Inbound]: Close: {:?}", reason);
            None
        },

        _ => {
            eprintln!("SYSTEM [Inbound]: Unsupported websocket type: {:?}", msg);
            None
        },
    };

    match raw_msg.ok_or("asdf".to_string()).map(parse_event) {
        Ok(Ok(Event::Hello { .. })) => {
            println!("Can send now!");
            Ok(None)
        },
        Ok(Ok(Event::Disconnect { reason: x})) => {
            println!("Disconnecting - {:?}", x);
            Ok(None)
        },
        Ok(Ok(Event::EventsApi { envelope_id: ei, accepts_response_payload: _ })) => {
            let ws_msg = json!({
                "envelope_id": ei,
            }).to_string();
            let _ = tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;

            Ok(Some("message"))
        },
        Ok(Err(x)) | Err(x) => {
            println!("error-parsing: {:?}", x);
            Ok(None)
        },
    }
}
