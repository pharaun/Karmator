use tokio_tungstenite::tungstenite;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use serde::Deserialize;
use serde::Serialize;

use std::result::Result;
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Instant;

use tokio::sync::mpsc;

use crate::core::santizer;


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
        payload: Payload,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Payload {
    EventCallback {
        event: UserEvent,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum UserEvent {
    // These events are what plugins will care for:
    //
    // Slack messages
    // TODO: more involved here for now basics
    Message {
        subtype: Option<String>,
        hidden: Option<bool>,
        #[serde(rename = "channel")]
        channel_id: Option<String>,
        #[serde(rename = "user")]
        user_id: String,
        text: String,
        ts: String,
        thread_ts: Option<String>,
    },

    ReactionAdded {
        #[serde(rename = "user")]
        user_id: String,
        reaction: String,
        item_user: Option<String>,
        item: ReactionItem,
        event_ts: String,
        ts: String,
    },
    ReactionRemoved {
        #[serde(rename = "user")]
        user_id: String,
        reaction: String,
        item_user: Option<String>,
        item: ReactionItem,
        event_ts: String,
        ts: String,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ReactionItem {
    Message {
        #[serde(rename = "channel")]
        // I think this is mandatory?
        channel_id: String,
        ts: String,
    },
    File {
        file: String,
    },
    FileComment {
        file: String,
        file_comment: String,
    },
}

#[derive(Debug)]
pub enum Reply {
    Message(Message),

    // Acknowledge reception of event
    Acknowledge(String),

    // Control
    Ping(Vec<u8>),
    Pong(Vec<u8>),
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub struct Message {
    channel: String,
    text: String,
    thread_ts: Option<String>,
}


fn parse_event(s: String) -> Option<Event> {
    let res = serde_json::from_str::<Event>(
        &s
    ).map_err(
        |x| format!("{:?}", x)
    );

    if res.is_err() {
        println!("Error: {:?}", res);
    }
    res.ok()
}


pub async fn send_simple_message(
    tx: &mut mpsc::Sender<Reply>,
    channel: String,
    thread_ts: Option<String>,
    text: String
) -> Result<(), &'static str> {
    if text.is_empty() {
        return Err("Empty string, not sending");
    }

    // TODO: track if it got santized or not
    let text = santizer::santize_output(&text);

    tx.send(Reply::Message(Message {
        channel: channel,
        text: text,
        thread_ts: thread_ts
    })).await.map_err(|_| "Error sending")
}


pub async fn send_slack_ping(
    tx: &mut mpsc::Sender<Reply>,
    last_ping_sent: Arc<RwLock<Instant>>,
) -> Result<(), &'static str> {
    {
        let mut timer = last_ping_sent.write().unwrap();
        *timer = Instant::now();
    }
    tx.send(
        Reply::Ping(vec![])
    ).await.map_err(|_| "Error sending")
}


pub async fn process_control_message(
    tx: mpsc::Sender<Reply>,
    can_send: Arc<AtomicBool>,
    reconnect: Arc<AtomicBool>,
    reconnect_count: Arc<RelaxedCounter>,
    last_message_recieved: Arc<RwLock<Instant>>,
    msg: tungstenite::Message,
) -> Result<Option<UserEvent>, Box<dyn std::error::Error>>
{
    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::Message::Text(x) => {
            {
                let mut timer = last_message_recieved.write().unwrap();
                *timer = Instant::now();
            }
            Some(x)
        },

        tungstenite::Message::Ping(x) => {
            let _ = tx.send(Reply::Pong(x)).await;
            None
        },

        tungstenite::Message::Close(reason) => {
            println!("SYSTEM [Inbound]: Close: {:?}", reason);

            reconnect.store(true, Ordering::Relaxed);
            can_send.store(false, Ordering::Relaxed);
            None
        },

        _ => {
            eprintln!("SYSTEM [Inbound]: Unsupported websocket type: {:?}", msg);
            None
        },
    };

    if let Some(e) = raw_msg.and_then(parse_event) {
        match e {
            Event::Hello {num_connections: _} => {
                // Hold on sending messages till this is recieved.
                can_send.store(true, Ordering::Relaxed);
                reconnect_count.reset();

                Ok(None)
            },
            Event::Disconnect {reason: _} => {
                // When this is recieved, reconnect
                reconnect.store(true, Ordering::Relaxed);
                can_send.store(false, Ordering::Relaxed);

                Ok(None)
            },
            Event::EventsApi {envelope_id: ei, accepts_response_payload: _, payload: pay} => {
                let _ = tx.send(Reply::Acknowledge(ei)).await;

                match pay {
                    Payload::EventCallback {event: e} => Ok(Some(e)),
                }
            },
        }
    } else {
        Ok(None)
    }
}
