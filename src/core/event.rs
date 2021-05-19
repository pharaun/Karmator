use tokio_tungstenite as tungstenite;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use serde::Deserialize;
use serde_json::json;

use std::result::Result;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::RwLock;
use std::time::Instant;
use chrono::prelude::Utc;

use tokio::sync::mpsc;

use crate::core::santizer;


// Type alias for msg_id
pub type MsgId = Arc<RelaxedCounter>;


#[derive(Debug)]
enum Event {
    UserEvent(UserEvent),
    SystemControl(SystemControl),
    MessageControl(MessageControl),
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

    // TODO: consider looking at slack_api for types to reuse here
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


#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum SystemControl {
    // First message upon successful connection establishment
    Hello,

    // Client should reconnect after getting such message
    Goodbye,

    // Reply to Ping = { type: ping, id: num }
    Pong { reply_to: usize, timestamp: Option<i64> },
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


fn parse_event(s: String) -> Option<Event> {
    // Return User Events first, then the other two
    serde_json::from_str(&s).and_then(|ue| {
        Ok(Event::UserEvent(ue))
    }).or_else(|_| {
        serde_json::from_str(&s).and_then(|sc| {
            Ok(Event::SystemControl(sc))
        }).or_else(|_| {
            serde_json::from_str(&s).and_then(|mc| {
                Ok(Event::MessageControl(mc))
            }).or_else(|x| {
                // TODO: for now print to stderr what didn't get deserialized
                // later can have config option to log to file the error or not
                Err(x)
            })
        })
    }).ok()
}


pub async fn send_simple_message(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    channel: String,
    thread_ts: Option<String>,
    text: String
) -> Result<(), &'static str> {
    if text.is_empty() {
        return Err("Empty string, not sending");
    }

    // TODO: track if it got santized or not
    let text = santizer::santize_output(&text);

    // TODO: register this message send to be tracked later
    let ws_msg = match thread_ts {
        Some(ts) => json!({
            "id": msg_id.inc(),
            "type": "message",
            "channel": channel,
            "text": text,
            "thread_ts": ts,
        }).to_string(),
        None => json!({
            "id": msg_id.inc(),
            "type": "message",
            "channel": channel,
            "text": text,
        }).to_string(),
    };
    tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await.map_err(|_| "Error sending")
}


pub async fn send_slack_ping(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    last_ping_sent: Arc<RwLock<Instant>>,
) -> Result<(), &'static str> {
    {
        let mut timer = last_ping_sent.write().unwrap();
        *timer = Instant::now();
    }
    let ws_msg = json!({
	"id": msg_id.inc(),
	"type": "ping",
        "timestamp": Utc::now().timestamp_millis(),
    }).to_string();
    tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await.map_err(|_| "Error sending")
}


pub async fn process_control_message(
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    can_send: Arc<AtomicBool>,
    reconnect: Arc<AtomicBool>,
    reconnect_count: Arc<RelaxedCounter>,
    last_message_recieved: Arc<RwLock<Instant>>,
    msg: tungstenite::tungstenite::Message,
) -> Result<Option<UserEvent>, Box<dyn std::error::Error>>
{
    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::tungstenite::Message::Text(x) => {
            {
                let mut timer = last_message_recieved.write().unwrap();
                *timer = Instant::now();
            }
            Some(x)
        },

        tungstenite::tungstenite::Message::Ping(x) => {
            let _ = tx.send(tungstenite::tungstenite::Message::Pong(x)).await;
            None
        },

        tungstenite::tungstenite::Message::Close(reason) => {
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
            Event::UserEvent(event)    => Ok(Some(event)),
            Event::MessageControl(_mc) => Ok(None),
            Event::SystemControl(sc)   => {
                match sc {
                    SystemControl::Hello => {
                        // Hold on sending messages till this is recieved.
                        can_send.store(true, Ordering::Relaxed);
                        reconnect_count.reset();
                    },
                    SystemControl::Goodbye => {
                        // When this is recieved, reconnect
                        reconnect.store(true, Ordering::Relaxed);
                        can_send.store(false, Ordering::Relaxed);
                    },
                    SystemControl::Pong{reply_to: _, timestamp: _ts} => {
                    //    if let Some(old) = _ts {
                    //        let now = Utc::now().timestamp_millis();
                    //        println!("SYSTEM [Inbound]: Ping delta: {:?}ms", now - old);
                    //    }
                    },
                }
                Ok(None)
            },
        }
    } else {
        Ok(None)
    }
}
