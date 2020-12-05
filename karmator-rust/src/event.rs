use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;
use tokio::sync::oneshot;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::sync::Arc;
use std::result::Result;

use crate::database::{RunQuery, ResQuery};
use crate::parser::santizer;


// Type alias for msg_id
pub type MsgId = Arc<RelaxedCounter>;


#[derive(Debug)]
pub enum Event {
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
        // item: ListResponseItem
        event_ts: String,
    },
    ReactionRemoved {
        #[serde(rename = "user")]
        user_id: String,
        reaction: String,
        item_user: Option<String>,
        // item: ListResponseItem
        event_ts: String,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum SystemControl {
    // First message upon successful connection establishment
    Hello,

    // Client should reconnect after getting such message
    Goodbye,

    // Reply to Ping = { type: ping, id: num }
    Pong { reply_to: usize },
}


#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum MessageControl {
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
pub struct ErrorDetail {
    code: usize,
    msg: String,
}


pub fn parse_event(s: String) -> Option<Event> {
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
                //eprintln!("Inbound - \tFail parse = {:?}", s);
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


pub async fn send_query(
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    query: RunQuery,
) -> Result<ResQuery, &'static str> {
    let (tx, rx) = oneshot::channel();
    sql_tx.send((query, Some(tx))).await.map_err(|_| "Error sending")?;
    rx.await.map_err(|_| "Error recieving")
}
