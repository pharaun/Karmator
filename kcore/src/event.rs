use tokio_tungstenite::tungstenite;

use serde::Deserialize;

use std::result::Result;

use log::{debug, error, info};

use tokio::sync::mpsc;

use crate::connection_state::ConnectionState;
use crate::sanitizer;
use crate::slack::Message;

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
#[expect(dead_code)]
enum Event {
    // First message upon successful connection establishment
    Hello {
        num_connections: u32,
    },

    // Client should reconnect after this
    Disconnect {
        reason: String,
    },

    // TODO: Find any other types that we need to be aware of so that we can
    // acknowledge them then forward it to the library user's code
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
        // This is converted into a json value, since we don't care about the content
        // of this payload, but we do need to know the envelope id for acknowledging
        event: serde_json::Value,
    },
}

#[derive(Debug)]
pub enum Reply {
    Message(Message),

    // Acknowledge reception of event
    Acknowledge(String),

    // Control
    Ping(tungstenite::Bytes),
    Pong(tungstenite::Bytes),
}

fn parse_event(s: &str) -> Option<Event> {
    let res = serde_json::from_str::<Event>(s).map_err(|e| format!("{e:?}"));

    debug!("Parsed Event: {res:?}");

    if res.is_err() {
        error!("parse_event - Error: {res:?}\n{s:?}\n");
    }
    res.ok()
}

pub async fn send_simple_message(
    tx: &mut mpsc::Sender<Reply>,
    channel: String,
    thread_ts: Option<String>,
    text: String,
) -> Result<(), &'static str> {
    if text.is_empty() {
        return Err("Empty string, not sending");
    }

    // TODO: track if it got sanitized or not
    let text = sanitizer::sanitize_output(&text);

    tx.send(Reply::Message(Message {
        channel,
        text,
        thread_ts,
    }))
    .await
    .map_err(|_| "Error sending")
}

pub(crate) async fn send_slack_ping(
    tx: &mut mpsc::Sender<Reply>,
    connection_state: &ConnectionState,
) -> Result<(), &'static str> {
    connection_state.ping_sent().await;
    tx.send(Reply::Ping(tungstenite::Bytes::new()))
        .await
        .map_err(|_| "Error sending")
}

pub(crate) async fn process_control_message(
    tx: mpsc::Sender<Reply>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> Result<Option<serde_json::Value>, Box<dyn std::error::Error>> {
    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::Message::Text(x) => {
            connection_state.message_received().await;
            Some(x)
        }

        tungstenite::Message::Ping(x) => {
            tx.send(Reply::Pong(x)).await?;
            None
        }

        // Reply from our heartbeat ping
        tungstenite::Message::Pong(_) => None,

        tungstenite::Message::Close(reason) => {
            info!("Slack Websocket - Close reason: {reason:?}");
            connection_state.reconnect();
            None
        }

        tungstenite::Message::Binary(_) => {
            error!("Slack Websocket - Unsupported websocket type: {msg:?}");
            None
        }

        tungstenite::Message::Frame(_) => {
            error!("Slack Websocket - Unsupported websocket type: {msg:?}");
            None
        }
    };

    if let Some(e) = raw_msg.as_deref().and_then(parse_event) {
        match e {
            Event::Hello { num_connections: _ } => {
                // Hold on sending messages till this is received.
                connection_state.connected();

                Ok(None)
            }
            Event::Disconnect { reason: r } => {
                // When this is received, reconnect
                connection_state.reconnect();

                info!("Slack Websocket - Disconnect reason: {r:?}");

                Ok(None)
            }
            Event::EventsApi {
                envelope_id: ei,
                accepts_response_payload: _,
                payload: pay,
            } => {
                tx.send(Reply::Acknowledge(ei)).await?;

                match pay {
                    Payload::EventCallback { event: e } => Ok(Some(e)),
                }
            }
        }
    } else {
        Ok(None)
    }
}
