use tokio_tungstenite::tungstenite;

use serde::Deserialize;

use anyhow::Result as AResult;
use std::result::Result;

use log::{debug, error, info, warn};

use tokio::sync::mpsc;
use tokio::sync::mpsc::error::TrySendError;

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
pub(crate) enum WebsocketReply {
    // Acknowledge reception of event
    Acknowledge(String),

    // Control
    Ping(tungstenite::Bytes),
    Pong(tungstenite::Bytes),
}

#[derive(Debug)]
pub enum HttpReply {
    Message(Message),
}


fn parse_event(s: &str) -> Option<Event> {
    let res = serde_json::from_str::<Event>(s).map_err(|e| format!("{e:?}"));

    debug!("Parsed Event: {res:?}");

    if res.is_err() {
        warn!("parse_event - Error: {res:?}\n{s:?}\n");
    }
    res.ok()
}

pub async fn send_simple_message(
    tx: &mpsc::Sender<HttpReply>,
    channel: String,
    thread_ts: Option<String>,
    text: String,
) -> Result<(), &'static str> {
    if text.is_empty() {
        return Err("Empty string, not sending");
    }

    // TODO: track if it got sanitized or not
    let text = sanitizer::sanitize_output(&text);

    tx.send(HttpReply::Message(Message {
        channel,
        text,
        thread_ts,
    }))
    .await
    .map_err(|_| "Error sending")
}

pub(crate) async fn send_slack_ping(
    wtx: &mpsc::Sender<WebsocketReply>,
    connection_state: &ConnectionState,
) -> Result<(), &'static str> {
    match wtx.try_send(WebsocketReply::Ping(tungstenite::Bytes::new())) {
        Ok(()) => {
            connection_state.ping_sent().await;
            Ok(())
        },
        // Drop it, if we reach this point the socket is in bad state and the eventual
        // ping fail will trigger a reconnect
        Err(TrySendError::Full(_)) => Ok(()),
        Err(TrySendError::Closed(_)) => Err("Channel Closed"),
    }
}

pub(crate) async fn process_control_message(
    wtx: &mpsc::Sender<WebsocketReply>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> AResult<Option<serde_json::Value>> {
    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::Message::Text(x) => {
            connection_state.message_received().await;
            Some(x)
        }

        tungstenite::Message::Ping(x) => {
            match wtx.try_send(WebsocketReply::Pong(x)) {
                Ok(()) | Err(TrySendError::Full(_)) => (),
                Err(TrySendError::Closed(_)) => {
                    error!("Ping channel closed");
                },
            }
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
                // TODO: not sure how to handle this yet, try_send?
                // TODO: We probs want to cache recently sent acknowledgement to handle reconnects
                wtx.send(WebsocketReply::Acknowledge(ei)).await?;

                match pay {
                    Payload::EventCallback { event: e } => Ok(Some(e)),
                }
            }
        }
    } else {
        Ok(None)
    }
}
