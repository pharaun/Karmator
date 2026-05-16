use tokio_tungstenite::tungstenite;
use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;

use serde::Deserialize;

use anyhow::Result as AResult;

use futures_util::stream::SplitSink;

use log::{debug, error, info, warn};

use tokio::net::TcpStream;

use crate::bot::send_acknowledge;
use crate::bot::send_pong;
use crate::connection_state::ConnectionState;

// Other unhandled types (interactive events):
// Reply with data: slash_commands, view_submission, block_suggestion
// Acknowlegement: block_actions, app_home_opened
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

    // Regular events
    // TODO: Have a way to extract an Envelope event for acknowledgement
    // Then re-parse it into the actual Payload/others for downstream processing
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

fn parse_event(s: &str) -> Option<Event> {
    let res = serde_json::from_str::<Event>(s).map_err(|e| format!("{e:?}"));

    debug!("Parsed Event: {res:?}");

    if res.is_err() {
        warn!("parse_event - Error: {res:?}\n{s:?}\n");
    }
    res.ok()
}

async fn process_websocket_message(
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> Option<tungstenite::Utf8Bytes> {
    match msg {
        tungstenite::Message::Text(x) => Some(x),

        tungstenite::Message::Ping(x) => {
            send_pong(connection_state, ws_write, x).await;
            None
        }

        // Reply from our heartbeat ping
        tungstenite::Message::Pong(_) => {
            // We wait for a ping-pong cycle ~30s once a connection
            // gets established before we reset the backoff timer,
            // this ensures it only get reset on an "alive" connection
            connection_state.reset_backoff_timer().await;
            None
        }

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
    }
}

async fn process_slack_control_message(
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    connection_state: &ConnectionState,
    msg: Event,
) -> Option<serde_json::Value> {
    match msg {
        Event::Hello { num_connections: _ } => {
            // Hold on sending messages till this is received.
            connection_state.connected().await;
            None
        }
        Event::Disconnect { reason: r } => {
            // When this is received, reconnect
            connection_state.reconnect();
            info!("Slack Websocket - Disconnect reason: {r:?}");
            None
        }
        Event::EventsApi {
            envelope_id: ei,
            accepts_response_payload: _,
            payload: pay,
        } => {
            // TODO: We probs want to cache recently sent acknowledgement to handle reconnects
            send_acknowledge(connection_state, ws_write, ei).await;
            match pay {
                Payload::EventCallback { event: e } => Some(e),
            }
        }
    }
}

pub(crate) async fn process_control_message(
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> AResult<Option<serde_json::Value>> {
    // Parse incoming message
    connection_state.message_received().await;

    let raw_msg = process_websocket_message(ws_write, connection_state, msg).await;

    if let Some(e) = raw_msg.as_deref().and_then(parse_event) {
        Ok(process_slack_control_message(ws_write, connection_state, e).await)
    } else {
        Ok(None)
    }
}
