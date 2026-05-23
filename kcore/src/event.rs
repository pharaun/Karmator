use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite;

use serde::Deserialize;

use anyhow::Result as AResult;

use log::{debug, error, info, warn};

use tokio::net::TcpStream;

use crate::bot::send_acknowledge;
use crate::bot::send_pong;
use crate::connection_state::ConnectionState;

#[derive(Debug, Deserialize, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Event {
    // First message upon successful connection establishment
    Hello {
        num_connections: u32,
    },

    // Client should reconnect after this
    Disconnect {
        reason: String,
    },

    #[serde(untagged)]
    Envelope(Envelope),
}

#[derive(Debug, Deserialize, PartialEq, Clone)]
#[serde(rename_all = "snake_case")]
pub struct Envelope {
    #[serde(rename = "envelope_id")]
    pub id: String,
    #[serde(rename = "type")]
    pub typ: String,
    pub accepts_response_payload: bool,
    // This is converted into a json value, since we don't care about the content
    // of this payload, but we do need to know the envelope id for acknowledging
    pub payload: serde_json::Value,
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
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> Option<tungstenite::Utf8Bytes> {
    match msg {
        tungstenite::Message::Text(x) => Some(x),

        tungstenite::Message::Ping(x) => {
            send_pong(connection_state, websocket, x).await;
            None
        }

        // Reply from our heartbeat ping
        tungstenite::Message::Pong(_) => {
            // We wait for a ping-pong cycle ~30s once a connection
            // gets established before we reset the backoff timer,
            // this ensures it only get reset on an "alive" connection
            connection_state.reset_backoff_timer();
            None
        }

        tungstenite::Message::Close(reason) => {
            info!("Slack Websocket - Close reason: {reason:?}");
            connection_state.reconnect();
            None
        }

        tungstenite::Message::Binary(x) => {
            error!("Slack Websocket - Unsupported websocket type: {x:?}");
            None
        }

        tungstenite::Message::Frame(x) => {
            error!("Slack Websocket - Unsupported websocket type: {x:?}");
            None
        }
    }
}

async fn process_slack_control_message(
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    connection_state: &ConnectionState,
    msg: Event,
) -> Option<Envelope> {
    match msg {
        Event::Hello { num_connections: _ } => {
            // Hold on sending messages till this is received.
            connection_state.connected();
            None
        }
        Event::Disconnect { reason: r } => {
            // When this is received, reconnect
            connection_state.reconnect();
            info!("Slack Websocket - Disconnect reason: {r:?}");
            None
        }
        Event::Envelope(envelope) => {
            // We should never get an envelope before an Hello, but if we ever do,
            // don't acknowledge and let slack resend it.
            if connection_state.should_send() {
                // As per slack "Maintain a response success rate of at least 5% of events per 60
                // minutes to prevent automatic disabling." So we automatically Acknowledge here
                // and handle overload on our end.
                //
                // TODO: interactivity needs to have data sent back along with the ack,so this
                // pattern won't work here but we aren't worried about those at the moment.
                send_acknowledge(connection_state, websocket, &envelope.id).await;
                Some(envelope)
            } else {
                warn!("Slack sent an envelope before hello, this shouldn't happen");
                None
            }
        }
    }
}

pub(crate) async fn process_control_message(
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    connection_state: &ConnectionState,
    msg: tungstenite::Message,
) -> AResult<Option<Envelope>> {
    // Parse incoming message
    connection_state.message_received();

    let raw_msg = process_websocket_message(websocket, connection_state, msg).await;

    if let Some(e) = raw_msg.as_deref().and_then(parse_event) {
        Ok(process_slack_control_message(websocket, connection_state, e).await)
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod test_event {
    use super::*;
    use serde_json::json;
    use test_log::test;

    #[test]
    fn test_hello() {
        let data = r#"{
            "type": "hello",
            "connection_info": {
                "app_id": "A1234"
            },
            "num_connections": 1,
            "debug_info": {
                "host": "applink-…."
            }
        }"#;
        assert_eq!(parse_event(data), Some(Event::Hello { num_connections: 1 }));
    }

    #[test]
    fn test_disconnect() {
        let data = r#"{
            "type" : "disconnect",
            "reason": "refresh_requested",
            "debug_info": {
                "host": "wss-111.slack.com"
            }
        }"#;
        assert_eq!(
            parse_event(data),
            Some(Event::Disconnect {
                reason: "refresh_requested".to_owned()
            })
        );
    }

    #[test]
    fn test_slash_command() {
        let data = r#"{
            "payload": {
                "channel_id": "C123ABC456",
                "user_id": "U123ABC456"
            },
            "envelope_id": "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545",
            "type": "slash_commands",
            "accepts_response_payload": true
        }"#;
        assert_eq!(
            parse_event(data),
            Some(Event::Envelope(Envelope {
                typ: "slash_commands".to_owned(),
                id: "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545".to_owned(),
                accepts_response_payload: true,
                payload: json!({
                    "channel_id": "C123ABC456",
                    "user_id": "U123ABC456",
                }),
            }))
        );
    }

    #[test]
    fn test_interactive() {
        let data = r#"{
            "payload": {
                "type": "block_actions",
                "trigger_id": "12321423423.333649436676.d8c1bb837935619ccad0f624c448ffb3"
            },
            "envelope_id": "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545",
            "type": "interactive",
            "accepts_response_payload": true
        }"#;
        assert_eq!(
            parse_event(data),
            Some(Event::Envelope(Envelope {
                typ: "interactive".to_owned(),
                id: "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545".to_owned(),
                accepts_response_payload: true,
                payload: json!({
                    "type": "block_actions",
                    "trigger_id": "12321423423.333649436676.d8c1bb837935619ccad0f624c448ffb3",
                }),
            }))
        );
    }

    #[test]
    fn test_events_api() {
        let data = r#"{
            "payload": {
                "type": "app_home_opened",
                "user": "U123ABC456"
            },
            "envelope_id": "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545",
            "type": "events_api",
            "accepts_response_payload": false
        }"#;
        assert_eq!(
            parse_event(data),
            Some(Event::Envelope(Envelope {
                typ: "events_api".to_owned(),
                id: "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545".to_owned(),
                accepts_response_payload: false,
                payload: json!({
                    "type": "app_home_opened",
                    "user": "U123ABC456"
                }),
            }))
        );
    }
}
