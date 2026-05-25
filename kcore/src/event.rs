use std::fmt::Debug;

use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::tungstenite::Utf8Bytes;

use futures_util::sink::Sink;

use serde::Deserialize;

use anyhow::Result as AResult;

use log::{debug, error, info, warn};

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
    // TODO: add this for interactive events
    // - accepts_response_payload: bool,
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

async fn process_websocket_message<W>(
    websocket: &mut W,
    connection_state: &ConnectionState,
    msg: Message,
) -> AResult<Option<Utf8Bytes>>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    match msg {
        Message::Text(x) => Ok(Some(x)),

        Message::Ping(x) => {
            send_pong::<W>(connection_state, websocket, x).await?;
            Ok(None)
        }

        Message::Pong(_) => {
            // Reply from our heartbeat ping
            Ok(None)
        }

        Message::Close(reason) => {
            info!("Slack Websocket - Close reason: {reason:?}");
            connection_state.reconnect();
            Ok(None)
        }

        Message::Binary(x) => {
            error!("Slack Websocket - Unsupported websocket type: {x:?}");
            Ok(None)
        }

        Message::Frame(x) => {
            error!("Slack Websocket - Unsupported websocket type: {x:?}");
            Ok(None)
        }
    }
}

async fn process_slack_control_message<W>(
    websocket: &mut W,
    connection_state: &ConnectionState,
    msg: Event,
) -> Option<Envelope>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
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
                if let Err(e) =
                    send_acknowledge::<W>(connection_state, websocket, &envelope.id).await
                {
                    warn!("send_acknowledge error: {e:?}");
                }
                // For now send the envelope to be queued always, consumer will need to
                // dedup via event_id
                Some(envelope)
            } else {
                warn!("Slack sent an envelope before hello, this shouldn't happen");
                None
            }
        }
    }
}

pub(crate) async fn process_control_message<W>(
    websocket: &mut W,
    connection_state: &ConnectionState,
    msg: Message,
) -> AResult<Option<Envelope>>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    connection_state.message_received();

    // Parse incoming message
    let raw_msg = process_websocket_message::<W>(websocket, connection_state, msg).await;

    match raw_msg.map(|m| m.as_deref().and_then(parse_event)) {
        Ok(Some(msg)) => {
            Ok(process_slack_control_message::<W>(websocket, connection_state, msg).await)
        }
        Ok(None) => Ok(None),
        Err(e) => {
            error!("process_websocket_message error - {e:?}");
            Ok(None)
        }
    }
}

#[cfg(test)]
mod test_process_websocket {
    use super::*;
    use serde_json::json;
    use tokio_tungstenite::tungstenite::Bytes;
    use tokio_tungstenite::tungstenite::protocol::frame::Frame;

    #[tokio::test(start_paused = true)]
    async fn test_websocket_message_text() {
        let data = json!({
            "envelope_id": "dbdd0ef3-1543-4f94-bfb4-133d0e6c1545",
            "type": "events_api",
            "payload": {
                "test": "test"
            }
        })
        .to_string();

        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let res = process_websocket_message(&mut ws_sink, &state, Message::from(data.clone()))
            .await
            .unwrap();

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        assert!(ws_sink.is_empty());
        assert_eq!(res, Some(data.into()));
    }

    #[tokio::test(start_paused = true)]
    async fn test_websocket_message_ping_connected() {
        let data = Bytes::from_static(b"hello");

        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let res = process_websocket_message(&mut ws_sink, &state, Message::Ping(data.clone()))
            .await
            .unwrap();

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        assert_eq!(ws_sink, vec![Message::Pong(data)]);
        assert_eq!(res, None);
    }

    #[tokio::test(start_paused = true)]
    async fn test_websocket_message_ping_pending() {
        let data = Bytes::from_static(b"hello");

        let state = ConnectionState::init();
        state.pending();

        let mut ws_sink = vec![];
        let res =
            process_websocket_message(&mut ws_sink, &state, Message::Ping(data.clone())).await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(!state.should_send());

        assert!(ws_sink.is_empty());
        assert!(res.is_err());
    }

    #[tokio::test(start_paused = true)]
    async fn test_websocket_message_closed() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let res = process_websocket_message(&mut ws_sink, &state, Message::Close(None))
            .await
            .unwrap();

        // Assert that we are in reconnect state now
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        assert!(ws_sink.is_empty());
        assert_eq!(res, None);
    }

    #[tokio::test(start_paused = true)]
    async fn test_websocket_message_ignored() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let messages = vec![
            Message::Pong(Bytes::new()),
            Message::Binary(Bytes::new()),
            Message::Frame(Frame::pong(Bytes::new())),
        ];
        let mut ws_sink = vec![];

        for msg in messages {
            let res = process_websocket_message(&mut ws_sink, &state, msg)
                .await
                .unwrap();
            assert_eq!(res, None);
        }

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        assert!(ws_sink.is_empty());
    }
}

#[cfg(test)]
mod test_process_control_message {
    use super::*;
    use serde_json::json;

    #[tokio::test(start_paused = true)]
    async fn test_event_hello() {
        let state = ConnectionState::init();
        state.pending();

        let mut ws_sink = vec![];
        let res = process_slack_control_message(
            &mut ws_sink,
            &state,
            Event::Hello {
                num_connections: 10,
            },
        )
        .await;

        // Assert that we are "connected now"
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        assert!(ws_sink.is_empty());
        assert_eq!(res, None);
    }

    #[tokio::test(start_paused = true)]
    async fn test_event_disconnect() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let res = process_slack_control_message(
            &mut ws_sink,
            &state,
            Event::Disconnect {
                reason: "asdf".to_owned(),
            },
        )
        .await;

        // Assert that we are "Disconnected now"
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        assert!(ws_sink.is_empty());
        assert_eq!(res, None);
    }

    #[tokio::test(start_paused = true)]
    async fn test_event_envelope() {
        let envelope = Envelope {
            id: "id".to_owned(),
            typ: "typ".to_owned(),
            payload: json!({"test": "asdf"}),
        };

        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let res =
            process_slack_control_message(&mut ws_sink, &state, Event::Envelope(envelope.clone()))
                .await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        assert_eq!(
            ws_sink,
            vec![Message::from(json!({"envelope_id": "id"}).to_string())]
        );
        assert_eq!(res, Some(envelope));
    }

    #[tokio::test(start_paused = true)]
    async fn test_event_envelope_send_gate() {
        let envelope = Envelope {
            id: "id".to_owned(),
            typ: "typ".to_owned(),
            payload: json!({"test": "asdf"}),
        };

        let state = ConnectionState::init();
        state.pending();

        let mut ws_sink = vec![];
        let res =
            process_slack_control_message(&mut ws_sink, &state, Event::Envelope(envelope.clone()))
                .await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(!state.should_send());

        assert!(ws_sink.is_empty());
        assert_eq!(res, None);
    }
}

#[cfg(test)]
mod test_smoke_top_level {
    use super::*;
    use std::time::Duration;

    #[tokio::test(start_paused = true)]
    async fn test_smoke_test() {
        let data = r#"{
            "type": "hello",
            "num_connections": 1
        }"#;

        let state = ConnectionState::init();
        state.pending();

        tokio::time::advance(Duration::from_mins(1)).await;

        let mut ws_sink = vec![];
        let res = process_control_message(&mut ws_sink, &state, Message::from(data))
            .await
            .unwrap();

        // Assert that we are "connected now"
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        // Assert that we set the message_received
        tokio::time::advance(Duration::from_mins(1)).await;
        let (lmr, _) = state.timer_delta();

        // If we did not, this would have shown as 2 mins due to prior advance
        assert_eq!(lmr, Some(Duration::from_mins(1)));

        assert!(ws_sink.is_empty());
        assert_eq!(res, None);
    }
}

#[cfg(test)]
mod test_event_parsing {
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
                payload: json!({
                    "type": "app_home_opened",
                    "user": "U123ABC456"
                }),
            }))
        );
    }
}
