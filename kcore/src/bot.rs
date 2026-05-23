use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite;
use tungstenite::protocol::frame::CloseFrame;
use tungstenite::protocol::frame::coding::CloseCode;

use futures_util::SinkExt as _;
use futures_util::StreamExt as _;

use tokio::net::TcpStream;
use tokio::sync::broadcast::Sender;
use tokio::time;
use tokio::time::MissedTickBehavior;
use tokio_util::future::FutureExt as _;
use tokio_util::sync::CancellationToken;

use serde_json::json;

use log::{debug, error, info, warn};

use tokio::time::Duration;

use anyhow::Result as AResult;
use anyhow::anyhow;

use crate::SlackClient;
use crate::SlackSender;
use crate::connection_state;
use crate::event;

pub async fn slack_websocket_handler<S: SlackSender>(
    slack: SlackClient<S>,
    watcher: CancellationToken,
    user_event: Sender<event::Envelope>,
) {
    let conn_state = connection_state::ConnectionState::init();

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Main server loop, exit if the database dies
    while !watcher.is_cancelled() {
        match ws_connect(&slack, &watcher).await {
            Err(e) => error!("Slack Websocket - Connection failed: {e:?}"),
            Ok(mut websocket) => {
                // Reset the connection to waiting for ack from Slack
                conn_state.pending();

                while !conn_state.should_reconnect() && !watcher.is_cancelled() {
                    tokio::select! {
                        // Listens for shutdown signals from the system or the database
                        () = watcher.cancelled() => info!("Shutdown signal received"),

                        // Process inbound messages
                        ws_msg = websocket.next() => process_message(&mut websocket, &conn_state, &user_event, ws_msg).await,

                        // This is woken up peroidically to force a heartbeat check
                        _ = heartbeat.tick() => heartbeat_tick(&mut websocket, &conn_state).await,
                    }
                }

                // We are exiting or reconnecting, close the tungstein stream
                // TODO: skip sending close if the underlaying stream is in a bad state
                //  - check the status/error from attempting to transmit
                if let Err(e) = websocket
                    .close(Some(CloseFrame {
                        code: CloseCode::Normal,
                        reason: "Reconnecting".into(),
                    }))
                    .timeout(Duration::from_secs(5))
                    .await
                {
                    warn!("Failed to close the websocket {e:?}");
                }
            }
        }

        if !watcher.is_cancelled() {
            // Backoff timer is implemented in conn_state, its reset on entering reconnect state
            // - When it returns None we have ran out of attempts and we should give up now
            // - This is also exceeded in a crash loop cuz we will not have an alive-connection and
            // max backoff timer expires
            if let Some(durt) = conn_state.fetch_next_backoff() {
                info!("Slack Websocket - Reconnecting, sleeping for: {durt:?}");
                time::sleep(durt).with_cancellation_token(&watcher).await;
            } else {
                error!("Slack Websocket - Exceeded with_max_elapsed_time, shutting down");
                watcher.cancel();
                break;
            }
        }
    }
    info!("Slack Websocket - Exiting");
}

async fn ws_connect<S: SlackSender>(
    slack: &SlackClient<S>,
    watcher: &CancellationToken,
) -> AResult<WebSocketStream<MaybeTlsStream<TcpStream>>> {
    let ws_url = slack
        .socket_connect()
        .with_cancellation_token(watcher)
        .await
        .ok_or(anyhow!("Shutdown triggered"))??;
    let (ws_stream, _) = connect_async(ws_url)
        .timeout(Duration::from_secs(5))
        .with_cancellation_token(watcher)
        .await
        .ok_or(anyhow!("Shutdown triggered"))???;
    info!("Slack Websocket - connection established");
    Ok(ws_stream)
}

async fn process_message(
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    connection_state: &connection_state::ConnectionState,
    user_event: &Sender<event::Envelope>,
    ws_msg: Option<Result<tungstenite::Message, tungstenite::Error>>,
) {
    if connection_state.should_reconnect() {
        return;
    }
    match ws_msg {
        Some(Ok(ws_msg)) => {
            match event::process_control_message(websocket, connection_state, ws_msg).await {
                Ok(Some(event)) => {
                    if let Err(e) = user_event.send(event) {
                        // A send fail doesn't mean it won't be picked up again.
                        error!("user_event.send error: {e:?}");
                    }
                }
                Err(e) => error!("process_control_message error: {e:?}"),
                _ => (),
            }
        }
        Some(Err(e)) => {
            error!("Slack Websocket - Process - Connection error: {e:?}");
            connection_state.reconnect();
        }
        None => {
            // Stream is done/closed
            warn!("Slack Websocket - Process - Stream closed, reconnecting");
            connection_state.reconnect();
        }
    }
}

async fn heartbeat_tick(
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    connection_state: &connection_state::ConnectionState,
) {
    if connection_state.should_reconnect() {
        return;
    }
    // Check the delta, If either or both are None, that means
    // their timer are *ahead* of the 'now' which is fine, stop checking.
    if let (Some(lmd), Some(lpd)) = connection_state.timer_delta() {
        // Check if more than 30s has past since the last slack message received
        if lmd.as_secs() > 30 {
            // Check if more than 2m has past since the last slack message received
            if lmd.as_secs() > 120 {
                // Reconnect
                info!(
                    "Slack Websocket Heartbeat - Last message: {:?}s, reconnecting",
                    lmd.as_secs()
                );
                connection_state.reconnect();

            // Check if more than 30s has past since the send of the last ping
            } else if lpd.as_secs() > 30 {
                // Send Ping
                debug!(
                    "Slack Websocket Heartbeat - Last message: {:?}s, Last ping: {:?}s",
                    lmd.as_secs(),
                    lpd.as_secs(),
                );
                if connection_state.should_send() {
                    // TODO: Generate a new value each time so we can track the ping-pong for
                    // connection aliveness
                    if let Err(e) =
                        send_ping(connection_state, websocket, tungstenite::Bytes::new()).await
                    {
                        error!("send_ping error: {e:?}");
                    }
                }
            }
        }
    }
}

// Acknowledge reception of event
pub(crate) async fn send_acknowledge(
    connection_state: &connection_state::ConnectionState,
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    envelope_id: &str,
) -> AResult<()> {
    send_websocket(
        connection_state,
        websocket,
        tungstenite::Message::from(
            json!({
                "envelope_id": envelope_id
            })
            .to_string(),
        ),
    )
    .await
}

async fn send_ping(
    connection_state: &connection_state::ConnectionState,
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    content: tungstenite::Bytes,
) -> AResult<()> {
    match send_websocket(
        connection_state,
        websocket,
        tungstenite::Message::Ping(content),
    )
    .await
    {
        Ok(()) => {
            connection_state.ping_sent();
            Ok(())
        }
        e => e,
    }
}

pub(crate) async fn send_pong(
    connection_state: &connection_state::ConnectionState,
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    content: tungstenite::Bytes,
) -> AResult<()> {
    send_websocket(
        connection_state,
        websocket,
        tungstenite::Message::Pong(content),
    )
    .await
}

async fn send_websocket(
    connection_state: &connection_state::ConnectionState,
    websocket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    message: tungstenite::Message,
) -> AResult<()> {
    if connection_state.should_send() {
        if let Err(e) = websocket.send(message).await {
            connection_state.reconnect();
            Err(anyhow!("Slack Websocket - Connection error: {e:?}"))
        } else {
            Ok(())
        }
    } else {
        Err(anyhow!(
            "Bad state! Can't send a control message yet received slack message"
        ))
    }
}
