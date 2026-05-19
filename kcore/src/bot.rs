use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite;
use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;

use futures_util::stream::SplitSink;
use futures_util::SinkExt as _;
use futures_util::StreamExt as _;

use tokio::sync::broadcast::Sender;
use tokio::net::TcpStream;
use tokio::time;
use tokio::time::timeout;
use tokio::time::MissedTickBehavior;
use tokio_util::sync::CancellationToken;

use serde_json::json;

use log::{debug, error, info, warn};

use std::time::Duration;

use anyhow::anyhow;
use anyhow::Result as AResult;

use crate::connection_state;
use crate::event;
use crate::SlackClient;
use crate::SlackSender;

pub async fn slack_websocket_handler<S: SlackSender>(
    slack: SlackClient<S>,
    watcher: CancellationToken,
    user_event: Sender<serde_json::Value>,
) {
    let conn_state = connection_state::ConnectionState::init();

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Main server loop, exit if the database dies
    while !watcher.is_cancelled() {
        match ws_connect(&slack, &watcher).await {
            Ok(websocket) => {
                let (mut ws_write, mut ws_read) = websocket.split();

                // Reset the connection to waiting for ack from Slack
                conn_state.pending().await;

                while !conn_state.should_reconnect() && !watcher.is_cancelled() {
                    tokio::select! {
                        // Listens for shutdown signals from the system or the database
                        () = watcher.cancelled() => info!("Shutdown signal received"),

                        // Process inbound messages
                        ws_msg = ws_read.next() => process_message(&mut ws_write, &conn_state, &user_event, ws_msg).await,

                        // This is woken up peroidically to force a heartbeat check
                        _ = heartbeat.tick() => heartbeat_tick(&mut ws_write, &conn_state).await,
                    }
                }
            }
            Err(e) => {
                conn_state.reconnect();
                error!("Slack Websocket - Connection failed: {e:?}");
            }
        }

        if !watcher.is_cancelled() {
            // Backoff timer is implemented in conn_state, its reset on entering reconnect state
            // - When it returns None we have ran out of attempts and we should give up now
            // - This is also exceeded in a crash loop cuz we will not have an alive-connection and
            // max backoff timer expires
            if let Some(durt) = conn_state.fetch_next_backoff() {
                info!("Slack Websocket - Reconnecting, sleeping for: {durt:?}");
                tokio::select! {
                    () = watcher.cancelled() => (),
                    () = time::sleep(durt) => (),
                }
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
    let ws_url = tokio::select! {
        () = watcher.cancelled() => Err(anyhow!("Shutdown triggered")),
        s = slack.socket_connect() => s,
    }?;
    let (ws_stream, _) = tokio::select! {
        () = watcher.cancelled() => Err(anyhow!("Shutdown triggered")),
        s = timeout(Duration::from_secs(5), connect_async(ws_url)) => s?.map_err(|e| anyhow!("Error: {e:?}")),
    }?;
    info!("Slack Websocket - connection established");
    Ok(ws_stream)
}

async fn process_message(
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    connection_state: &connection_state::ConnectionState,
    user_event: &Sender<serde_json::Value>,
    ws_msg: Option<Result<tungstenite::Message, tungstenite::Error>>,
) {
    match ws_msg {
        Some(Ok(ws_msg)) => {
            match event::process_control_message(ws_write, connection_state, ws_msg).await {
                Ok(Some(event)) => if let Err(e) = user_event.send(event) {
                    // A send fail doesn't mean it won't be picked up again.
                    error!("user_event.send error: {e:?}")
                },
                Err(e) => error!("process_control_message error: {e:?}"),
                _ => (),
            }
        }
        Some(Err(e)) => {
            error!("Slack Websocket - Process - Connection error: {e:?}");
            connection_state.reconnect();
        }

        // Stream is done/closed
        None => {
            warn!("Slack Websocket - Process - Stream closed, reconnecting");
            connection_state.reconnect();
        }
    }
}

async fn heartbeat_tick(
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    connection_state: &connection_state::ConnectionState,
) {
    // Check the delta, If either or both are None, that means
    // their timer are *ahead* of the 'now' which is fine, stop checking.
    if let (Some(lmd), Some(lpd)) = connection_state.timer_delta().await {
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
                // TODO: Generate a new value each time so we can track the ping-pong for
                // connection aliveness
                send_ping(connection_state, ws_write, tungstenite::Bytes::new()).await;
            }
        }
    }
}

// Acknowledge reception of event
pub(crate) async fn send_acknowledge(
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    envelope_id: String,
) {
    send_websocket(
        connection_state,
        ws_write,
        tungstenite::Message::from(
            json!({
                "envelope_id": envelope_id
            })
            .to_string(),
        ),
    )
    .await;
}

async fn send_ping(
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    content: tungstenite::Bytes,
) {
    send_websocket(
        connection_state,
        ws_write,
        tungstenite::Message::Ping(content),
    )
    .await;
    connection_state.ping_sent().await;
}

pub(crate) async fn send_pong(
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    content: tungstenite::Bytes,
) {
    send_websocket(
        connection_state,
        ws_write,
        tungstenite::Message::Pong(content),
    )
    .await;
}

async fn send_websocket(
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    message: tungstenite::Message,
) {
    if connection_state.should_send() {
        if let Err(e) = ws_write.send(message).await {
            error!("Slack Websocket - Connection error: {e:?}");
            connection_state.reconnect();
        }
    } else {
        error!("Bad state! Can't send a control message yet received slack message");
    }
}
