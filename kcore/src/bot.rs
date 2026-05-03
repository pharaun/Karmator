use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite;
use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;

use futures_util::stream::SplitSink;
use futures_util::SinkExt as _;
use futures_util::StreamExt as _;

use tokio::net::TcpStream;
use tokio::sync::mpsc;
use tokio::time;
use tokio::time::MissedTickBehavior;
use tokio::time::timeout;

use serde_json::json;

use log::{debug, error, info, warn};

use std::time::Duration;

use anyhow::anyhow;
use anyhow::Result as AResult;

use crate::connection_state;
use crate::event;
use crate::signal;
use crate::SlackClient;
use crate::SlackSender;

pub async fn default_event_loop<S: SlackSender, F1>(
    slack: SlackClient<S>,
    mut signal: signal::Signal,
    user_event_listener: F1,
) -> AResult<()>
where
    F1: Fn(serde_json::Value, SlackClient<S>),
{
    let conn_state = connection_state::ConnectionState::init();

    // Shutdown watcher
    let mut watcher = signal.get_shutdown_watcher();

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Websocket Tx/Rx from the message loop for managing the websocket
    let (wtx, mut wrx) = mpsc::channel(32);

    // Main server loop, exit if the database dies
    while !watcher.should_shutdown() {
        match ws_connect(&slack, &mut watcher).await {
            Ok(websocket) => {
                let (mut ws_write, mut ws_read) = websocket.split();

                // Reset the connection to waiting for ack from Slack
                conn_state.pending().await;

                while !conn_state.should_reconnect() && !watcher.should_shutdown() {
                    tokio::select! {
                        // Listens for shutdown signals from the system or the database
                        _ = signal.shutdown_signal() => info!("Shutdown signal received"),

                        // Process inbound messages
                        ws_msg = ws_read.next() => process_message(&slack, &wtx, &conn_state, &user_event_listener, ws_msg).await,

                        // Process outbound control messages
                        // NOTE: If should_send is false this will be disabled till heartbeat ticks
                        // (~1s delay) or any other arm of the select fires.
                        Some(message) = wrx.recv(), if conn_state.should_send() => send_websocket(&conn_state, &mut ws_write, message).await,

                        // This is woken up peroidically to force a heartbeat check
                        _ = heartbeat.tick() => heartbeat_tick(&wtx, &conn_state).await,
                    }
                }
            }
            Err(e) => {
                conn_state.special_reconnect();
                error!("Slack Websocket - Connection failed: {e:?}");
            }
        }

        if !watcher.should_shutdown() {
            // Backoff timer is implemented in conn_state, its reset on entering reconnect state
            // - When it returns None we have ran out of attempts and we should give up now
            match conn_state.fetch_next_backoff() {
                Some(durt) => {
                    info!("Slack Websocket - Reconnecting, sleeping for: {durt:?}");
                    tokio::select! {
                        _ = watcher.shutdown() => (),
                        _ = time::sleep(durt) => (),
                    }
                },
                None => {
                    error!("Slack Websocket - Exceeded ~1 minutes of retries, shutting down");
                    watcher.shutdown_now();
                    break;
                }
            }
        }
    }
    Ok(())
}

async fn ws_connect<S: SlackSender>(
    slack: &SlackClient<S>,
    watcher: &mut signal::Watcher,
) -> AResult<WebSocketStream<MaybeTlsStream<TcpStream>>> {
    let ws_url = slack.socket_connect(false).await?;
    let (ws_stream, _) = tokio::select! {
        _ = watcher.shutdown() => Err(anyhow!("Shutdown triggered")),
        s = timeout(Duration::from_millis(500), connect_async(ws_url)) => s?.map_err(|e| anyhow!("Error: {e:?}")),
    }?;
    info!("Slack Websocket - connection established");
    Ok(ws_stream)
}

async fn process_message<S: SlackSender, F1>(
    slack: &SlackClient<S>,
    wtx: &mpsc::Sender<event::WebsocketReply>,
    connection_state: &connection_state::ConnectionState,
    user_event_listener: F1,
    ws_msg: Option<Result<tungstenite::Message, tungstenite::Error>>,
) where
    F1: Fn(serde_json::Value, SlackClient<S>),
{
    match ws_msg {
        Some(Ok(ws_msg)) => {
            match event::process_control_message(wtx, connection_state, ws_msg).await {
                // If there's an user event returned spawn off the user event processor
                Ok(Some(event)) => user_event_listener(event, slack.clone()),
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

async fn send_websocket(
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    message: event::WebsocketReply,
) {
    // Convert this to a string json message to feed into the stream
    if let Err(e) = match message {
        event::WebsocketReply::Pong(x) => ws_write.send(tungstenite::Message::Pong(x)).await,
        event::WebsocketReply::Ping(x) => ws_write.send(tungstenite::Message::Ping(x)).await,
        event::WebsocketReply::Acknowledge(envelope_id) => {
            ws_write
                .send(tungstenite::Message::from(
                    json!({
                        "envelope_id": envelope_id
                    })
                    .to_string(),
                ))
                .await
        }
    } {
        error!("Slack Websocket - Connection error: {e:?}");
        connection_state.reconnect();
    }
}

async fn heartbeat_tick(
    wtx: &mpsc::Sender<event::WebsocketReply>,
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
                if let Err(e) = event::send_slack_ping(wtx, connection_state).await {
                    warn!("Slack Websocket Heartbeat - Error sending ping {e:?}");
                }
            }
        }
    }
}
