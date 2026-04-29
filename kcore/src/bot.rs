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

use serde_json::json;

use log::{debug, error, info, warn};

use std::time::Duration;

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
    F1: Fn(serde_json::Value, SlackClient<S>, mpsc::Sender<event::Reply>),
{
    let conn_state = connection_state::ConnectionState::init();

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Message Tx/Rx from the message loop to the downstream handlers
    let (tx, mut rx) = mpsc::channel(32);

    // Main server loop, exit if the database dies
    while !signal.should_shutdown() {
        let (mut ws_write, mut ws_read) = ws_connect(&slack).await?.split();

        // Reset the connection to waiting for ack from Slack
        conn_state.pending().await;

        while !conn_state.should_reconnect() && !signal.should_shutdown() {
            tokio::select! {
                // Listens for shutdown signals from the system or the database
                () = signal.shutdown() => info!("Shutdown signal received"),

                // Process inbound messages
                ws_msg = ws_read.next() => process_message(&slack, &tx, &conn_state, &user_event_listener, ws_msg).await,

                // The send may not happen right away, the select! checks the (if x)
                // part before it checks the queue/etc
                // TODO: this would be better if instead of exiting the select loop we
                // gracefully drain the rx queue before exiting
                Some(message) = rx.recv(), if conn_state.should_send() => send_messages(&slack, &conn_state, &mut ws_write, message).await,

                // This is woken up peroidically to force a heartbeat check
                _ = heartbeat.tick() => heartbeat_tick(&tx, &conn_state).await,
            }
        }

        if !signal.should_shutdown() {
            // We should implement this as an actual exp backoff before trying to reconnect, its a flat
            // 20s wait between each reconnect, Also after 10 attempt shut down the bot
            let count = conn_state.fetch_reconnect_count_add();

            // Count to 10 but the atomic fetch_add returns the previous so on the 10th retry it'll be == 9
            if count <= 9 {
                info!("Slack Websocket - Reconnecting, try: {count:?}");
                time::sleep(Duration::from_secs(20)).await;
            } else {
                error!("Slack Websocket - Exceeded 10 retries, shutting down");
                signal.shutdown_now();
                break;
            }
        }
    }
    Ok(())
}

async fn ws_connect<S: SlackSender>(
    slack: &SlackClient<S>,
) -> AResult<WebSocketStream<MaybeTlsStream<TcpStream>>> {
    let ws_url = slack.socket_connect(false).await?;
    let (ws_stream, _) = connect_async(ws_url).await?;
    info!("Slack Websocket - connection established");
    Ok(ws_stream)
}

async fn process_message<S: SlackSender, F1>(
    slack: &SlackClient<S>,
    tx: &mpsc::Sender<crate::Reply>,
    connection_state: &connection_state::ConnectionState,
    user_event_listener: F1,
    ws_msg: Option<Result<tungstenite::Message, tungstenite::Error>>,
) where
    F1: Fn(serde_json::Value, SlackClient<S>, mpsc::Sender<event::Reply>),
{
    match ws_msg {
        Some(Ok(ws_msg)) => {
            match event::process_control_message(tx.clone(), &connection_state, ws_msg).await {
                // If there's an user event returned spawn off the user event processor
                Ok(Some(event)) => user_event_listener(event, slack.clone(), tx.clone()),
                Err(e) => error!("process_control_message error: {e:?}"),
                _ => (),
            }
        }
        Some(Err(e)) => {
            error!("Slack Websocket - Connection error: {e:?}");
            connection_state.reconnect();
        }

        // Stream is done/closed
        None => {
            warn!("Slack Websocket - Stream closed, reconnecting");
            connection_state.reconnect();
        }
    }
}

async fn send_messages<S: SlackSender>(
    slack: &SlackClient<S>,
    connection_state: &connection_state::ConnectionState,
    ws_write: &mut SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, tungstenite::Message>,
    message: crate::Reply,
) {
    // Convert this to a string json message to feed into the stream
    if let Err(e) = match message {
        event::Reply::Pong(x) => ws_write.send(tungstenite::Message::Pong(x)).await,
        event::Reply::Ping(x) => ws_write.send(tungstenite::Message::Ping(x)).await,
        event::Reply::Acknowledge(envelope_id) => {
            ws_write
                .send(tungstenite::Message::from(
                    json!({
                        "envelope_id": envelope_id
                    })
                    .to_string(),
                ))
                .await
        }
        event::Reply::Message(msg) => {
            if let Err(e) = slack.post_message(msg).await {
                // Don't propagage error to websocket cuz this is via web api
                error!("Slack Http - Post message error: {e:?}");
            }
            Ok(())
        }
    } {
        error!("Slack Websocket - Connection error: {e:?}");
        connection_state.reconnect();
    }
}

async fn heartbeat_tick(
    tx: &mpsc::Sender<crate::Reply>,
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
                if let Err(e) = event::send_slack_ping(&mut tx.clone(), &connection_state).await {
                    warn!("Slack Websocket Heartbeat - Error sending ping {e:?}");
                }
            }
        }
    }
}
