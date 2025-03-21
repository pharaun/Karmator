use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;
use tokio::sync::RwLock;
use tokio::time;
use tokio::time::MissedTickBehavior;

use serde_json::json;

use log::{debug, error, info, warn};

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::Result as AResult;

use crate::event;
use crate::signal;
use crate::slack;

pub async fn default_event_loop<S, F1>(
    slack: slack::Client<S>,
    mut signal: signal::Signal,
    user_event_listener: F1,
) -> AResult<()>
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
    F1: Fn(serde_json::Value, slack::Client<S>, mpsc::Sender<event::Reply>),
{
    // Atomic boolean for ensuring send can't happen till the hello is received from slack
    let can_send = Arc::new(AtomicBool::new(false));

    // Atomic boolean for exiting and re-establishing the connection
    let reconnect = Arc::new(AtomicBool::new(false));
    let reconnect_count = Arc::new(RelaxedCounter::new(0));

    // Monotonical clock for heartbeat/ping management
    let last_message_received = Arc::new(RwLock::new(Instant::now()));
    let last_ping_sent = Arc::new(RwLock::new(Instant::now()));

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(time::Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Message Tx/Rx from the message loop to the downstream handlers
    let (tx, mut rx) = mpsc::channel(32);

    // Main server loop, exit if the database dies
    while !signal.should_shutdown() {
        // Work to establish the WS connection
        let ws_url = slack.socket_connect(false).await?;

        let (ws_stream, _) = connect_async(ws_url).await?;
        info!("Slack Websocket - connection established");

        // Split the stream
        let (mut ws_write, mut ws_read) = ws_stream.split();

        // Set the can-send to false till its set true by the reciever
        can_send.store(false, Ordering::Relaxed);

        // Set the reconnect to till the system control sets it to true
        reconnect.store(false, Ordering::Relaxed);

        while !reconnect.load(Ordering::Relaxed) && !signal.should_shutdown() {
            tokio::select! {
                // Listens for shutdown signals from the system or the database
                _ = signal.shutdown() => info!("Shutdown signal received"),

                ws_msg = ws_read.next() => {
                    match ws_msg {
                        Some(Ok(ws_msg)) => {
                            match event::process_control_message(
                                tx.clone(),
                                can_send.clone(),
                                reconnect.clone(),
                                reconnect_count.clone(),
                                last_message_received.clone(),
                                ws_msg,
                            ).await {
                                // If there's an user event returned spawn off the user event processor
                                Ok(Some(event)) => user_event_listener(event, slack.clone(), tx.clone()),
                                Err(e) => error!("process_control_message error: {:?}", e),
                                _ => (),
                            }
                        },
                        Some(Err(e)) => {
                            error!("Slack Websocket - Connection error: {:?}", e);
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },

                        // Stream is done/closed
                        None => {
                            warn!("Slack Websocket - Stream closed, reconnecting");
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },
                    }
                },

                // The send may not happen right away, the select! checks the (if x)
                // part before it checks the queue/etc
                // TODO: this would be better if instead of exiting the select loop we
                // gracefully drain the rx queue before exiting
                Some(message) = rx.recv(), if can_send.load(Ordering::Relaxed) => {
                    // Convert this to a string json message to feed into the stream
                    if let Err(e) = match message {
                        event::Reply::Pong(x) => ws_write.send(tungstenite::Message::Pong(x)).await,
                        event::Reply::Ping(x) => ws_write.send(tungstenite::Message::Ping(x)).await,
                        event::Reply::Acknowledge(envelope_id) => ws_write.send(tungstenite::Message::from(
                            json!({
                                "envelope_id": envelope_id
                            }).to_string())
                        ).await,
                        event::Reply::Message(msg) => {
                            if let Err(e) = slack.post_message(msg).await {
                                // Don't propagage error to websocket cuz this is via web api
                                error!("Slack Http - Post message error: {:?}", e);
                            }
                            Ok(())
                        },
                    } {
                        error!("Slack Websocket - Connection error: {:?}", e);
                        reconnect.store(true, Ordering::Relaxed);
                        can_send.store(false, Ordering::Relaxed);
                    }
                },

                // This is woken up peroidically to force a heartbeat check
                _ = heartbeat.tick() => {
                    let now = Instant::now();

                    let last_message_delta = {
                        let timer = last_message_received.read().await;
                        now.checked_duration_since(*timer)
                    };

                    let last_ping_delta = {
                        let timer = last_ping_sent.read().await;
                        now.checked_duration_since(*timer)
                    };

                    // Check the delta, If either or both are None, that means
                    // their timer are *ahead* of the 'now' which is fine, stop checking.
                    if let (Some(lmd), Some(lpd)) = (last_message_delta, last_ping_delta) {
                        // Check if more than 30s has past since the last slack message received
                        //  - [Yes] Check if more than 30s has past since the send of the last ping
                        //    - [Yes] Send Ping
                        //    - [No] Do nothing
                        //  - [No] Do nothing
                        //
                        // Check if more than 2m has past since the last slack message received
                        //  - [Yes] Reconnect
                        //  - [No] Do nothing
                        if lmd.as_secs() > 30 {
                            if lpd.as_secs() > 30 {
                                debug!("Slack Websocket Heartbeat - Last message: {:?}s, Last ping: {:?}s",
                                    lmd.as_secs(),
                                    lpd.as_secs(),
                                );
                                if let Err(e) = event::send_slack_ping(
                                    &mut tx.clone(),
                                    last_ping_sent.clone(),
                                ).await {
                                    warn!("Slack Websocket Heartbeat - Error sending ping {:?}", e);
                                }
                            }
                        } else if lmd.as_secs() > 120 {
                            info!("Slack Websocket Heartbeat - Last message: {:?}s, reconnecting", lmd.as_secs());
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        }
                    }
                },
            }
        }

        // We exited the inner loop, wait here and do an exp backoff before trying to reconnect
        // wait for 20s, then increment the reconnect_count by 1, and if it exceeds 10, set the
        // shutdown flag and exit
        if !signal.should_shutdown() {
            let count = reconnect_count.clone().inc();

            if count <= 10 {
                info!("Slack Websocket - Reconnecting, try: {:?}", count);
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
