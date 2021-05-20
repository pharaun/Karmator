use slack_api as slack;
use slack_api::requests::SlackWebRequestSender;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};
use futures_timer::Delay;

use tokio::sync::mpsc;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use std::result::Result;
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Duration;
use std::time::Instant;

use crate::core::event;
use crate::core::signal;


//********************
// Core Bot event loop
//********************
pub async fn default_event_loop<R, F>(
    token: &str,
    client: R,
    mut signal: signal::Signal,
    user_event_listener: F
) -> Result<(), Box<dyn std::error::Error>>
where
    F: Fn(
        event::UserEvent,
        event::MsgId,
        mpsc::Sender<tungstenite::tungstenite::Message>
    ) -> (),
    R: SlackWebRequestSender + Clone
{
    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    // Atomic boolean for ensuring send can't happen till the hello is recieved from slack
    let can_send = Arc::new(AtomicBool::new(false));

    // Atomic boolean for exiting and re-establishing the connection
    let reconnect = Arc::new(AtomicBool::new(false));
    let reconnect_count = Arc::new(RelaxedCounter::new(0));

    // Monotonical clock for heartbeat/ping management
    let last_message_recieved = Arc::new(RwLock::new(Instant::now()));
    let last_ping_sent = Arc::new(RwLock::new(Instant::now()));

    // Message Tx/Rx from the message loop to the downstream handlers
    let (tx, mut rx) = mpsc::channel(32);

    // Main server loop, exit if the database dies
    while !signal.should_shutdown() {
        // Work to establish the WS connection
        let response = slack::rtm::connect(&client, &token).await.map_err(|e| format!("control - {:?}", e))?;
        let ws_url = response.url.ok_or(format!("Control - \tNo Ws url"))?;
        println!("SYSTEM [Slack RTM]: Websocket Url: {:?}", ws_url);

        let (ws_stream, _) = tungstenite::connect_async(ws_url).await.map_err(|e| format!("Control - \t\t{:?}", e))?;
        println!("SYSTEM [Slack RTM]: Websocket connection established");

        // Split the stream
        let (mut ws_write, mut ws_read) = ws_stream.split();

        // Set the can-send to false till its set true by the reciever
        can_send.store(false, Ordering::Relaxed);

        // Set the reconnect to till the system control sets it to true
        reconnect.store(false, Ordering::Relaxed);

        while !reconnect.load(Ordering::Relaxed) && !signal.should_shutdown() {
            tokio::select! {
                // Listens for shutdown signals from the system or the database
                _ = signal.shutdown() => (),

                ws_msg = ws_read.next() => {
                    match ws_msg {
                        Some(Ok(ws_msg)) => {
                            // TODO: we may have some ordering funnyness if we spawn a processor per
                            // inbound message, ponder doing one processor per channel so its consistent
                            // order per channel, if want to be intelligent about it, have a timeout, if a
                            // channel has been quiet long enough, go ahead and shut down the channel processor.
                            //
                            // TODO: this could explode if the outstream or database get backed up it will just
                            // spawn more and more inbound message, not good.
                            let ue = event::process_control_message(
                                tx.clone(),
                                can_send.clone(),
                                reconnect.clone(),
                                reconnect_count.clone(),
                                last_message_recieved.clone(),
                                ws_msg,
                            ).await;

                            // If there's an user event returned spawn off the user event processor
                            if let Ok(Some(event)) = ue {
                                // TODO: should handle error here and listen
                                user_event_listener(
                                    event,
                                    msg_id.clone(),
                                    tx.clone()
                                );
                            }
                        },
                        Some(Err(e)) => {
                            eprintln!("SYSTEM [Slack RTM]: Connection error: {:?}", e);
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },

                        // Stream is done/closed
                        None => {
                            println!("SYSTEM [Slack RTM]: Stream closed, reconnecting");
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
                    // TODO: present some way to do plain vs fancy message, and if
                    // fancy do a webapi post, otherwise dump into the WS
                    //
                    // TODO: look into tracking the sent message with a confirmation
                    // that it was sent (via msg id) and if it fails, resend with
                    // backoff
                    match ws_write.send(message).await {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("SYSTEM [Slack RTM]: Connection error: {:?}", e);
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },
                    }
                },

                // This is woken up peroidically to force a heartbeat check
                _ = Delay::new(Duration::from_secs(1)) => {
                    let now = Instant::now();

                    let last_message_delta = {
                        let timer = last_message_recieved.read().unwrap();
                        now.checked_duration_since(*timer)
                    };

                    let last_ping_delta = {
                        let timer = last_ping_sent.read().unwrap();
                        now.checked_duration_since(*timer)
                    };

                    // Check the delta, If either or both are None, that means
                    // their timer are *ahead* of the 'now' which is fine, stop checking.
                    match (last_message_delta, last_ping_delta) {
                        (Some(lmd), Some(lpd)) => {
                            // Check if more than 30s has past since the last slack message recieved
                            //  - [Yes] Check if more than 30s has past since the send of the last ping
                            //    - [Yes] Send Ping
                            //    - [No] Do nothing
                            //  - [No] Do nothing
                            //
                            // Check if more than 2m has past since the last slack message recieved
                            //  - [Yes] Reconnect
                            //  - [No] Do nothing
                            if lmd.as_secs() > 30 {
                                if lpd.as_secs() > 30 {
                                    //println!("SYSTEM [Slack RTM]: Last message: {:?}s, Last ping: {:?}s",
                                    //    lmd.as_secs(),
                                    //    lpd.as_secs(),
                                    //);
                                    let _ = event::send_slack_ping(
                                        msg_id.clone(),
                                        &mut tx.clone(),
                                        last_ping_sent.clone(),
                                    ).await;
                                }
                            } else if lmd.as_secs() > 120 {
                                eprintln!("SYSTEM [Slack RTM]: Last message: {:?}s, reconnecting", lmd.as_secs());
                                reconnect.store(true, Ordering::Relaxed);
                                can_send.store(false, Ordering::Relaxed);
                            }
                        },
                        _ => (),
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
                println!("SYSTEM [Slack RTM]: Reconnecting, try: {:?}", count);
                Delay::new(Duration::from_secs(20)).await;
            } else {
                eprintln!("ERROR [Slack RTM]: Exceeded 10 retries, shutting down");
                signal.shutdown_now();
                break;
            }
        }
    }

    Ok(())
}
