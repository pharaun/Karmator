use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};
use futures_timer::Delay;

use tokio::sync::mpsc;
use tokio::signal;

use atomic_counter::RelaxedCounter;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use atomic_counter::AtomicCounter;

use std::sync::Arc;
use std::env;
use std::path::Path;
use std::result::Result;
use std::thread;
use std::sync::RwLock;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use std::time::Instant;


use karmator_rust::database;
use karmator_rust::event;
use karmator_rust::user_event;
use karmator_rust::cache;


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::var("SQLITE_FILE").map_err(|_| "SQLITE_FILE env var must be set")?;
    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    // Atomic boolean for ensuring send can't happen till the hello is recieved from slack
    let can_send = Arc::new(AtomicBool::new(false));

    // Atomic boolean for exiting and re-establishing the connection
    let reconnect = Arc::new(AtomicBool::new(false));
    let reconnect_count = Arc::new(RelaxedCounter::new(0));

    // Atomic boolean for shutting down the server if the db thread dies
    let shutdown = Arc::new(AtomicBool::new(false));

    // Monotonical clock for heartbeat/ping management
    let last_message_recieved = Arc::new(RwLock::new(Instant::now()));
    let last_ping_sent = Arc::new(RwLock::new(Instant::now()));

    // Launch a listener for ctrl-c
    let ctrl_c_shutdown = shutdown.clone();
    tokio::spawn(async move {
        println!("Ctrl-c listener installed");
        let _ = signal::ctrl_c().await;
        println!("Ctrl-c listener invoked - shutting down");
        ctrl_c_shutdown.store(true, Ordering::Relaxed);
    });

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // System Cache manager
    let cache = cache::Cache::new(&token, client.clone());

    // Message Tx/Rx from the message loop to the downstream handlers
    let (tx, mut rx) = mpsc::channel(32);

    // Sql request/reply channels for the downstream handlers to talk to the
    // sqlite worker thread
    let (sql_tx, sql_rx) = mpsc::channel(32);

    // Launch the sqlite worker thread
    let sql_shutdown = shutdown.clone();
    let sql_worker = thread::Builder::new().name("sqlite_worker".into()).spawn(move || {
        println!("Sql Worker - Launching");

        let res = database::process_queries(Path::new(&filename), sql_rx);
        println!("Sql Worker - {:?}", res);

        // Worker died, signal the shutdown signal
        sql_shutdown.store(true, Ordering::Relaxed);

        // TODO: Worker died, let's make sure everything else goes down as well.
        println!("Sql Worker - Exiting");
    })?;

    // Main server loop, exit if the database dies
    while !shutdown.load(Ordering::Relaxed) {
        // Work to establish the WS connection
        let response = slack::rtm::connect(&client, &token).await.map_err(|e| format!("Control - {:?}", e))?;
        println!("Control - Got an ok reply");

        let ws_url = response.url.ok_or(format!("Control - \tNo Ws url"))?;
        println!("Control - \tGot a WS url: {:?}", ws_url);

        let (ws_stream, _) = tungstenite::connect_async(ws_url).await.map_err(|e| format!("Control - \t\t{:?}", e))?;
        println!("Control - \t\tWS connection established");

        // Split the stream
        let (mut ws_write, mut ws_read) = ws_stream.split();

        // Set the can-send to false till its set true by the reciever
        can_send.store(false, Ordering::Relaxed);

        // Set the reconnect to till the system control sets it to true
        reconnect.store(false, Ordering::Relaxed);

        while !reconnect.load(Ordering::Relaxed) && !shutdown.load(Ordering::Relaxed) {
            tokio::select! {
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
                                let msg_id = msg_id.clone();
                                let tx2 = tx.clone();
                                let sql_tx2 = sql_tx.clone();
                                let cache = cache.clone();

                                tokio::spawn(async move {
                                    // TODO: check result
                                    let _ = user_event::process_user_message(
                                        msg_id,
                                        event,
                                        tx2,
                                        sql_tx2,
                                        start_time,
                                        cache,
                                    ).await;
                                });
                            }
                        },
                        Some(Err(e)) => {
                            println!("Connection error - {:?}", e);
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },

                        // Stream is done/closed
                        None => {
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
                    println!("Outbound - {:?}", message);

                    // TODO: present some way to do plain vs fancy message, and if
                    // fancy do a webapi post, otherwise dump into the WS
                    //
                    // TODO: look into tracking the sent message with a confirmation
                    // that it was sent (via msg id) and if it fails, resend with
                    // backoff
                    match ws_write.send(message).await {
                        Ok(_) => (),
                        Err(e) => {
                            println!("Connection error - {:?}", e);
                            reconnect.store(true, Ordering::Relaxed);
                            can_send.store(false, Ordering::Relaxed);
                        },
                    }
                },

                // This is woken up peroidically to force a check of the shutdown flag.
                _ = Delay::new(Duration::from_millis(250)) => {
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
                                    println!(
                                        "Input - last message: {:?}s, last ping: {:?}s, pinging",
                                        lmd.as_secs(),
                                        lpd.as_secs(),
                                    );
                                    let _ = event::send_slack_ping(
                                        msg_id.clone(),
                                        &mut tx.clone(),
                                        last_ping_sent.clone(),
                                    ).await;
                                }
                            } else if lmd.as_secs() > 120 {
                                println!("Input - last message: {:?}s, reconnecting", lmd.as_secs());
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
        if !shutdown.load(Ordering::Relaxed) {
            let count = reconnect_count.clone().inc();

            if count <= 10 {
                println!("Reconnecting... try: {:?}", count);
                Delay::new(Duration::from_secs(20)).await;
            } else {
                println!("Reconnecting... Failed, exceeded 10 retries, shutting down");
                shutdown.clone().store(true, Ordering::Relaxed);
                break;
            }
        }
    }

    // Force drop the sender (since all other sender clone should be dropped by now)
    drop(sql_tx);
    let res = sql_worker.join();
    println!("Control - \t\t\tSql worker: {:?}", res);

    Ok(())
}
