use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use atomic_counter::RelaxedCounter;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use std::sync::Arc;
use std::env;
use std::path::Path;
use std::result::Result;
use std::thread;

use chrono::prelude::{Utc, DateTime};


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
    let sql_worker = thread::Builder::new().name("sqlite_worker".into()).spawn(move || {
        println!("Sql Worker - Launching");

        let res = database::process_queries(Path::new(&filename), sql_rx);
        println!("Sql Worker - {:?}", res);

        // TODO: Worker died, let's make sure everything else goes down as well.
        println!("Sql Worker - Exiting");
    })?;


    // TODO: figure out how to re-establish an connection upon connection loss or a 'good bye' from
    // slack
    //
    // I think the steps should be:
    // 1. loop start
    // 2. establish an WS connection
    // 3. select!(get/send)
    // 4. do some processing in 'get' for system control messages (another layer for message
    //    control)
    // 5. if system control get/connection lost, exit, causes the select! to end, and the loop
    //    again

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

    while !reconnect.load(Ordering::Relaxed) {
        // TODO: have each branch return a Ok or Err (or w/e) to indicate all ok or shit is bad
        // then if shit is bad go to recovery loop, and if its irrevociable, exit loop and
        // rejoin/terminate the sql worker if its not already terminated
        //
        // TODO: nested loop, one for the connection setup/teardown, then one for the inner select
        // loop

        tokio::select! {
            Some(Ok(ws_msg)) = ws_read.next() => {
                // TODO: we may have some ordering funnyness if we spawn a processor per
                // inbound message, ponder doing one processor per channel so its consistent
                // order per channel, if want to be intelligent about it, have a timeout, if a
                // channel has been quiet long enough, go ahead and shut down the channel processor.
                //
                // TODO: this could explode if the outstream or database get backed up it will just
                // spawn more and more inbound message, not good.
                let ue = event::process_control_message(
                    msg_id.clone(),
                    can_send.clone(),
                    reconnect.clone(),
                    ws_msg,
                    tx.clone(),
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

            // The send may not happen right away, the select! checks the (if x)
            // part before it checks the queue/etc
            Some(message) = rx.recv(), if can_send.load(Ordering::Relaxed) => {
                println!("Outbound - {:?}", message);

                // TODO: present some way to do plain vs fancy message, and if
                // fancy do a webapi post, otherwise dump into the WS
                //
                // TODO: look into tracking the sent message with a confirmation
                // that it was sent (via msg id) and if it fails, resend with
                // backoff
                //
                // TODO: find a way to handle the ping/pong cycle and monitor
                // TODO: check result
                let _ = ws_write.send(message).await;
            },
        }
    }

    // The sql_tx got moved into the inbound tokio async, so when that dies....
    // Its now not moved into a inbound tokio async, should be able to resume
    // Upon exit/termination we could in the web workers close the sql pipe which will then
    // terminate the sql worker loop
    let res = sql_worker.join();
    println!("Control - \t\t\tSql worker: {:?}", res);

    Ok(())
}
