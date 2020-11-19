use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::collections::HashSet;
use std::sync::Arc;
use std::default::Default;
use std::env;
use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;

// SQlite worker thread
use std::thread;
use futures::executor::block_on_stream;

use rusqlite as rs;
use std::path::Path;
use tokio::sync::oneshot;

// User id -> display name cache
// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;


// Test data
mod schema_sample;

// Bot breakup
mod database;
mod message;
mod build_info;
mod cache;



#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // User id -> User Display name cache
    let user_cache: crate::cache::UserCache = Arc::new(DashMap::new());


    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    // Post a message
    let msg = slack::chat::PostMessageRequest {
        channel: "karmator-devs",
        text: "Pogger, i'm starting up!",
        as_user: Some(true),
        ..Default::default()
    };
    slack::chat::post_message(&client, &token, &msg).await;

    // Work to establish the WS connection
    let response = slack::rtm::connect(&client, &token).await.map_err(|e| format!("Control - {:?}", e))?;
    println!("Control - Got an ok reply");

    let ws_url = response.url.ok_or(format!("Control - \tNo Ws url"))?;
    println!("Control - \tGot a WS url: {:?}", ws_url);

    let (ws_stream, _) = tungstenite::connect_async(ws_url).await.map_err(|e| format!("Control - \t\t{:?}", e))?;
    println!("Control - \t\tWS connection established");

    // Setup the mpsc channel
    let (tx, mut rx) = mpsc::channel(32);

    // Split the stream
    let (mut ws_write, mut ws_read) = ws_stream.split();


    // Setup the mpsc channel for sqlite and spawn the sqlite worker thread
    // TODO: define a data format for asking the db to query something on your behalf, for now
    // pass in the raw query string?
    let (sql_tx, sql_rx) = mpsc::channel(32);

    let sql_worker = thread::spawn(move || {
        println!("Sql Worker - Launching");
        crate::database::process_queries(sql_rx);
        println!("Sql Worker - Exiting");
    });

    // Spawn the inbound ws stream processor
    let inbound = tokio::spawn(async move {
        loop {
            if let Some(Ok(ws_msg)) = ws_read.next().await {
                let msg_id = msg_id.clone();
                let tx2 = tx.clone();
                let sql_tx2 = sql_tx.clone();
                let user_cache = user_cache.clone();
                let token = token.clone();

                // TODO: not 100% for sure here but seems like inner handle is behind ARC so should
                // be safe to clone a reqwest client
                let client = client.clone();

                tokio::spawn(async move {
                    crate::message::process_inbound_message(
                        msg_id,
                        ws_msg,
                        tx2,
                        sql_tx2,
                        start_time,
                        user_cache,
                        &token,
                        client
                    ).await;
                });

            }
        }
    });

    // Spawn outbound ws processor
    // TODO: need to make sure to wait on sending till we recieve the hello event
    // could be a oneshot or some sort of flag which it waits till inbound has
    // gotten the hello
    let outbound = tokio::spawn(async move {
        loop {
            while let Some(message) = rx.recv().await {
                println!("Outbound - {:?}", message);

                // TODO: present some way to do plain vs fancy message, and if
                // fancy do a webapi post, otherwise dump into the WS
                //
                // TODO: look into tracking the sent message with a confirmation
                // that it was sent (via msg id) and if it fails, resend with
                // backoff
                //
                // TODO: find a way to handle the ping/pong cycle and monitor
                ws_write.send(message).await;
            }
        }
    });

    // Wait till either exits (error) then begin recovery
    match tokio::try_join!(inbound, outbound) {
        Ok((first, second)) => println!("Control - \t\t\tBoth exited fine"),
        Err(err) => println!("Control - \t\t\tSomething failed: {:?}", err),
    }

    // The sql_tx got moved into the inbound tokio async, so when that dies....
    let res = sql_worker.join();
    println!("Control - \t\t\tSql worker: {:?}", res);

    Ok(())
}
