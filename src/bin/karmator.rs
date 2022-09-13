use slack_api as slack;

use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

use std::env;
use std::path::Path;
use std::result::Result;
use std::thread;

use chrono::prelude::{Utc, DateTime};

use karmator_rust::core::database;
use karmator_rust::core::cache;
use karmator_rust::core::signal;
use karmator_rust::core::bot;
use karmator_rust::bot::user_event;


// TODO:
// Fix voting for - @luisp++ case it should -> luisp++ for the database
// Fix command parsing for <@user> -> user for commands
// FIX self-voting on reacji since we now also upvote the owner of the message itself along with
//      the message

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = env::var("SQLITE_FILE").map_err(|_| "SQLITE_FILE env var must be set")?;
    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let backup = env::var("SQLITE_BACKUP_DIR");
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // System Cache manager
    let cache = cache::Cache::new(&token, client.clone());

    // Shutdown Signal
    let (sql_shutdown_tx, signal) = signal::Signal::new();


    //*******************
    // Sql bits
    //*******************

    // Sql request/reply channels for the downstream handlers to talk to the
    // sqlite worker thread
    let (sql_tx, sql_rx) = mpsc::channel(32);

    // Launch the sqlite worker thread
    let sql_worker = thread::Builder::new().name("sqlite_worker".into()).spawn(move || {
        println!("INFO [Sql Worker]: Launching");

        let res = database::process_queries(
            Path::new(&filename),
            ReceiverStream::new(sql_rx),
        );
        eprintln!("ERROR [Sql Worker]: {:?}", res);

        // Worker died, signal the shutdown signal
        let _ = sql_shutdown_tx.send(true);

        println!("INFO [Sql Worker]: Exiting");
    })?;


    //*******************
    // Setting up backup
    //*******************
    let backup_callback: Box<dyn Fn()> = match backup {
        Ok(b) => {
            // Install an backup job for backing up the database
            let sql_tx3 = sql_tx.clone();
            Box::new(move || {
                let sql_tx4 = sql_tx3.clone();
                let path = b.clone();
                tokio::spawn(async move {
                    let _ = database::backup(
                        path.clone(),
                        sql_tx4
                    ).await;
                });
            })
        },
        Err(_) => {
            println!("WARNING [Backup]: SQLITE_BACKUP_DIR env var must be set");
            Box::new(|| {})
        },
    };


    //*******************
    // Core bot eventloop
    //*******************
    bot::default_event_loop(
        &token,
        client.clone(),
        signal,
        |event, msg_id, tx| {
            let sql_tx2 = sql_tx.clone();
            let cache2 = cache.clone();

            tokio::spawn(async move {
                // TODO: check result
                let _ = user_event::process_user_message(
                    msg_id,
                    event,
                    tx,
                    sql_tx2,
                    start_time,
                    cache2,
                ).await;
            });
        },
        backup_callback,
    ).await?;


    //*******************
    // Sql bits
    //*******************

    // Force drop the sender (since all other sender clone should be dropped by now)
    drop(sql_tx);
    let res = sql_worker.join();
    println!("SYSTEM [Sql Worker]: Thread Join: {:?}", res);

    Ok(())
}
