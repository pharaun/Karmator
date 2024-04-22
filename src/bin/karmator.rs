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
    let app_token = env::var("SLACK_APP_TOKEN").map_err(|_| "SLACK_APP_TOKEN env var must be set")?;
    let bot_token = env::var("SLACK_BOT_TOKEN").map_err(|_| "SLACK_BOT_TOKEN env var must be set")?;
    let backup = env::var("SQLITE_BACKUP_DIR");

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // System Cache manager
    let cache = cache::Cache::new(&app_token, &bot_token);

    // Shutdown Signal
    let (sql_shutdown_tx, signal) = signal::Signal::new();

    // Legacy bot api - If this is set, various bot features are disabled
    match env::var("SLACK_API_TOKEN") {
        Ok(legacy_api_token) => {
            use karmator_rust::bot::migration;

            println!("INFO [Legacy Mode]: because SLACK_API_TOKEN was set, entering legacy mode");

            let migration = migration::Migration::new(&legacy_api_token);

            // Invoke and print result for testing this api out
            let conv = migration.get_channels(
                10,
                vec![],
                None,
            ).await;

            println!("DEBUG:\n {:?}", conv);

            // Only join if:
            //  is_member: true, is_channel: true, is_private: true/false, is_archived: false
            // TODO:
            //  - Do the pagination process, and process it block by block.
            //  - filter out non-desired channels
            //  - Check (for crash robustness) if already joined or not (reconcilation somehow)
            //  - Join and confirm join
            //
            // Implementation:
            //  - 20 request a minute, set it to like 10 request a minute at say 200 channel a time
            //  - one request, get a list of channel
            //  - send list of channel to database to see which one already has been joined
            //      (or a memory cache)
            //  - get a list of channel to join,
            //  - 50 request a minute (for channel join, set it to like 20? join a minute
            //  - upon successful join, record to database the join for future run.
            //  - Print some sort of stats like when we hit 0 new joins after a while, turn off
            //      old bot and let this one run in background + serve new content
            //  - after old bot is shut off and anything in background that invites it, remove this

            return Ok(());
        },
        Err(_) => (),
    }

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
        cache.clone(),
        signal,
        |event, tx| {
            let sql_tx2 = sql_tx.clone();
            let cache2 = cache.clone();

            tokio::spawn(async move {
                // TODO: check result
                let _ = user_event::process_user_message(
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
