use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};
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
    // Legacy bot api migration
    //*******************
    match (env::var("SLACK_API_TOKEN"), env::var("SLACK_STATE")) {
        (Ok(_), Err(_)) => {
            println!("ERROR [Legacy Mode]: please set SLACK_STATE");
            return Ok(());
        },
        (Ok(legacy_api_token), Ok(state)) => {
            use karmator_rust::bot::migration;

            println!("INFO [Legacy Mode]: because SLACK_API_TOKEN was set, entering legacy mode");
            println!("INFO [Legacy Mode]: SLACK_STATE set to {:?}", state);

            let migration = migration::Migration::new(&bot_token, &legacy_api_token);

            // There is a few state to be in
            // list - fetch all channel and compare/update the sql db table
            // join - join the unjoined channel in the sql db table
            match state.as_str() {
                "list" => {
                    let mut tally: u64 = 0;
                    let mut next_cursor = None;

                    loop {
                        let conv = migration.get_channels(
                            100,
                            vec!["public_channel"],
                            next_cursor,
                            true,
                            true
                        ).await.expect("list-query");
                        next_cursor = conv.next_cursor;

                        let channels_len = conv.channels.len();
                        let mut upsert: u64 = 0;
                        for channel in conv.channels {
                            // Only join if:
                            //  is_member: true, is_channel: true, is_private: true/false, is_archived: false
                            if (channel.is_member == Some(true) || channel.is_member == None) &&
                                channel.is_channel == true &&
                                channel.is_archived == false {

                                let mut sql_tx2 = sql_tx.clone();
                                migration::upsert_channel(
                                    &mut sql_tx2,
                                    channel.id,
                                    channel.name
                                ).await;
                                upsert += 1;
                            }
                        }

                        tally += 1;
                        println!(
                            "INFO [Legacy Mode]: Tally: {:?}, upsert: {:?}, channel return: {:?}",
                            tally, upsert, channels_len
                        );

                        if next_cursor.is_none() {
                            break;
                        }

                        // So that we rate limit ourself
                        sleep(Duration::from_secs(10)).await;
                    }
                },
                "validate" => {
                    // Scan through the list of channels and validate their status
                    //  - set migration done if already member of a channel
                    //  - if is not member of a channel, reset it to todo
                    let mut sql_tx2 = sql_tx.clone();
                    let channel_ids = migration::get_all_channel_ids(
                        &mut sql_tx2
                    ).await.expect("channel_ids");

                    let channels_len = channel_ids.len();
                    let mut tally: u64 = 0;

                    for (channel_id, outcome) in channel_ids {
                        let info = migration.get_channel_info(&channel_id, false).await.expect("get_channel_info");

                        if info.is_archived {
                            println!("CHANNEL ARCHIVED - {:?}", &info.name);

                            let mut sql_tx3 = sql_tx.clone();
                            migration::delete_channel(
                                &mut sql_tx3,
                                channel_id.clone(),
                            ).await;
                        }

                        match info.is_member {
                            Some(false) => {
                                // Need to join so check if outcome is correct or not.
                                // if its failed, then print out the channel id
                                if outcome != 0 && outcome != 1 {
                                    println!("Channel: {:?} - failed: {:?}", channel_id, outcome);
                                    break;
                                } else {
                                    // Need to update outcome
                                    let mut sql_tx3 = sql_tx.clone();
                                    migration::update_channel(
                                        &mut sql_tx3,
                                        channel_id,
                                        0 // TO-DO
                                    ).await;
                                }
                            },
                            Some(true) => {
                                // Need to update outcome
                                let mut sql_tx3 = sql_tx.clone();
                                migration::update_channel(
                                    &mut sql_tx3,
                                    channel_id,
                                    1 // Done/Confirmed
                                ).await;
                            },
                            None => {
                                // Unexpected
                                println!("ERROR: unexpected None from channel_info");
                                break;
                            }
                        }

                        tally += 1;
                        println!(
                            "INFO [Legacy Mode]: Tally: {:?}/{:?}",
                            tally, channels_len
                        );

                        // So that we rate limit ourself
                        sleep(Duration::from_secs(2)).await;
                    }
                },
                "join" => {
                    let mut sql_tx2 = sql_tx.clone();
                    let channel_ids = migration::get_all_channel_ids(
                        &mut sql_tx2
                    ).await.expect("channel_ids");

                    let channels_len = channel_ids.len();
                    let mut skip_tally: u64 = 0;
                    let mut join_tally: u64 = 0;
                    let mut fail_tally: u64 = 0;

                    for (channel_id, outcome) in channel_ids {
                        if outcome != 0 {
                            skip_tally += 1;
                            // Skipping this one
                            continue;
                        }

                        // Let's actually join, wait a bit, then query if it actually joined
                        let info = migration.join_channel(&channel_id, false).await.expect("join_channel");

                        let new_outcome = match info.is_member {
                            Some(true) => {
                                join_tally += 1;
                                1 // migrated
                            },
                            _ => {
                                fail_tally += 1;
                                200 //Failure
                            },
                        };

                        // Need to update outcome
                        let mut sql_tx3 = sql_tx.clone();
                        migration::update_channel(
                            &mut sql_tx3,
                            channel_id,
                            new_outcome
                        ).await;

                        println!(
                            "INFO [Legacy Mode]: skip-tally: {:?} join-tally: {:?} fail-tally: {:?} total: {:?}",
                            skip_tally, join_tally, fail_tally, channels_len
                        );

                        // So that we rate limit ourself
                        sleep(Duration::from_secs(2)).await;
                    }
                },
                "special" => {
                    let mut tally: u64 = 0;
                    let mut next_cursor: Option<String> = None;

                    let instruction = format!(
                        "[*ACTION REQUIRED*] Hello! In order to make use of newer Slack features, we must unfortunately create another Karmator user. It is not possible to automatically join private channels, so we need your help. To continue to use Karmator features, invite <@{}> into this channel. This message will happen only once.",
                        env::var("SLACK_NEW_BOT_USER_ID").expect("SLACK_NEW_BOT_USER_ID")
                    );

                    loop {
                        let conv = migration.get_channels(
                            100,
                            vec!["private_channel"],
                            next_cursor,
                            true,
                            true
                        ).await.expect("list-query");
                        next_cursor = conv.next_cursor;

                        for channel in conv.channels {
                            println!("Sending Message: {:?}", channel.name);
                            //migration.post_message(
                            //    &channel.id,
                            //    &instruction,
                            //    true
                            //).await;
                            //sleep(Duration::from_secs(10)).await;
                        }

                        if next_cursor.is_none() {
                            break;
                        }

                        sleep(Duration::from_secs(10)).await;
                    }
                },
                x => {
                    println!("ERROR [Legacy Mode]: unknown state: {:?}", x);
                    return Ok(());
                }
            }

            return Ok(());
        },
        (_, _) => (),
    }


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
