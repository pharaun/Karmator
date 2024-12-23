use std::env;
use std::result::Result;
use std::sync::Arc;

use chrono::prelude::{Utc, DateTime};

use rustls::pki_types::pem::PemObject;
use rustls::pki_types::CertificateDer;
use rustls::ClientConfig as RustlsClientConfig;
use tokio_postgres_rustls::MakeRustlsConnect;

use karmator_rust::core::cache;
use karmator_rust::core::signal;
use karmator_rust::core::bot;
use karmator_rust::bot::user_event;

// TODO:
// 1. update println + eprintln to use logging
// 2. update postgres pem to be optional (for talking to a local test database)
// 3. figure out how to intercept the slack api calls (maybe pass in optional slack url)
// 4. Migrate from batch over to stored procedure for cleaning out votes run (ie repeated votes for
//    same item by the same person - default is max of 20 in one run) - You select the whole votes
//    table, iterate it row by row and compare current row with previous, and increment the run
//    count if its repeated, otherwise reset and make a new run-record. then scan through the run
//    record and prune anything more than say 20 items

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let postgres_pem = env::var("POSTGRES_PEM").map_err(|_| "POSTGRES_PEM env var must be set")?;
    let postgres_url = env::var("POSTGRES_URL").map_err(|_| "POSTGRES_URL env var must be set")?;
    let app_token = env::var("SLACK_APP_TOKEN").map_err(|_| "SLACK_APP_TOKEN env var must be set")?;
    let bot_token = env::var("SLACK_BOT_TOKEN").map_err(|_| "SLACK_BOT_TOKEN env var must be set")?;

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // System Cache manager
    let cache = cache::Cache::new("https://slack.com/api", &app_token, &bot_token);

    // Shutdown Signal
    let (sql_shutdown_tx, signal) = signal::Signal::new();

    //*******************
    // Postgres bits
    //*******************
    let tls = {
        // TODO: Improve this (find alternative for native-tls)
        let certs: Vec<_> = CertificateDer::pem_file_iter(postgres_pem)
            .unwrap()
            .map(|x| x.unwrap())
            .collect();
        let mut root_store = rustls::RootCertStore::empty();
        root_store.add_parsable_certificates(certs);
        let tls_config = RustlsClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();
        MakeRustlsConnect::new(tls_config)
    };

    let (client, connection) = tokio_postgres::connect(&postgres_url, tls).await?;
    let client = Arc::new(client);

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("ERROR [Sql Worker]: {:?}", e);

            // Worker died, signal the shutdown signal
            let _ = sql_shutdown_tx.send(true);
        }
    });

    //*******************
    // Core bot eventloop
    //*******************
    bot::default_event_loop(
        cache.clone(),
        signal,
        |event, tx| {
            let client2 = client.clone();
            let cache2 = cache.clone();

            tokio::spawn(async move {
                // TODO: check result
                let _ = user_event::process_user_message(
                    event,
                    tx,
                    client2,
                    start_time,
                    cache2,
                ).await;
            });
        },
    ).await?;

    Ok(())
}
