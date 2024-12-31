use std::env;
use std::sync::Arc;

use log::{info, error};

use anyhow::anyhow;
use anyhow::Result as AResult;

use rustls::pki_types::pem::PemObject;
use rustls::pki_types::CertificateDer;
use rustls::ClientConfig as RustlsClientConfig;
use tokio_postgres_rustls::MakeRustlsConnect;
use tokio::sync::RwLock;

use kcore::slack;
use kcore::signal;
use kcore::bot;
use karmator::bot::user_event;

// TODO:
// 5. Migrate from batch over to stored procedure for cleaning out votes run (ie repeated votes for
//    same item by the same person - default is max of 20 in one run) - You select the whole votes
//    table, iterate it row by row and compare current row with previous, and increment the run
//    count if its repeated, otherwise reset and make a new run-record. then scan through the run
//    record and prune anything more than say 20 items
#[tokio::main]
async fn main() -> AResult<()> {
    env_logger::init();

    let postgres_pem = env::var("POSTGRES_PEM").map_err(|_| anyhow!("POSTGRES_PEM env var must be set"))?;
    let postgres_url = env::var("POSTGRES_URL").map_err(|_| anyhow!("POSTGRES_URL env var must be set"))?;
    let app_token = env::var("SLACK_APP_TOKEN").map_err(|_| anyhow!("SLACK_APP_TOKEN env var must be set"))?;
    let bot_token = env::var("SLACK_BOT_TOKEN").map_err(|_| anyhow!("SLACK_BOT_TOKEN env var must be set"))?;

    // System slack client manager
    let slack = slack::Client::new("https://slack.com/api", &app_token, &bot_token, 50);

    //*******************
    // Signals bits
    //*******************
    let (sql_shutdown_tx, signal) = signal::Signal::new();

    let mut signal2 = signal.clone();
    tokio::spawn(async move {
        // If this exits, then shutdown got invoked and this is no longer needed
        signal2.shutdown_daemon().await;
        info!("Shutdown listener exited, shutdown is invoked");
    });

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
    let client = Arc::new(RwLock::new(client));

    tokio::spawn(async move {
        // If the connection exits its for one of the 2 reasons:
        // 1. The client got dropped (meaning main loop exited)
        // 2. There was an error, and we are in a bad state, shutdown
        if let Err(e) = connection.await {error!("Database Error: {:?}", e);}
        if let Err(e) = sql_shutdown_tx.send(true) {error!("Shutdown Signal Error: {:?}", e);}
    });

    //*******************
    // Core bot eventloop
    //*******************
    bot::default_event_loop(
        slack.clone(),
        signal,
        |event, tx| {
            let client2 = client.clone();
            let slack2 = slack.clone();

            tokio::spawn(async move {
                if let Err(e) = user_event::process_user_message(
                    event,
                    tx,
                    client2,
                    slack2,
                ).await {
                    error!("user_event::process_user_message error: {:?}", e);
                };
            });
        },
    ).await?;

    Ok(())
}
