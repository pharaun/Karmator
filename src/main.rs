use std::env;

use log::{error, info};

use anyhow::anyhow;
use anyhow::Result as AResult;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod};
use rustls::pki_types::pem::PemObject as _;
use rustls::pki_types::CertificateDer;
use rustls::ClientConfig as RustlsClientConfig;
use tokio_postgres_rustls::MakeRustlsConnect;

use karmator::bot::user_event::process_user_message;

use kcore::default_event_loop;
use kcore::Signal;
use kcore::SlackClient;

// TODO:
// 5. Migrate from batch over to stored procedure for cleaning out votes run (ie repeated votes for
//    same item by the same person - default is max of 20 in one run) - You select the whole votes
//    table, iterate it row by row and compare current row with previous, and increment the run
//    count if its repeated, otherwise reset and make a new run-record. then scan through the run
//    record and prune anything more than say 20 items
#[tokio::main]
async fn main() -> AResult<()> {
    env_logger::init();

    let postgres_pem =
        env::var("POSTGRES_PEM").map_err(|_| anyhow!("POSTGRES_PEM env var must be set"))?;
    let postgres_url =
        env::var("POSTGRES_URL").map_err(|_| anyhow!("POSTGRES_URL env var must be set"))?;
    let app_token =
        env::var("SLACK_APP_TOKEN").map_err(|_| anyhow!("SLACK_APP_TOKEN env var must be set"))?;
    let bot_token =
        env::var("SLACK_BOT_TOKEN").map_err(|_| anyhow!("SLACK_BOT_TOKEN env var must be set"))?;

    //*******************
    // Signals bits
    //*******************
    // TODO: Don't need sql_shutdown cuz of deadpool now?
    let (_, signal) = Signal::new();
    {
        let mut signal = signal.clone();
        tokio::spawn(async move {
            // If this exits, then shutdown got invoked and this is no longer needed
            if let Err(e) = signal.shutdown_daemon().await {
                error!("Signal Error: {e:?}");
            }
            info!("Shutdown listener exited, shutdown is invoked");
        });
    }

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
    let pg_config = postgres_url.parse::<tokio_postgres::Config>()?;

    // Deadpool configuration
    let mgr_config = ManagerConfig {
        recycling_method: RecyclingMethod::Verified,
    };
    let mgr = Manager::from_config(pg_config, tls, mgr_config);
    let pool = Pool::builder(mgr).max_size(2).build().unwrap();

    //*******************
    // Core bot eventloop
    //*******************
    default_event_loop(
        SlackClient::new("https://slack.com/api", &app_token, &bot_token, 50),
        signal,
        |event, slack, tx| {
            let pool = pool.clone();
            tokio::spawn(async move {
                if let Err(e) = process_user_message(event, slack, tx, &pool).await {
                    error!("user_event::process_user_message error: {e:?}");
                }
            });
        },
    )
    .await?;

    Ok(())
}
