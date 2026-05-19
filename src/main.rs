use std::env;

use log::{error, info};

use anyhow::anyhow;
use anyhow::Result as AResult;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod};
use rustls::pki_types::pem::PemObject as _;
use rustls::pki_types::CertificateDer;
use rustls::ClientConfig as RustlsClientConfig;
use tokio::sync::broadcast::channel;
use tokio::task::JoinSet;
use tokio_postgres_rustls::MakeRustlsConnect;
use tokio_util::sync::CancellationToken;

use karmator::bot::user_event::user_message_handler;

use kcore::slack_websocket_handler;
use kcore::wait_for_signal;
use kcore::SlackClient;

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

    // Postgres TLS + Pool
    let pool = {
        // TODO: Improve this (find alternative for native-tls)
        let certs: Vec<_> = CertificateDer::pem_file_iter(postgres_pem)?
            .map(|x| x.expect("Cert unwrap failed"))
            .collect();
        let mut root_store = rustls::RootCertStore::empty();
        root_store.add_parsable_certificates(certs);
        let tls_config = RustlsClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();
        let tls = MakeRustlsConnect::new(tls_config);

        let pg_config = postgres_url.parse::<tokio_postgres::Config>()?;

        // Deadpool configuration
        let mgr_config = ManagerConfig {
            recycling_method: RecyclingMethod::Verified,
        };
        let mgr = Manager::from_config(pg_config, tls, mgr_config);

        Pool::builder(mgr).max_size(8).build()?
    };

    // Tokio JoinSet
    let mut set = JoinSet::new();

    // Shutdown signal
    let watcher = CancellationToken::new();
    set.spawn(wait_for_signal(watcher.clone()));

    // Slack Websocket
    let slack = SlackClient::new("https://slack.com/api", &app_token, &bot_token, 50);
    let (sue, rue) = channel(32);
    set.spawn(slack_websocket_handler(slack.clone(), watcher.clone(), sue));

    // User Event processing
    set.spawn(user_message_handler(
        slack.clone(),
        watcher.clone(),
        pool.clone(),
        rue,
    ));

    // Monitor the "daemon" tasks, and if any exits trigger shutdown.
    while let Some(res) = set.join_next().await {
        watcher.cancel();
        match res {
            Ok(()) => info!("One of the task exited, notifying the rest"),
            Err(e) => {
                error!("{e:?}");
                if e.is_panic() {
                    std::panic::resume_unwind(e.into_panic());
                }
            }
        }
    }
    Ok(())
}
