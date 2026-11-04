use std::env;
use std::sync::Arc;

use std::time::Duration;
use std::time::Instant;

use log::{debug, error, info};

use anyhow::Result as AResult;
use anyhow::anyhow;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod, Runtime};
use quick_cache::sync::Cache;
use rustls::ClientConfig as RustlsClientConfig;
use rustls::pki_types::CertificateDer;
use rustls::pki_types::pem::PemObject as _;
use tokio::sync::broadcast::channel;
use tokio::task::JoinSet;
use tokio_postgres_rustls::MakeRustlsConnect;
use tokio_util::sync::CancellationToken;

use karmator::cache_handler;
use karmator::user_message_handler;

use kcore::SlackClient;
use kcore::TungsteniteClient;
use kcore::slack_websocket_handler;
use kcore::wait_for_signal;

#[tokio::main]
async fn main() -> AResult<()> {
    env_logger::Builder::from_default_env()
        .format_timestamp(None)
        .init();

    info!(
        "Starting karmator {} ({})",
        karmator::bot::build::PKG_VERSION,
        karmator::bot::build::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown sha"),
    );

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

        let mut pg_config = postgres_url.parse::<tokio_postgres::Config>()?;

        // Detect dead connection quicker (~1m instead of ~15m)
        pg_config
            .connect_timeout(Duration::from_secs(5))
            .keepalives(true)
            .keepalives_idle(Duration::from_secs(30))
            .keepalives_interval(Duration::from_secs(10))
            .keepalives_retries(3)
            .tcp_user_timeout(Duration::from_secs(60));

        // Deadpool configuration
        let mgr_config = ManagerConfig {
            recycling_method: RecyclingMethod::Verified,
        };
        let mgr = Manager::from_config(pg_config, tls, mgr_config);

        Pool::builder(mgr)
            .max_size(8)
            .runtime(Runtime::Tokio1)
            .wait_timeout(Some(Duration::from_secs(10)))
            .create_timeout(Some(Duration::from_secs(10)))
            .recycle_timeout(Some(Duration::from_secs(5)))
            .build()?
    };

    // Tokio JoinSet
    let mut set = JoinSet::new();

    // Shutdown signal
    let watcher = CancellationToken::new();
    set.spawn(wait_for_signal(watcher.clone()));

    // Slack Websocket
    let slack = SlackClient::new("https://slack.com/api", &app_token, &bot_token, 50);
    let (sue, rue) = channel(32);
    set.spawn(slack_websocket_handler(
        slack.clone(),
        TungsteniteClient,
        watcher.clone(),
        sue,
    ));

    // User Event processing
    set.spawn(user_message_handler(
        slack.clone(),
        watcher.clone(),
        Arc::new(Cache::new(1000)),
        pool.clone(),
        rue,
    ));

    // Postgres cache cleaner
    set.spawn(cache_handler(watcher.clone(), pool.clone()));

    // Monitor the "daemon" tasks, and if any exits trigger shutdown.
    let mut shutdown_start = None;
    while let Some(res) = set.join_next().await {
        if shutdown_start.is_none() {
            info!("Shutting down");
            shutdown_start = Some(Instant::now());
        }
        watcher.cancel();
        match res {
            Ok(()) => debug!("One of the task exited, notifying the rest"),
            Err(e) => {
                error!("{e:?}");
                if e.is_panic() {
                    std::panic::resume_unwind(e.into_panic());
                }
            }
        }
    }
    if let Some(start) = shutdown_start {
        info!("Shutdown complete in {:?}", start.elapsed());
    }
    Ok(())
}
