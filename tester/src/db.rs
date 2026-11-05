// Scratch postgres database for easier testing
//
// Each run gets a fresh `karmator_test_<random>` database
// with the schema in `schema/` There is two options:
//
// 1. `POSTGRES_URL` - an existing server (needs CREATEDB user)
// 2. testcontainers (docker) - default is postgres:16 for `KARMATOR_PG_IMAGE`
//
// To persist database on exit. Set `KARMATOR_KEEP_DB=1` also for testcontainers
// also need `TESTCONTAINERS_RYUK_DISABLED=true`
use std::env;

use anyhow::Result as AResult;
use anyhow::{Context as _, anyhow};

use log::{info, warn};

use random_string::charsets;
use random_string::generate;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod};
use tokio_postgres::{Client, Config, NoTls};

use testcontainers_modules::postgres::Postgres;
use testcontainers_modules::testcontainers::{
    ContainerAsync, ImageExt as _, runners::AsyncRunner as _,
};

// Add new schema files here as needed, this is applied in listed order.
#[rustfmt::skip]
const SCHEMA: &[(&str, &str)] = &[
    ("v1.sql", include_str!("../../schema/v1.sql")),
    ("v1-triggers.sql", include_str!("../../schema/v1-triggers.sql")),
    ("v1-event-cache.sql", include_str!("../../schema/v1-event-cache.sql")),
];

// All of the tables + function created by SCHEMA.
const DROP_SCHEMA: &str = "
    DROP TABLE IF EXISTS
        votes, reacji_votes, reacji_message, nick_metadata, chan_metadata,
        karma_received_count, karma_given_count, processed_events
    CASCADE;
    DROP FUNCTION IF EXISTS
        insert_karma_count, delete_karma_count,
        insert_reacji_karma_count, delete_reacji_karma_count
    CASCADE;
";

const TRUNCATE: &str = "
    TRUNCATE
        votes, reacji_votes, reacji_message, nick_metadata, chan_metadata,
        karma_received_count, karma_given_count, processed_events;
";

enum Server {
    External,
    Container(Box<ContainerAsync<Postgres>>),
}

#[rustfmt::skip]
pub struct ScratchDb {
    pub config: Config, // Connection config for bot pool
    pub url: String, // For human use of psql
    admin_config: Config,
    db_name: String,
    keep: bool,
    server: Server,
}

impl ScratchDb {
    pub async fn new() -> AResult<Self> {
        let keep = env::var("KARMATOR_KEEP_DB").is_ok_and(|v| !v.is_empty() && v != "0");

        let (admin_config, server) = if let Ok(url) = env::var("POSTGRES_URL") {
            info!("Using postgres server from POSTGRES_URL");
            (
                url.parse::<Config>().context("Invalid POSTGRES_URL")?,
                Server::External,
            )
        } else {
            let image = env::var("KARMATOR_PG_IMAGE").unwrap_or_else(|_| "postgres:16".to_owned());
            let (name, tag) = image.rsplit_once(':').unwrap_or((&image, "latest"));
            info!("Starting postgres container: {name}:{tag}");

            let container = Postgres::default()
                .with_name(name)
                .with_tag(tag)
                .start()
                .await
                .map_err(|e| {
                    anyhow!(
                        "Failed to start a postgres container ({e}).\n\
                        Check that docker works, or set POSTGRES_URL"
                    )
                })?;

            let mut config = Config::new();
            config
                .host(container.get_host().await?.to_string())
                .port(container.get_host_port_ipv4(5432).await?)
                .user("postgres")
                .password("postgres")
                .dbname("postgres");
            (config, Server::Container(Box::new(container)))
        };

        // Create the scratch database
        let db_name = format!("karmator_test_{}", generate(8, charsets::ALPHA_LOWER));
        let admin = connect(&admin_config).await?;
        admin
            .batch_execute(&format!("CREATE DATABASE {db_name}"))
            .await
            .with_context(|| {
                format!("Failed to CREATE DATABASE {db_name} (user needs CREATEDB permission)")
            })?;

        let mut config = admin_config.clone();
        config.dbname(&db_name);

        let url = to_url(&config);
        let db = Self {
            config,
            url,
            admin_config,
            db_name,
            keep,
            server,
        };

        db.load_schema().await?;
        info!("Scratch database ready: {}", db.url);
        Ok(db)
    }

    async fn load_schema(&self) -> AResult<()> {
        let client = connect(&self.config).await?;
        for (name, sql) in SCHEMA {
            client
                .batch_execute(sql)
                .await
                .with_context(|| format!("Failed to apply schema/{name}"))?;
        }
        Ok(())
    }

    pub fn pool(&self) -> AResult<Pool> {
        let mgr = Manager::from_config(
            self.config.clone(),
            NoTls,
            ManagerConfig {
                recycling_method: RecyclingMethod::Verified,
            },
        );
        Ok(Pool::builder(mgr).max_size(8).build()?)
    }

    pub async fn truncate(&self) -> AResult<()> {
        connect(&self.config).await?.batch_execute(TRUNCATE).await?;
        Ok(())
    }

    pub async fn reset(&self) -> AResult<()> {
        connect(&self.config)
            .await?
            .batch_execute(DROP_SCHEMA)
            .await?;
        self.load_schema().await
    }

    pub async fn cleanup(self) {
        if self.keep {
            info!(
                "KARMATOR_KEEP_DB set, keeping scratch database: {}",
                self.url
            );
            // For the testcontainer mode: dropping the database handle will auto-remove the container+db
            if let Server::Container(container) = self.server {
                std::mem::forget(container);

                if env::var("TESTCONTAINERS_RYUK_DISABLED").is_ok_and(|v| v == "true") {
                    warn!("Container left running, remove it with docker when done");
                } else {
                    warn!(
                        "Container will still be removed on exit, \
                        also set TESTCONTAINERS_RYUK_DISABLED=true to keep it"
                    );
                }
            }
            return;
        }

        if let Server::External = self.server {
            // WITH (FORCE) drops any stray connections still open
            let res: AResult<()> = async {
                connect(&self.admin_config)
                    .await?
                    .batch_execute(&format!(
                        "DROP DATABASE IF EXISTS {} WITH (FORCE)",
                        self.db_name
                    ))
                    .await?;
                Ok(())
            }
            .await;
            match res {
                Ok(()) => info!("Dropped scratch database {}", self.db_name),
                Err(e) => warn!("Failed to drop scratch database {}: {e:?}", self.db_name),
            }
        }
    }
}

async fn connect(config: &Config) -> AResult<Client> {
    let (client, conn) = config
        .connect(NoTls)
        .await
        .context("Failed to connect to postgres")?;
    tokio::spawn(async move {
        if let Err(e) = conn.await {
            warn!("Scratch db connection error: {e:?}");
        }
    });
    Ok(client)
}

fn to_url(config: &Config) -> String {
    let host = match config.get_hosts().first() {
        Some(tokio_postgres::config::Host::Tcp(h)) => h.clone(),
        #[cfg(unix)]
        Some(tokio_postgres::config::Host::Unix(p)) => p.display().to_string(),
        None => "localhost".to_owned(),
    };
    let port = config.get_ports().first().copied().unwrap_or(5432);
    let user = config.get_user().unwrap_or("postgres");
    let pass = config
        .get_password()
        .map(|p| format!(":{}", String::from_utf8_lossy(p)))
        .unwrap_or_default();
    let db = config.get_dbname().unwrap_or("postgres");
    format!("postgres://{user}{pass}@{host}:{port}/{db}")
}
