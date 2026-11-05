use std::sync::Arc;
use std::time::Duration;

use anyhow::Result as AResult;

use deadpool_postgres::Pool;
use log::{error, info, warn};
use quick_cache::sync::Cache;
use tokio::sync::broadcast;
use tokio::task::JoinSet;
use tokio_util::future::FutureExt as _;
use tokio_util::sync::CancellationToken;

use kcore::SlackClient;
use kcore::TungsteniteClient;
use kcore::slack_websocket_handler;

use crate::db::ScratchDb;
use crate::slack_api::FakeSlackApi;
use crate::ws_server::{FakeSlackServer, ServerConfig, ServerHandle};

const SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(Clone)]
pub struct HarnessHandle {
    pub db: Arc<ScratchDb>,
    pub server: ServerHandle,
    pub watcher: CancellationToken,
}

pub struct Harness {
    db: Arc<ScratchDb>,
    pool: Pool,
    server: ServerHandle,
    watcher: CancellationToken,
    tasks: JoinSet<()>,
}

impl Harness {
    pub async fn start(watcher: CancellationToken, config: ServerConfig) -> AResult<Self> {
        let mut tasks = JoinSet::new();

        let (server, handle) = FakeSlackServer::bind("127.0.0.1:0", config).await?;
        tasks.spawn(server.run(watcher.clone()));

        let db = Arc::new(ScratchDb::new().await?);
        let pool = db.pool()?;

        let slack = SlackClient::with_sender(
            FakeSlackApi::new(handle.url()),
            "http://localhost",
            "app-token",
            "bot-token",
            10,
        );
        let (event_tx, event_rx) = broadcast::channel(32);
        tasks.spawn(slack_websocket_handler(
            slack.clone(),
            TungsteniteClient,
            watcher.clone(),
            event_tx,
        ));
        tasks.spawn(karmator::user_message_handler(
            slack,
            watcher.clone(),
            Arc::new(Cache::new(1000)),
            pool.clone(),
            event_rx,
        ));
        tasks.spawn(karmator::cache_handler(watcher.clone(), pool.clone()));

        Ok(Self {
            db,
            pool,
            server: handle,
            watcher,
            tasks,
        })
    }

    pub fn handle(&self) -> HarnessHandle {
        HarnessHandle {
            db: Arc::clone(&self.db),
            server: self.server.clone(),
            watcher: self.watcher.clone(),
        }
    }

    pub fn pool(&self) -> &Pool {
        &self.pool
    }

    pub fn spawn(&mut self, task: impl Future<Output = ()> + Send + 'static) {
        // This is for spawning additional tasks (ie test driver, etc...)
        self.tasks.spawn(task);
    }

    pub async fn run(self) {
        // Drop our copy so the db Arc is unique once all the tasks are done
        let Self {
            db,
            pool,
            watcher,
            mut tasks,
            ..
        } = self;

        let mut panic = None;
        loop {
            // Once shutting down, don't let a stuck task hang us forever. The bot
            // gives itself ~20s to drain so leave room for that.
            let next = if watcher.is_cancelled() {
                match tasks.join_next().timeout(SHUTDOWN_TIMEOUT).await {
                    Ok(next) => next,
                    Err(_) => {
                        error!("Tasks didn't shut down in {SHUTDOWN_TIMEOUT:?}, aborting them");
                        tasks.shutdown().await;
                        break;
                    }
                }
            } else {
                tasks.join_next().await
            };
            let Some(res) = next else { break };

            watcher.cancel();
            match res {
                Ok(()) => info!("One of the task exited, notifying the rest"),
                Err(e) if e.is_panic() => {
                    error!("Task panicked: {e:?}");
                    panic.get_or_insert(e.into_panic());
                }
                Err(e) => error!("{e:?}"),
            }
        }

        pool.close();
        match Arc::try_unwrap(db) {
            Ok(db) => db.cleanup().await,
            Err(_) => {
                warn!("Scratch database still in use (live HarnessHandle?), skipping cleanup")
            }
        }

        if let Some(panic) = panic {
            std::panic::resume_unwind(panic);
        }
    }
}
