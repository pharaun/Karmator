use std::sync::Arc;
use tokio::time::Duration;

use log::{error, info};

use deadpool_postgres::Pool;
use quick_cache::sync::Cache;
use tokio::sync::Semaphore;
use tokio::sync::broadcast::Receiver;
use tokio::sync::broadcast::error::RecvError;
use tokio_util::future::FutureExt as _;
use tokio_util::sync::CancellationToken;
use tokio_util::task::TaskTracker;

use kcore::Envelope;
use kcore::SlackClient;
use kcore::SlackSender;

pub mod bot;
pub mod parser;
pub mod query;

pub use crate::bot::user_event::cache_handler;
use crate::bot::user_event::process_user_message;

pub async fn user_message_handler<S: SlackSender + 'static>(
    slack: SlackClient<S>,
    watcher: CancellationToken,
    event_seen_cache: Arc<Cache<String, bool>>,
    pool: Pool,
    mut user_event: Receiver<Envelope>,
) {
    // Match this to the database pool (8)
    let semaphore = Arc::new(Semaphore::new(8));

    // Permit timed shutdown to allow queue + stragglers to finish up
    let tracker = TaskTracker::new();

    // Hard abort for any open tasks
    let hard_abort = CancellationToken::new();

    // This call will block till the queue is empty + closed
    if inner_user_message_handler(
        &slack,
        &event_seen_cache,
        &pool,
        &mut user_event,
        &semaphore,
        &tracker,
        &hard_abort,
    )
    .with_cancellation_token(&watcher)
    .await
    .is_none()
    {
        info!("Cancellation triggered, draining queue now");
        if inner_user_message_handler(
            &slack,
            &event_seen_cache,
            &pool,
            &mut user_event,
            &semaphore,
            &tracker,
            &hard_abort,
        )
        .timeout(Duration::from_secs(10))
        .await
        .is_err()
        {
            info!("Timed out on draining, lost {:?} tasks!", user_event.len());
        }
    }

    info!("User Handler - Shutting down");
    tracker.close();

    // Wait for everything to finish, give them
    // 10s to finish then abort
    if let Err(e) = tracker.wait().timeout(Duration::from_secs(10)).await {
        error!("User Handler shutdown timed out, aborting children tasks: {e:?}");
        hard_abort.cancel();
        if let Err(e) = tracker.wait().timeout(Duration::from_secs(1)).await {
            error!("User Handler shutdown abort timed out, giving up: {e:?}");
        }
    }
    info!("User Handler - Exiting");
}

async fn inner_user_message_handler<S: SlackSender + 'static>(
    slack: &SlackClient<S>,
    event_seen_cache: &Arc<Cache<String, bool>>,
    pool: &Pool,
    user_event: &mut Receiver<Envelope>,
    semaphore: &Arc<Semaphore>,
    tracker: &TaskTracker,
    hard_abort: &CancellationToken,
) {
    loop {
        match user_event.recv().await {
            Err(RecvError::Closed) => {
                // We are done here Bail out of the loop
                info!("Producer closed queue, bailing");
                break;
            }
            Err(RecvError::Lagged(count)) => {
                error!("Lagging, lost {count} messages!");
            }
            Ok(val) => match Arc::clone(semaphore).acquire_owned().await {
                Ok(permit) => {
                    let event_seen_cache = Arc::clone(event_seen_cache);
                    let pool = pool.clone();
                    let slack = slack.clone();
                    let abort = hard_abort.clone();
                    tracker.spawn(
                        async move {
                            let permit = permit;
                            if let Err(e) =
                                process_user_message(val, slack, event_seen_cache, &pool).await
                            {
                                error!("user_event::process_user_message error: {e:?}");
                            }
                            drop(permit);
                        }
                        .with_cancellation_token_owned(abort),
                    );
                }
                Err(e) => {
                    error!("Task Semaphore closed: {e:?}");
                    break;
                }
            },
        }
    }
}
