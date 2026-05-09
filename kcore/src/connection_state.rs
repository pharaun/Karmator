use tokio::sync::RwLock;

use std::num::NonZeroU32;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use std::time::Duration;
use std::time::Instant;

use log::error;

use backoff::backoff::Backoff as _;
use backoff::exponential::ExponentialBackoffBuilder;
use backoff::ExponentialBackoff;

use governor::DefaultDirectRateLimiter;
use governor::Quota;

pub(crate) struct ConnectionState {
    // Atomic boolean for ensuring send can't happen till the hello is received from slack
    can_send: AtomicBool,
    // Atomic boolean for exiting and re-establishing the connection
    reconnect: AtomicBool,
    reconnect_backoff: Mutex<ExponentialBackoff>,
    reconnect_limiter: DefaultDirectRateLimiter,
    reconnect_latch: AtomicBool,
    // Monotonical clock for heartbeat/ping management
    last_message_received: RwLock<Instant>,
    last_ping_sent: RwLock<Instant>,
}

impl ConnectionState {
    pub(crate) fn init() -> Self {
        Self {
            can_send: AtomicBool::new(false),
            reconnect: AtomicBool::new(false),
            reconnect_backoff: Mutex::new(
                ExponentialBackoffBuilder::new()
                    .with_initial_interval(Duration::from_millis(500))
                    .with_randomization_factor(0.5)
                    .with_multiplier(1.5)
                    // Reevaulate this and maybe back off to a much longer, tho the existing logic was
                    // already doing 10 tries of 20s each
                    .with_max_interval(Duration::from_secs(20))
                    //                    .with_max_elapsed_time(Some(Duration::from_mins(1)))
                    .build(),
            ),
            reconnect_limiter: DefaultDirectRateLimiter::direct(
                Quota::per_minute(NonZeroU32::new(5).expect("greater than zero"))
                    .allow_burst(NonZeroU32::new(5).expect("greater than zero")),
            ),
            reconnect_latch: AtomicBool::new(false),
            last_message_received: RwLock::new(Instant::now()),
            last_ping_sent: RwLock::new(Instant::now()),
        }
    }

    pub(crate) async fn pending(&self) {
        // Connection attempt, waiting ack
        self.can_send.store(false, Ordering::Relaxed);
        self.reconnect.store(false, Ordering::Relaxed);

        // Reset timer to avoid a race between reconnecting and first hello
        let now = Instant::now();
        *(self.last_message_received.write().await) = now;
        *(self.last_ping_sent.write().await) = now;
    }

    pub(crate) fn reconnect(&self) {
        // Need to reconnect,
        self.reconnect.store(true, Ordering::Relaxed);
        self.can_send.store(false, Ordering::Relaxed);
        self.reconnect_backoff
            .lock()
            .expect("Poisoned mutex")
            .reset();

        // Check the rate limit quota to catch disconnect loop,
        // if quota exceeded, latch.
        if self.reconnect_limiter.check().is_err() {
            self.reconnect_latch.store(true, Ordering::Relaxed);
        }
    }

    pub(crate) fn special_reconnect(&self) {
        // Need to reconnect,
        self.reconnect.store(true, Ordering::Relaxed);
        self.can_send.store(false, Ordering::Relaxed);
    }

    pub(crate) fn should_reconnect(&self) -> bool {
        self.reconnect.load(Ordering::Relaxed)
    }

    pub(crate) fn connected(&self) {
        // Acknowledgement of connection
        self.can_send.store(true, Ordering::Relaxed);
    }

    pub(crate) fn should_send(&self) -> bool {
        self.can_send.load(Ordering::Relaxed)
    }

    pub(crate) fn fetch_next_backoff(&self) -> Option<Duration> {
        // Check if the reconnect latch is activated, if so then we have reconnected and
        // disconnected too often, something isn't right with our code or slack. Exit.
        if self.reconnect_latch.load(Ordering::Relaxed) {
            error!("Reconnected and failed too fast, quota hit");
            None
        } else {
            self.reconnect_backoff
                .lock()
                .expect("Poisoned mutex")
                .next_backoff()
        }
    }

    pub(crate) async fn message_received(&self) {
        *(self.last_message_received.write().await) = Instant::now();
    }

    pub(crate) async fn ping_sent(&self) {
        *(self.last_ping_sent.write().await) = Instant::now();
    }

    pub(crate) async fn timer_delta(&self) -> (Option<Duration>, Option<Duration>) {
        let now = Instant::now();
        (
            now.checked_duration_since(*self.last_message_received.read().await),
            now.checked_duration_since(*self.last_ping_sent.read().await),
        )
    }
}
