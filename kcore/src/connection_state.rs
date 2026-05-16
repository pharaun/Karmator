use tokio::sync::RwLock;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use std::time::Duration;
use std::time::Instant;

use log::warn;

use backoff::backoff::Backoff as _;
use backoff::exponential::ExponentialBackoffBuilder;
use backoff::ExponentialBackoff;

pub(crate) struct ConnectionState {
    // Atomic boolean for ensuring send can't happen till the hello is received from slack
    can_send: AtomicBool,
    // Atomic boolean for exiting and re-establishing the connection
    reconnect: AtomicBool,
    reconnect_backoff: Mutex<ExponentialBackoff>,
    // Monotonical clock for heartbeat/ping management
    last_message_received: RwLock<Instant>,
    last_ping_sent: RwLock<Instant>,
    last_connect: RwLock<Instant>,
}

impl ConnectionState {
    pub(crate) fn init() -> Self {
        Self {
            can_send: AtomicBool::new(false),
            reconnect: AtomicBool::new(false),
            reconnect_backoff: Mutex::new(
                ExponentialBackoffBuilder::new()
                    .with_initial_interval(Duration::from_secs(1))
                    .with_randomization_factor(0.5)
                    .with_multiplier(2.0)
                    .with_max_interval(Duration::from_mins(1))
                    .with_max_elapsed_time(Some(Duration::from_mins(15)))
                    .build(),
            ),
            last_message_received: RwLock::new(Instant::now()),
            last_ping_sent: RwLock::new(Instant::now()),
            last_connect: RwLock::new(Instant::now()),
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

    pub(crate) async fn reset_backoff_timer(&self) {
        let now = Instant::now();
        let durt = now.checked_duration_since(*self.last_connect.read().await);

        match durt {
            // Ensure the durt is at least X before allowing reconnect backoff reset:
            Some(t) => {
                if t.as_secs() > 30 {
                    self.reconnect_backoff
                        .lock()
                        .expect("Poisoned mutex")
                        .reset();
                } else {
                    warn!("reset_backoff_timer fired too soon, ignoring till 30s has passed");
                }
            }
            None => warn!("reset_backoff_timer returned None, timing is wonky"),
        }
    }

    pub(crate) fn reconnect(&self) {
        // Need to reconnect,
        self.reconnect.store(true, Ordering::Relaxed);
        self.can_send.store(false, Ordering::Relaxed);
    }

    pub(crate) fn should_reconnect(&self) -> bool {
        self.reconnect.load(Ordering::Relaxed)
    }

    pub(crate) async fn connected(&self) {
        // Acknowledgement of connection
        self.can_send.store(true, Ordering::Relaxed);

        // So that we forces the reset of backoff to be at least
        // a certain amount of time
        *(self.last_connect.write().await) = Instant::now();
    }

    pub(crate) fn should_send(&self) -> bool {
        self.can_send.load(Ordering::Relaxed)
    }

    pub(crate) fn fetch_next_backoff(&self) -> Option<Duration> {
        self.reconnect_backoff
            .lock()
            .expect("Poisoned mutex")
            .next_backoff()
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
