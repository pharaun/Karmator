use parking_lot::Mutex;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use tokio::time::Duration;
use tokio::time::Instant;

use log::warn;

use backoff::ExponentialBackoff;
use backoff::backoff::Backoff as _;
use backoff::exponential::ExponentialBackoffBuilder;

// Note: If we ever want to have independent watcher of these values
// we may want figure out a better way to manage this there's various ways
// that state can leak out in an inconsistent way here as it is.
pub(crate) struct ConnectionState {
    // Atomic boolean for ensuring send can't happen till the hello is received from slack
    can_send: AtomicBool,
    // Atomic boolean for exiting and re-establishing the connection
    reconnect: AtomicBool,
    reconnect_backoff: Mutex<ExponentialBackoff>,
    // Monotonical clock for heartbeat/ping management
    last_message_received: Mutex<Instant>,
    last_ping_sent: Mutex<Instant>,
    last_connect: Mutex<Option<Instant>>,
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
            last_message_received: Mutex::new(Instant::now()),
            last_ping_sent: Mutex::new(Instant::now()),
            last_connect: Mutex::new(None),
        }
    }

    pub(crate) fn pending(&self) {
        // Reset timer to avoid a race between reconnecting and first hello
        let now = Instant::now();
        *(self.last_message_received.lock()) = now;
        *(self.last_ping_sent.lock()) = now;

        // Connection attempt, waiting ack
        self.can_send.store(false, Ordering::Release);
        self.reconnect.store(false, Ordering::Release);
    }

    pub(crate) fn reset_backoff_timer(&self) {
        let last_connect = *self.last_connect.lock();
        match last_connect {
            None => warn!("Tried to reset_backoff_timer without an connection being alive!"),
            Some(last_connect) => {
                let now = Instant::now();
                let durt = now.checked_duration_since(last_connect);

                match durt {
                    None => warn!("reset_backoff_timer returned None, timing is wonky"),
                    Some(t) => {
                        // Ensure the durt is at least X before allowing reconnect backoff reset:
                        if t.as_secs() > 30 {
                            self.reconnect_backoff.lock().reset();
                        } else {
                            warn!(
                                "reset_backoff_timer fired too soon, ignoring till 30s has passed"
                            );
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn reconnect(&self) {
        *(self.last_connect.lock()) = None;
        self.reconnect.store(true, Ordering::Release);
        self.can_send.store(false, Ordering::Release);
    }

    pub(crate) fn should_reconnect(&self) -> bool {
        self.reconnect.load(Ordering::Acquire)
    }

    pub(crate) fn connected(&self) {
        // So that we forces the reset of backoff to be at least
        // a certain amount of time
        *(self.last_connect.lock()) = Some(Instant::now());

        // Acknowledgement of connection
        self.can_send.store(true, Ordering::Release);
    }

    pub(crate) fn should_send(&self) -> bool {
        self.can_send.load(Ordering::Acquire)
    }

    pub(crate) fn fetch_next_backoff(&self) -> Option<Duration> {
        self.reconnect_backoff.lock().next_backoff()
    }

    pub(crate) fn message_received(&self) {
        *(self.last_message_received.lock()) = Instant::now();
    }

    pub(crate) fn ping_sent(&self) {
        *(self.last_ping_sent.lock()) = Instant::now();
    }

    pub(crate) fn timer_delta(&self) -> (Option<Duration>, Option<Duration>) {
        let now = Instant::now();
        (
            now.checked_duration_since(*self.last_message_received.lock()),
            now.checked_duration_since(*self.last_ping_sent.lock()),
        )
    }
}
