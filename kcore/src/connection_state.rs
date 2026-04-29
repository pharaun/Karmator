use tokio::sync::RwLock;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

pub(crate) struct ConnectionState {
    // Atomic boolean for ensuring send can't happen till the hello is received from slack
    can_send: Arc<AtomicBool>,
    // Atomic boolean for exiting and re-establishing the connection
    reconnect: Arc<AtomicBool>,
    reconnect_count: Arc<AtomicUsize>,
    // Monotonical clock for heartbeat/ping management
    last_message_received: Arc<RwLock<Instant>>,
    last_ping_sent: Arc<RwLock<Instant>>,
}

impl ConnectionState {
    pub(crate) fn init() -> Self {
        Self {
            can_send: Arc::new(AtomicBool::new(false)),
            reconnect: Arc::new(AtomicBool::new(false)),
            reconnect_count: Arc::new(AtomicUsize::new(0)),
            last_message_received: Arc::new(RwLock::new(Instant::now())),
            last_ping_sent: Arc::new(RwLock::new(Instant::now())),
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
    }

    pub(crate) fn should_reconnect(&self) -> bool {
        self.reconnect.load(Ordering::Relaxed)
    }

    pub(crate) fn connected(&self) {
        // Acknowledgement of connection
        self.can_send.store(true, Ordering::Relaxed);
        self.reconnect_count.store(0, Ordering::Relaxed);
    }

    pub(crate) fn should_send(&self) -> bool {
        self.can_send.load(Ordering::Relaxed)
    }

    pub(crate) fn fetch_reconnect_count_add(&self) -> usize {
        self.reconnect_count.fetch_add(1, Ordering::Relaxed)
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
