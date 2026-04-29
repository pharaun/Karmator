use tokio::sync::RwLock;

use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

pub struct ConnectionState {
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
    pub fn init() -> Self {
        Self {
            can_send: Arc::new(AtomicBool::new(false)),
            reconnect: Arc::new(AtomicBool::new(false)),
            reconnect_count: Arc::new(AtomicUsize::new(0)),
            last_message_received: Arc::new(RwLock::new(Instant::now())),
            last_ping_sent: Arc::new(RwLock::new(Instant::now())),
        }
    }

    pub fn pending(&self) {
        // Connection attempt, waiting ack
        self.can_send.store(false, Ordering::Relaxed);
        self.reconnect.store(false, Ordering::Relaxed);
    }

    pub fn reconnect(&self) {
        // Need to reconnect,
        self.reconnect.store(true, Ordering::Relaxed);
        self.can_send.store(false, Ordering::Relaxed);
    }

    pub fn should_reconnect(&self) -> bool {
        self.reconnect.load(Ordering::Relaxed)
    }

    pub fn connected(&self) {
        // Acknowledgement of connection
        self.can_send.store(true, Ordering::Relaxed);
        self.reconnect_count.store(0, Ordering::Relaxed);
    }

    pub fn should_send(&self) -> bool {
        self.can_send.load(Ordering::Relaxed)
    }

    pub fn fetch_reconnect_count_add(&self) -> usize {
        self.reconnect_count.fetch_add(1, Ordering::Relaxed)
    }

    pub async fn message_received(&self) {
        let mut timer = self.last_message_received.write().await;
        *timer = Instant::now();
    }

    pub async fn ping_sent(&self) {
        let mut timer = self.last_ping_sent.write().await;
        *timer = Instant::now();
    }

    pub async fn timer_delta(&self) -> (Option<Duration>, Option<Duration>) {
        let now = Instant::now();

        (
            {
                let timer = self.last_message_received.read().await;
                now.checked_duration_since(*timer)
            },
            {
                let timer = self.last_ping_sent.read().await;
                now.checked_duration_since(*timer)
            },
        )
    }
}
