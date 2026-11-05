use std::fmt;
use std::sync::{Arc, Mutex, PoisonError};
use std::time::Instant;

use serde_json::Value;
use tokio::sync::{broadcast, watch};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Connection {
    Waiting,
    Connected,
    NetDown,
    CrashLoop,
    Stopped,
}

#[derive(Debug, Clone)]
pub enum Activity {
    Connection(Connection),
    RefreshRequested,
    Queued {
        pending: usize,
    },
    EventSent {
        envelope_id: String,
        event: Value,
    },
    Acked {
        envelope_id: String,
    },
    ApiCall {
        method: String,
    },
    BotReply {
        channel: String,
        text: String,
        thread_ts: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub struct Status {
    pub connection: Connection,
    pub connections: u64,
    pub events_sent: u64,
    pub events_acked: u64,
    pub unacked: usize,
    pub pending: usize,
    pub bot_replies: u64,
    pub api_calls: u64,
    pub started: Instant,
}

impl Default for Status {
    fn default() -> Self {
        Self {
            connection: Connection::Waiting,
            connections: 0,
            events_sent: 0,
            events_acked: 0,
            unacked: 0,
            pending: 0,
            bot_replies: 0,
            api_calls: 0,
            started: Instant::now(),
        }
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "bot: {:?} (connections: {}) | events sent: {}, acked: {}, unacked: {}, queued: {} \
             | replies: {}, api calls: {} | up {}s",
            self.connection,
            self.connections,
            self.events_sent,
            self.events_acked,
            self.unacked,
            self.pending,
            self.bot_replies,
            self.api_calls,
            self.started.elapsed().as_secs(),
        )
    }
}

#[derive(Default)]
struct Inner {
    unacked: Vec<String>,
}

#[derive(Clone)]
pub struct Monitor {
    activity: broadcast::Sender<Activity>,
    status: Arc<watch::Sender<Status>>,
    inner: Arc<Mutex<Inner>>,
}

impl Default for Monitor {
    fn default() -> Self {
        Self::new()
    }
}

impl Monitor {
    pub fn new() -> Self {
        Self {
            activity: broadcast::channel(1024).0,
            status: Arc::new(watch::channel(Status::default()).0),
            inner: Arc::default(),
        }
    }

    pub fn subscribe(&self) -> broadcast::Receiver<Activity> {
        self.activity.subscribe()
    }

    pub fn watch_status(&self) -> watch::Receiver<Status> {
        self.status.subscribe()
    }

    pub fn status(&self) -> Status {
        self.status.borrow().clone()
    }

    pub(crate) fn report(&self, activity: Activity) {
        self.status.send_modify(|s| {
            let mut inner = self.inner.lock().unwrap_or_else(PoisonError::into_inner);
            match &activity {
                Activity::Connection(c) => {
                    if *c == Connection::Connected {
                        s.connections += 1;
                    }
                    s.connection = *c;
                }
                Activity::RefreshRequested => (),
                Activity::Queued { pending } => s.pending = *pending,
                Activity::EventSent { envelope_id, .. } => {
                    s.events_sent += 1;
                    s.pending = s.pending.saturating_sub(1);
                    inner.unacked.push(envelope_id.clone());
                }
                Activity::Acked { envelope_id } => {
                    // Duplicate sends get acked twice, only count the first
                    if let Some(i) = inner.unacked.iter().position(|id| id == envelope_id) {
                        inner.unacked.swap_remove(i);
                        s.events_acked += 1;
                    }
                }
                Activity::ApiCall { .. } => s.api_calls += 1,
                Activity::BotReply { .. } => s.bot_replies += 1,
            }
            s.unacked = inner.unacked.len();
        });
        let _ = self.activity.send(activity);
    }
}

impl fmt::Display for Activity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Connection(c) => write!(f, "connection: {c:?}"),
            Self::RefreshRequested => write!(f, "asked bot to reconnect"),
            Self::Queued { pending } => write!(f, "bot offline, event queued ({pending} pending)"),
            Self::EventSent { envelope_id, event } => {
                let kind = event["type"].as_str().unwrap_or("?");
                match kind {
                    "message" => write!(f, "-> message {:?}", event["text"].as_str().unwrap_or("")),
                    _ => write!(
                        f,
                        "-> {kind} {:?}",
                        event["reaction"].as_str().unwrap_or("")
                    ),
                }?;
                write!(f, " [{envelope_id}]")
            }
            Self::Acked { envelope_id } => write!(f, "ack [{envelope_id}]"),
            Self::ApiCall { method } => write!(f, "api {method}"),
            Self::BotReply {
                channel,
                text,
                thread_ts,
            } => {
                write!(f, "<- {channel}")?;
                if let Some(ts) = thread_ts {
                    write!(f, " (thread {ts})")?;
                }
                write!(f, ": {text}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ack_tracking() {
        let m = Monitor::new();
        m.report(Activity::Queued { pending: 1 });
        m.report(Activity::EventSent {
            envelope_id: "a".to_owned(),
            event: Value::Null,
        });
        assert_eq!(m.status().pending, 0);
        assert_eq!(m.status().unacked, 1);

        m.report(Activity::Acked {
            envelope_id: "a".to_owned(),
        });
        m.report(Activity::Acked {
            envelope_id: "a".to_owned(),
        });
        let s = m.status();
        assert_eq!((s.events_sent, s.events_acked, s.unacked), (1, 1, 0));
    }
}
