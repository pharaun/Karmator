use std::fmt;
use std::sync::{Arc, OnceLock, PoisonError, RwLock};
use std::time::SystemTime;

use anyhow::Result as AResult;
use anyhow::anyhow;

use log::{LevelFilter, Log, Metadata, Record};
use tokio::sync::mpsc;

const NOISY: &[&str] = &[
    "bollard",
    "testcontainers",
    "hyper",
    "hyper_util",
    "reqwest",
    "rustls",
    "tungstenite",
    "tokio_tungstenite",
    "tokio_postgres",
    "deadpool",
    "deadpool_postgres",
    "h2",
    "mio",
];

#[derive(Debug, Clone)]
pub struct LogLine {
    pub time: SystemTime,
    pub level: log::Level,
    pub target: String,
    pub message: String,
}

impl fmt::Display for LogLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:5} {}: {}", self.level, self.target, self.message)
    }
}

#[derive(Debug, Clone)]
struct Filters {
    default: LevelFilter,
    // (target prefix, level), longest prefix wins
    targets: Vec<(String, LevelFilter)>,
}

impl Filters {
    fn level_for(&self, target: &str) -> LevelFilter {
        self.targets
            .iter()
            .filter(|(prefix, _)| {
                target == prefix
                    || target
                        .strip_prefix(prefix.as_str())
                        .is_some_and(|rest| rest.starts_with("::"))
            })
            .max_by_key(|(prefix, _)| prefix.len())
            .map_or(self.default, |(_, level)| *level)
    }

    fn max(&self) -> LevelFilter {
        self.targets
            .iter()
            .map(|(_, l)| *l)
            .fold(self.default, Ord::max)
    }
}

#[derive(Clone)]
pub struct LogControl {
    filters: Arc<RwLock<Filters>>,
}

impl LogControl {
    pub fn set_default(&self, level: LevelFilter) {
        self.update(|f| f.default = level);
    }

    pub fn set_target(&self, target: &str, level: LevelFilter) {
        self.update(|f| {
            f.targets.retain(|(t, _)| t != target);
            f.targets.push((target.to_owned(), level));
        });
    }

    pub fn describe(&self) -> String {
        let f = self.filters.read().unwrap_or_else(PoisonError::into_inner);
        let mut out = format!("default: {}", f.default);
        let mut targets = f.targets.clone();
        targets.sort();
        for (t, l) in targets {
            out.push_str(&format!(", {t}: {l}"));
        }
        out
    }

    fn update(&self, change: impl FnOnce(&mut Filters)) {
        let mut f = self.filters.write().unwrap_or_else(PoisonError::into_inner);
        change(&mut f);
        log::set_max_level(f.max());
    }
}

struct ChannelLogger {
    filters: Arc<RwLock<Filters>>,
    tx: mpsc::UnboundedSender<LogLine>,
}

impl Log for ChannelLogger {
    fn enabled(&self, metadata: &Metadata<'_>) -> bool {
        let f = self.filters.read().unwrap_or_else(PoisonError::into_inner);
        metadata.level() <= f.level_for(metadata.target())
    }

    fn log(&self, record: &Record<'_>) {
        if self.enabled(record.metadata()) {
            let _ = self.tx.send(LogLine {
                time: SystemTime::now(),
                level: record.level(),
                target: record.target().to_owned(),
                message: record.args().to_string(),
            });
        }
    }

    fn flush(&self) {}
}

static CONTROL: OnceLock<LogControl> = OnceLock::new();

pub fn init(default: LevelFilter) -> AResult<(LogControl, mpsc::UnboundedReceiver<LogLine>)> {
    let filters = Arc::new(RwLock::new(Filters {
        default,
        targets: NOISY
            .iter()
            .map(|t| ((*t).to_owned(), LevelFilter::Warn))
            .collect(),
    }));
    let (tx, rx) = mpsc::unbounded_channel();
    log::set_boxed_logger(Box::new(ChannelLogger {
        filters: Arc::clone(&filters),
        tx,
    }))
    .map_err(|e| anyhow!("Logger already installed: {e}"))?;

    let control = LogControl { filters };
    log::set_max_level(
        control
            .filters
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .max(),
    );
    let _ = CONTROL.set(control.clone());
    Ok((control, rx))
}

pub fn control() -> Option<&'static LogControl> {
    CONTROL.get()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn longest_prefix_wins() {
        let f = Filters {
            default: LevelFilter::Info,
            targets: vec![
                ("kcore".to_owned(), LevelFilter::Debug),
                ("kcore::bot".to_owned(), LevelFilter::Error),
                ("hyper".to_owned(), LevelFilter::Warn),
            ],
        };
        assert_eq!(f.level_for("kcore"), LevelFilter::Debug);
        assert_eq!(f.level_for("kcore::event"), LevelFilter::Debug);
        assert_eq!(f.level_for("kcore::bot"), LevelFilter::Error);
        assert_eq!(f.level_for("hyper_util::client"), LevelFilter::Info);
        assert_eq!(f.level_for("karmator"), LevelFilter::Info);
        assert_eq!(f.max(), LevelFilter::Debug);
    }
}
