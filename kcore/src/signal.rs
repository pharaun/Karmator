use tokio::sync::watch;

use log::{error, info};

use anyhow::Result as AResult;

#[cfg(windows)]
use tokio::signal;

#[cfg(unix)]
use tokio::signal::unix;

#[derive(Clone)]
pub struct Watcher {
    shutdown: (watch::Sender<bool>, watch::Receiver<bool>),
}

impl Watcher {
    fn new() -> Self {
        Self {
            shutdown: watch::channel(false),
        }
    }

    pub async fn shutdown(&mut self) {
        if let Err(e) = self.shutdown.1.wait_for(|v| *v).await {
            error!("Signal shutdown error: {e:?}");
        }
    }

    pub fn should_shutdown(&self) -> bool {
        *self.shutdown.1.borrow()
    }

    pub fn shutdown_now(&mut self) {
        if let Err(e) = self.shutdown.0.send(true) {
            error!("Signal shutdown error: {e:?}");
        }
    }
}

pub struct Signal {
    pub watcher: Watcher,
    #[cfg(unix)]
    sig_int: unix::Signal,
    #[cfg(unix)]
    sig_term: unix::Signal,
}

impl Signal {
    #[cfg(windows)]
    pub fn new() -> AResult<Self> {
        info!("Signal installed");
        Ok(Self {
            watcher: Watcher::new(),
        })
    }

    #[cfg(unix)]
    pub fn new() -> AResult<Self> {
        info!("Signal installed");
        Ok(Self {
            watcher: Watcher::new(),
            sig_int: unix::signal(unix::SignalKind::interrupt())?,
            sig_term: unix::signal(unix::SignalKind::terminate())?,
        })
    }

    #[cfg(unix)]
    pub async fn shutdown_signal(&mut self) -> AResult<()> {
        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.watcher.shutdown.1.wait_for(|v| *v) => (),
            _ = self.sig_int.recv() => (),
            _ = self.sig_term.recv() => (),
        };

        // We returned from the select, shutdown has been invoked.
        self.watcher.shutdown.0.send(true)?;
        Ok(())
    }

    #[cfg(windows)]
    pub async fn shutdown_signal(&mut self) -> AResult<()> {
        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.watcher.shutdown.1.wait_for(|v| *v) => (),
            _ = signal::ctrl_c() => (),
        };

        // We returned from the select, shutdown has been invoked.
        self.watcher.shutdown.0.send(true)?;
        Ok(())
    }

    pub fn get_shutdown_watcher(&self) -> Watcher {
        self.watcher.clone()
    }
}
