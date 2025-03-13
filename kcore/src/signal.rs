use tokio::sync::watch;

use log::{error, info};

use anyhow::Result as AResult;

#[cfg(windows)]
use tokio::signal;

#[cfg(unix)]
use tokio::signal::unix;

#[derive(Clone)]
pub struct Signal {
    shutdown: (watch::Sender<bool>, watch::Receiver<bool>),
    external_shutdown: watch::Receiver<bool>,
}

impl Signal {
    pub fn new() -> (watch::Sender<bool>, Signal) {
        let (tx, rx) = watch::channel(false);
        info!("Signal installed");

        (
            tx,
            Signal {
                shutdown: watch::channel(false),
                external_shutdown: rx,
            },
        )
    }

    #[cfg(unix)]
    pub async fn shutdown_daemon(&mut self) -> AResult<()> {
        let mut sig_int = unix::signal(unix::SignalKind::interrupt())?;
        let mut sig_term = unix::signal(unix::SignalKind::terminate())?;

        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.external_shutdown.wait_for(|v| *v == true) => (),
            _ = self.shutdown.1.wait_for(|v| *v == true) => (),
            _ = sig_int.recv() => (),
            _ = sig_term.recv() => (),
        };

        // We returned from the select, shutdown has been invoked.
        self.shutdown.0.send(true)?;
        Ok(())
    }

    #[cfg(windows)]
    pub async fn shutdown_daemon(&mut self) -> AResult<()> {
        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.external_shutdown.wait_for(|v| *v == true) => (),
            _ = self.shutdown.1.wait_for(|v| *v == true) => (),
            _ = signal::ctrl_c() => (),
        };

        // We returned from the select, shutdown has been invoked.
        self.shutdown.0.send(true)?;
        Ok(())
    }

    pub async fn shutdown(&mut self) {
        if let Err(e) = self.shutdown.1.wait_for(|v| *v == true).await {
            error!("Signal shutdown error: {:?}", e);
        }
    }

    pub fn should_shutdown(&self) -> bool {
        *self.shutdown.1.borrow()
    }

    pub fn shutdown_now(&mut self) {
        if let Err(e) = self.shutdown.0.send(true) {
            error!("Signal shutdown error: {:?}", e);
        }
    }
}
