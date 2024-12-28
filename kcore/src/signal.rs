use tokio::sync::watch;

use log::info;

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

        (tx, Signal {
            shutdown: watch::channel(false),
            external_shutdown: rx,
        })
    }

    #[cfg(unix)]
    pub async fn shutdown_daemon(&mut self) {
        let mut sig_int = unix::signal(unix::SignalKind::interrupt()).unwrap();
        let mut sig_term = unix::signal(unix::SignalKind::terminate()).unwrap();

        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.external_shutdown.wait_for(|v| *v == true) => (),
            _ = self.shutdown.1.wait_for(|v| *v == true) => (),
            _ = sig_int.recv() => (),
            _ = sig_term.recv() => (),
        };

        // We returned from the select, shutdown has been invoked.
        let _ = self.shutdown.0.send(true);
    }

    #[cfg(windows)]
    pub async fn shutdown_daemon(&mut self) {
        // This will never exit till shutdown is true
        tokio::select! {
            _ = self.external_shutdown.wait_for(|v| *v == true) => (),
            _ = self.shutdown.1.wait_for(|v| *v == true) => (),
            _ = signal::ctrl_c() => (),
        };

        // We returned from the select, shutdown has been invoked.
        let _ = self.shutdown.0.send(true);
    }

    pub async fn shutdown(&mut self) {
        let _ = self.shutdown.1.wait_for(|v| *v == true).await;
    }

    pub fn should_shutdown(&self) -> bool {
        *self.shutdown.1.borrow()
    }

    pub fn shutdown_now(&mut self) {
        let _ = self.shutdown.0.send(true);
    }
}
