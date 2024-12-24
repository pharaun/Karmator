use tokio::sync::watch;

use log::info;

#[cfg(windows)]
use tokio::signal;

#[cfg(unix)]
use tokio::signal::unix;


pub struct Signal {
    shutdown: bool,

    external_shutdown: watch::Receiver<bool>,

    #[cfg(unix)]
    sig_int: unix::Signal,

    #[cfg(unix)]
    sig_term: unix::Signal,
}

impl Signal {
    pub fn new() -> (watch::Sender<bool>, Signal) {
        info!("Signal installed");
        let (tx, rx) = watch::channel(false);

        (tx, Signal {
            shutdown: false,
            external_shutdown: rx,

            #[cfg(unix)]
            sig_int: unix::signal(unix::SignalKind::interrupt()).unwrap(),

            #[cfg(unix)]
            sig_term: unix::signal(unix::SignalKind::terminate()).unwrap(),
        })
    }

    #[cfg(unix)]
    pub async fn shutdown(&mut self) {
        self.shutdown = tokio::select! {
            _ = self.external_shutdown.changed() => *self.external_shutdown.borrow(),
            _ = self.sig_int.recv() => true,
            _ = self.sig_term.recv() => true,
        }
    }

    #[cfg(windows)]
    pub async fn shutdown(&mut self) {
        self.shutdown = tokio::select! {
            _ = self.external_shutdown.changed() => *self.external_shutdown.borrow(),
            _ = signal::ctrl_c() => true,
        }
    }

    pub fn should_shutdown(&self) -> bool {
        self.shutdown
    }

    pub fn shutdown_now(&mut self) {
        self.shutdown = true;
    }
}
