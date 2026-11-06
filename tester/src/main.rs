mod tui;

use anyhow::Result as AResult;
use log::LevelFilter;
use tokio::sync::oneshot;
use tokio_util::sync::CancellationToken;

use karmator_tester::{Harness, HarnessConfig, logging};
use kcore::wait_for_signal;

#[tokio::main]
async fn main() -> AResult<()> {
    let (log, logs) = logging::init(LevelFilter::Info)?;
    let watcher = CancellationToken::new();
    let (ready_tx, ready_rx) = oneshot::channel();
    let (done_tx, done_rx) = oneshot::channel();

    let w = watcher.clone();
    let harness = tokio::spawn(async move {
        let mut harness = Harness::start(w.clone(), HarnessConfig::default()).await?;
        harness.spawn(wait_for_signal(w));
        let _ = ready_tx.send(harness.handle());
        harness.run().await;
        let _ = done_tx.send(());
        AResult::<()>::Ok(())
    });

    let ui = tui::run(log, logs, watcher.clone(), ready_rx, done_rx).await;
    watcher.cancel();
    harness.await??;
    ui
}
