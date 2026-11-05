mod repl;

use anyhow::Result as AResult;
use log::{LevelFilter, error};
use tokio_util::sync::CancellationToken;

use karmator_tester::{Harness, HarnessConfig, logging};
use kcore::wait_for_signal;

#[tokio::main]
async fn main() -> AResult<()> {
    let (log, logs) = logging::init(LevelFilter::Info)?;
    let mut repl = repl::Repl::new(logs, log)?;

    let watcher = CancellationToken::new();
    let start = Harness::start(watcher.clone(), HarnessConfig::default());
    let mut harness = match repl.drain_until(start).await {
        Ok(harness) => harness,
        Err(e) => {
            error!("Harness failed to start: {e:#}");
            repl.drain_until(async {}).await;
            return Err(e);
        }
    };

    harness.spawn(wait_for_signal(watcher));
    let handle = harness.handle();
    let run = harness.run();
    tokio::pin!(run);

    let finished = tokio::select! {
        () = &mut run => true,
        () = repl.run(handle) => false,
    };
    if !finished {
        repl.drain_until(run).await;
    }
    Ok(())
}
