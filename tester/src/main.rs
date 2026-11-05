mod repl;

use anyhow::Result as AResult;

use rustyline_async::Readline;
use tokio_util::sync::CancellationToken;

use karmator_tester::{Harness, ServerConfig};
use kcore::wait_for_signal;

#[tokio::main]
async fn main() -> AResult<()> {
    let (rl, stdout) = Readline::new("> ".to_owned())?;

    simplelog::WriteLogger::init(
        log::LevelFilter::Debug,
        simplelog::Config::default(),
        stdout.clone(),
    )?;

    let watcher = CancellationToken::new();
    let mut harness = Harness::start(watcher.clone(), ServerConfig::default()).await?;
    harness.spawn(wait_for_signal(watcher));
    harness.spawn(repl::run(rl, stdout, harness.handle()));
    harness.run().await;
    Ok(())
}
