mod repl;

use anyhow::Result as AResult;

use std::time::Duration;

use rustyline_async::Readline;
use tokio::time;
use tokio_util::sync::CancellationToken;

use karmator_tester::{Harness, ServerConfig};
use kcore::wait_for_signal;

#[tokio::main]
async fn main() -> AResult<()> {
    let (mut rl, stdout) = Readline::new("> ".to_owned())?;

    simplelog::WriteLogger::init(
        log::LevelFilter::Debug,
        simplelog::Config::default(),
        stdout.clone(),
    )?;

    let watcher = CancellationToken::new();
    let mut harness = Harness::start(watcher.clone(), ServerConfig::default()).await?;
    harness.spawn(wait_for_signal(watcher));
    let handle = harness.handle();

    let run = harness.run();
    tokio::pin!(run);
    tokio::select! {
        () = &mut run => (),
        () = repl::run(&mut rl, stdout, handle) => {
            // Repl is done, keep printing logs until everything has shut down
            let mut tick = time::interval(Duration::from_millis(100));
            loop {
                tokio::select! {
                    () = &mut run => break,
                    _ = tick.tick() => { let _ = rl.flush(); }
                }
            }
        }
    }
    let _ = rl.flush();
    Ok(())
}
