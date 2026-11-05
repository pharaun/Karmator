use std::io::Write as _;

use futures_util::FutureExt as _;
use log::info;
use rustyline_async::{Readline, ReadlineEvent, SharedWriter};

use karmator_tester::HarnessHandle;
use karmator_tester::command::Command;

pub async fn run(rl: &mut Readline, mut stdout: SharedWriter, harness: HarnessHandle) {
    let watcher = harness.watcher.clone();
    while !watcher.is_cancelled() {
        let _ = rl.flush();
        let event = tokio::select! {
            () = watcher.cancelled() => break,
            event = rl.readline().fuse() => event,
        };

        match event {
            Ok(ReadlineEvent::Line(line)) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(line.to_owned());

                let result = match line.parse::<Command>() {
                    Ok(cmd) => cmd.execute(&harness).await,
                    Err(e) => Err(e),
                };
                match result {
                    Ok(out) if out.is_empty() => (),
                    Ok(out) => {
                        let _ = writeln!(stdout, "{out}");
                    }
                    Err(e) => {
                        let _ = writeln!(stdout, "Error: {e:#}");
                    }
                }
            }
            Ok(ReadlineEvent::Eof) => {
                let _ = writeln!(stdout, "Exiting...");
                break;
            }
            Ok(ReadlineEvent::Interrupted) => {
                let _ = writeln!(stdout, "^C");
                break;
            }
            Err(e) => {
                let _ = writeln!(stdout, "Readline error: {e:?}, exiting...");
                break;
            }
        }
    }
    watcher.cancel();
    info!("Terminal Handler - Exiting");
}
