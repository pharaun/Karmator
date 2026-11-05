use std::io::Write as _;

use anyhow::Result as AResult;
use crossterm::style::{Color, Stylize as _};
use futures_util::FutureExt as _;
use log::Level;
use rustyline_async::{Readline, ReadlineEvent, SharedWriter};
use tokio::sync::{broadcast, mpsc};

use karmator_tester::command::{self, Command, Context};
use karmator_tester::logging::{LogControl, LogLine};
use karmator_tester::{Activity, Connection, HarnessHandle};


pub struct Repl {
    rl: Readline,
    out: SharedWriter,
    logs: mpsc::UnboundedReceiver<LogLine>,
    log: LogControl,
}

impl Repl {
    pub fn new(logs: mpsc::UnboundedReceiver<LogLine>, log: LogControl) -> AResult<Self> {
        let (mut rl, out) = Readline::new(prompt(Connection::Waiting))?;
        rl.should_print_line_on(false, false);
        Ok(Self { rl, out, logs, log })
    }

    pub async fn drain_until<T>(&mut self, done: impl Future<Output = T>) -> T {
        tokio::pin!(done);
        let res = loop {
            tokio::select! {
                res = &mut done => break res,
                Some(line) = self.logs.recv() => self.print_log(&line),
            }
            let _ = self.rl.flush();
        };
        while let Ok(line) = self.logs.try_recv() {
            self.print_log(&line);
        }
        let _ = self.rl.flush();
        res
    }

    pub async fn run(&mut self, harness: HarnessHandle) {
        let watcher = harness.watcher.clone();
        let mut activity = harness.monitor.subscribe();
        let mut status = harness.monitor.watch_status();
        let log = self.log.clone();
        let ctx = Context {
            harness: &harness,
            log: Some(&log),
        };
        self.print(&"Ready, type `help` for commands".dim().to_string());
        self.set_prompt(status.borrow().connection);

        loop {
            let _ = self.rl.flush();
            tokio::select! {
                () = watcher.cancelled() => break,
                Some(line) = self.logs.recv() => self.print_log(&line),
                res = activity.recv() => match res {
                    Ok(a) => self.print_activity(&a),
                    Err(broadcast::error::RecvError::Lagged(n)) => {
                        self.print(&format!("... skipped {n} activity entries").yellow().to_string());
                    }
                    Err(broadcast::error::RecvError::Closed) => break,
                },
                Ok(()) = status.changed() => {
                    let connection = status.borrow_and_update().connection;
                    self.set_prompt(connection);
                }
                event = self.rl.readline().fuse() => match event {
                    Ok(ReadlineEvent::Line(line)) => self.line(&line, &ctx).await,
                    Ok(ReadlineEvent::Eof | ReadlineEvent::Interrupted) => break,
                    Err(e) => {
                        self.print(&format!("Readline error: {e:?}").red().to_string());
                        break;
                    }
                },
            }
        }
        self.print(&"Exiting...".dim().to_string());
        watcher.cancel();
    }

    async fn line(&mut self, line: &str, ctx: &Context<'_>) {
        let line = line.trim();
        if line.is_empty() {
            return;
        }
        self.rl.add_history_entry(line.to_owned());
        self.print(&format!("> {line}").blue().to_string());

        let result = match line.parse::<Command>() {
            Ok(cmd) => cmd.execute(ctx).await,
            Err(e) => Err(suggest(line, e)),
        };
        match result {
            Ok(out) if out.is_empty() => (),
            Ok(out) => self.print(&out),
            Err(e) => self.print(&format!("error: {e:#}").red().to_string()),
        }
    }

    fn set_prompt(&mut self, connection: Connection) {
        let _ = self.rl.update_prompt(&prompt(connection));
    }

    fn print(&mut self, text: &str) {
        let _ = writeln!(self.out, "{text}");
    }

    fn print_activity(&mut self, activity: &Activity) {
        let text = activity.to_string();
        let styled = match activity {
            Activity::BotReply { .. } => text.green(),
            Activity::Connection(_) | Activity::RefreshRequested => text.magenta(),
            Activity::Queued { .. } => text.yellow(),
            Activity::EventSent { .. } => text.cyan(),
            Activity::Acked { .. } | Activity::ApiCall { .. } => text.dim(),
        };
        self.print(&styled.to_string());
    }

    fn print_log(&mut self, line: &LogLine) {
        let text = format!("  {line}");
        let styled = match line.level {
            Level::Error => text.red(),
            Level::Warn => text.yellow(),
            Level::Info | Level::Debug | Level::Trace => text.dim(),
        };
        self.print(&styled.to_string());
    }
}

fn prompt(connection: Connection) -> String {
    let (color, label) = match connection {
        Connection::Connected => (Color::Green, "connected"),
        Connection::Waiting => (Color::Yellow, "waiting"),
        Connection::NetDown => (Color::Red, "netdown"),
        Connection::CrashLoop => (Color::Red, "crashloop"),
        Connection::Stopped => (Color::Red, "stopped"),
    };
    format!("{} > ", format!("[{label}]").with(color))
}

fn suggest(line: &str, err: anyhow::Error) -> anyhow::Error {
    let cmd = line.split_whitespace().next().unwrap_or("").to_lowercase();
    let matches: Vec<_> = command::COMMANDS
        .iter()
        .map(|c| c.name)
        .filter(|n| {
            let n = n.to_lowercase();
            !cmd.is_empty() && (n.starts_with(&cmd) || cmd.starts_with(&n))
        })
        .collect();
    if matches.is_empty() {
        err
    } else {
        anyhow::anyhow!("{err} - did you mean: {}", matches.join(", "))
    }
}
