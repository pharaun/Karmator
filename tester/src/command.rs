use std::fmt::Write as _;
use std::str::FromStr;
use std::time::Duration;

use anyhow::Result as AResult;
use anyhow::{Context as _, bail};

use log::LevelFilter;

use crate::events;
use crate::harness::HarnessHandle;
use crate::logging::LogControl;

pub struct Spec {
    pub name: &'static str,
    pub args: &'static str,
    pub help: &'static str,
}

#[rustfmt::skip]
pub const COMMANDS: &[Spec] = &[
    Spec { name: "help", args: "", help: "this help" },
    Spec { name: "send", args: "<text>", help: "send a message to the bot" },
    Spec { name: "spam", args: "<count> <text>", help: "send the message <count> times" },
    Spec { name: "addReacji", args: "<name>", help: "send a reaction_added event" },
    Spec { name: "delReacji", args: "<name>", help: "send a reaction_removed event" },
    Spec { name: "reconnect", args: "[secs]", help: "drop the websocket and refuse connections for secs (default 1)" },
    Spec { name: "crash", args: "", help: "put the websocket server into a slack crashloop" },
    Spec { name: "status", args: "", help: "connection + event counters" },
    Spec { name: "log", args: "[level] [target]", help: "show or set log levels (ie `log debug`, `log trace kcore`)" },
    Spec { name: "truncate", args: "", help: "empty all tables" },
    Spec { name: "reset", args: "", help: "drop + reload the schema" },
    Spec { name: "db", args: "", help: "print the psql url for the scratch database" },
    Spec { name: "quit", args: "", help: "exit" },
];

pub fn help() -> String {
    COMMANDS
        .iter()
        .map(|c| format!("{:<28} - {}", format!("{} {}", c.name, c.args), c.help))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn complete(prefix: &str) -> impl Iterator<Item = &'static str> + '_ {
    COMMANDS
        .iter()
        .map(|c| c.name)
        .filter(move |n| n.starts_with(prefix))
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Help,
    Send(String),
    Spam(u64, String),
    AddReacji(String),
    DelReacji(String),
    Reconnect(Duration),
    Crash,
    Status,
    Log(Option<LevelFilter>, Option<String>),
    Truncate,
    Reset,
    Db,
    Quit,
}

impl FromStr for Command {
    type Err = anyhow::Error;

    // Text arguments are taken verbatim (rest of the line) so karma syntax like
    // `send "foo bar"++` reaches the bot exactly as typed.
    fn from_str(line: &str) -> AResult<Self> {
        let line = line.trim();
        let (cmd, rest) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let rest = rest.trim_start();

        Ok(match cmd {
            "help" | "?" => Self::Help,
            "send" => Self::Send(rest.to_owned()),
            "spam" => {
                let (count, text) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
                let count = count.parse().context("usage: spam <count> <text>")?;
                Self::Spam(count, text.trim_start().to_owned())
            }
            "addReacji" => Self::AddReacji(rest.to_owned()),
            "delReacji" => Self::DelReacji(rest.to_owned()),
            "reconnect" => {
                let secs = if rest.is_empty() {
                    1
                } else {
                    rest.parse().context("usage: reconnect [secs]")?
                };
                Self::Reconnect(Duration::from_secs(secs))
            }
            "crash" => Self::Crash,
            "status" => Self::Status,
            "log" => {
                let mut args = rest.split_whitespace();
                let level = args
                    .next()
                    .map(LevelFilter::from_str)
                    .transpose()
                    .context("usage: log [off|error|warn|info|debug|trace] [target]")?;
                Self::Log(level, args.next().map(str::to_owned))
            }
            "truncate" => Self::Truncate,
            "reset" => Self::Reset,
            "db" => Self::Db,
            "quit" | "exit" => Self::Quit,
            other => bail!("Command not found: {other:?} (try help)"),
        })
    }
}

pub struct Context<'a> {
    pub harness: &'a HarnessHandle,
    pub log: Option<&'a LogControl>,
}

impl Command {
    pub async fn execute(self, ctx: &Context<'_>) -> AResult<String> {
        let harness = ctx.harness;
        let server = &harness.server;
        let mut out = String::new();
        match self {
            Self::Help => out.push_str(&help()),
            Self::Send(text) => server.send_event(events::message(&text))?,
            Self::Spam(count, text) => {
                let event = events::message(&text);
                for _ in 0..count {
                    server.send_event(event.clone())?;
                }
            }
            Self::AddReacji(name) => server.send_event(events::reaction_added(&name))?,
            Self::DelReacji(name) => server.send_event(events::reaction_removed(&name))?,
            Self::Reconnect(durt) => {
                write!(out, "Triggering websocket reconnect - down for {durt:?}")?;
                server.net_disconnect(durt)?;
            }
            Self::Crash => {
                // During a normal failure the quota usually won't kick in. However
                // this isto specifically test a slack crash-loop to force test the quota
                out.push_str("Triggering a slack crashloop");
                server.crash_loop()?;
            }
            Self::Status => write!(out, "{}", harness.monitor.status())?,
            Self::Log(level, target) => {
                let log = ctx
                    .log
                    .context("Log levels can't be changed in this frontend")?;
                match (level, target) {
                    (None, _) => (),
                    (Some(level), None) => log.set_default(level),
                    (Some(level), Some(target)) => log.set_target(&target, level),
                }
                write!(out, "log levels: {}", log.describe())?;
            }
            Self::Truncate => {
                harness.db.truncate().await?;
                out.push_str("Truncated all tables");
            }
            Self::Reset => {
                harness.db.reset().await?;
                out.push_str("Schema reset");
            }
            Self::Db => write!(out, "psql {}", harness.db.url)?,
            Self::Quit => harness.watcher.cancel(),
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_is_verbatim() {
        assert_eq!(
            "send   \"foo bar\"++  baz".parse::<Command>().unwrap(),
            Command::Send("\"foo bar\"++  baz".to_owned())
        );
    }

    #[test]
    fn spam() {
        assert_eq!(
            "spam 3 a++".parse::<Command>().unwrap(),
            Command::Spam(3, "a++".to_owned())
        );
        assert!("spam".parse::<Command>().is_err());
        assert!("spam x a++".parse::<Command>().is_err());
    }

    #[test]
    fn reconnect() {
        assert_eq!(
            "reconnect".parse::<Command>().unwrap(),
            Command::Reconnect(Duration::from_secs(1))
        );
        assert_eq!(
            "reconnect 5".parse::<Command>().unwrap(),
            Command::Reconnect(Duration::from_secs(5))
        );
        assert!("reconnect abc".parse::<Command>().is_err());
    }

    #[test]
    fn log() {
        assert_eq!("log".parse::<Command>().unwrap(), Command::Log(None, None));
        assert_eq!(
            "log debug kcore".parse::<Command>().unwrap(),
            Command::Log(Some(LevelFilter::Debug), Some("kcore".to_owned()))
        );
        assert!("log loud".parse::<Command>().is_err());
    }

    #[test]
    fn help_and_completion_cover_parser() {
        for spec in COMMANDS {
            assert!(
                !matches!(spec.name.parse::<Command>(), Err(e) if e.to_string().contains("not found")),
                "{} in COMMANDS but not parsed",
                spec.name
            );
        }
        assert_eq!(complete("re").collect::<Vec<_>>(), ["reconnect", "reset"]);
    }

    #[test]
    fn unknown() {
        assert!("nope".parse::<Command>().is_err());
    }
}
