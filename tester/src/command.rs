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
    Spec { name: "help", args: "[command]", help: "list commands, or show usage for one" },
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

pub const SIGIL: char = '/';

pub fn help(command: Option<&str>) -> AResult<String> {
    let Some(name) = command else {
        let names: Vec<_> = COMMANDS.iter().map(|c| format!("{SIGIL}{}", c.name)).collect();
        return Ok(format!(
            "{} · {SIGIL}help <command> for usage",
            names.join(" ")
        ));
    };
    let name = name.trim_start_matches(SIGIL);
    let Some(c) = spec(name) else {
        bail!("unknown command {SIGIL}{name}");
    };
    Ok(format!("{SIGIL}{} {} - {}", c.name, c.args, c.help).replace("  ", " "))
}

fn spec(name: &str) -> Option<&'static Spec> {
    COMMANDS.iter().find(|c| c.name == name)
}

pub fn hint(line: &str) -> Option<String> {
    let rest = line.strip_prefix(SIGIL)?;
    let (name, args) = rest.split_once(' ').unwrap_or((rest, ""));
    let spec = spec(name)?;

    // A word still being typed counts as filled in
    let filled = args.split_whitespace().count();
    let remaining: Vec<_> = spec.args.split_whitespace().skip(filled).collect();
    if remaining.is_empty() {
        return None;
    }
    let sep = if line.ends_with(' ') { "" } else { " " };
    Some(format!("{sep}{}", remaining.join(" ")))
}

pub fn complete(line: &str) -> Option<String> {
    let prefix = line.strip_prefix(SIGIL)?;
    if prefix.contains(' ') || prefix.starts_with(SIGIL) {
        return None;
    }
    let mut matches = COMMANDS.iter().filter(|c| c.name.starts_with(prefix));
    match (matches.next(), matches.next()) {
        (Some(c), None) => Some(format!("{SIGIL}{} ", c.name)),
        _ => None,
    }
}

pub fn suggest(name: &str) -> Vec<&'static str> {
    let name = name.to_lowercase();
    COMMANDS
        .iter()
        .map(|c| c.name)
        .filter(|n| {
            let n = n.to_lowercase();
            !name.is_empty() && (n.starts_with(&name) || name.starts_with(&n))
        })
        .collect()
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Message(String),
    Help(Option<String>),
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

    fn from_str(line: &str) -> AResult<Self> {
        let Some(line) = line.strip_prefix(SIGIL) else {
            return Ok(Self::Message(line.to_owned()));
        };
        if line.starts_with(SIGIL) {
            return Ok(Self::Message(line.to_owned()));
        }
        let line = line.trim();
        let (cmd, rest) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let rest = rest.trim_start();

        Ok(match cmd {
            "help" | "?" => Self::Help(rest.split_whitespace().next().map(str::to_owned)),
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
            other => match suggest(other).as_slice() {
                [] => bail!("unknown command {SIGIL}{other} (try {SIGIL}help)"),
                m => bail!("unknown command {SIGIL}{other}, did you mean: {SIGIL}{}", m.join(", /")),
            },
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
            Self::Message(text) => server.send_event(events::message(&text))?,
            Self::Help(command) => out.push_str(&help(command.as_deref())?),
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

    fn parse(line: &str) -> Command {
        line.parse().unwrap()
    }

    #[test]
    fn plain_text_is_verbatim_message() {
        assert_eq!(
            parse("  \"foo bar\"++  baz"),
            Command::Message("  \"foo bar\"++  baz".to_owned())
        );
        assert_eq!(parse("!karma foo"), Command::Message("!karma foo".to_owned()));
        assert_eq!(parse("//tmp"), Command::Message("/tmp".to_owned()));
    }

    #[test]
    fn spam() {
        assert_eq!(parse("/spam 3 a++"), Command::Spam(3, "a++".to_owned()));
        assert!("/spam".parse::<Command>().is_err());
        assert!("/spam x a++".parse::<Command>().is_err());
    }

    #[test]
    fn reconnect() {
        assert_eq!(parse("/reconnect"), Command::Reconnect(Duration::from_secs(1)));
        assert_eq!(parse("/reconnect 5"), Command::Reconnect(Duration::from_secs(5)));
        assert!("/reconnect abc".parse::<Command>().is_err());
    }

    #[test]
    fn log() {
        assert_eq!(parse("/log"), Command::Log(None, None));
        assert_eq!(
            parse("/log debug kcore"),
            Command::Log(Some(LevelFilter::Debug), Some("kcore".to_owned()))
        );
        assert!("/log loud".parse::<Command>().is_err());
    }

    #[test]
    fn every_spec_parses() {
        for spec in COMMANDS {
            let res = format!("/{}", spec.name).parse::<Command>();
            assert!(
                !matches!(res, Err(e) if e.to_string().contains("unknown")),
                "{} in COMMANDS but not parsed",
                spec.name
            );
        }
    }

    #[test]
    fn help_usage() {
        assert_eq!(parse("/help"), Command::Help(None));
        assert_eq!(parse("/help spam"), Command::Help(Some("spam".to_owned())));
        assert!(help(None).unwrap().contains("/spam /addReacji"));
        assert_eq!(
            help(Some("/spam")).unwrap(),
            "/spam <count> <text> - send the message <count> times"
        );
        assert_eq!(help(Some("crash")).unwrap(), "/crash - put the websocket server into a slack crashloop");
        assert!(help(Some("nope")).is_err());
    }

    #[test]
    fn unknown_suggests() {
        let err = "/sp".parse::<Command>().unwrap_err().to_string();
        assert!(err.contains("did you mean: /spam"), "{err}");
    }

    #[test]
    fn hints() {
        assert_eq!(hint("/spam").as_deref(), Some(" <count> <text>"));
        assert_eq!(hint("/spam ").as_deref(), Some("<count> <text>"));
        assert_eq!(hint("/spam 1").as_deref(), Some(" <text>"));
        assert_eq!(hint("/spam 1 ").as_deref(), Some("<text>"));
        assert_eq!(hint("/spam 1 foo"), None);
        assert_eq!(hint("/crash"), None);
        assert_eq!(hint("/nope"), None);
        assert_eq!(hint("spam"), None);
    }

    #[test]
    fn completion() {
        assert_eq!(complete("/sp").as_deref(), Some("/spam "));
        assert_eq!(complete("/re"), None);
        assert_eq!(complete("/spam 1"), None);
        assert_eq!(complete("sp"), None);
    }
}
