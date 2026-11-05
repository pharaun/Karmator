use std::fmt::Write as _;
use std::str::FromStr;
use std::time::Duration;

use anyhow::Result as AResult;
use anyhow::{Context as _, bail};

use crate::events;
use crate::harness::HarnessHandle;

pub const HELP: &str = "\
help                 - this help
send <text>          - send a message to the bot
spam <count> <text>  - send the message <count> times
addReacji <name>     - send a reaction_added event
delReacji <name>     - send a reaction_removed event
reconnect [secs]     - drop the websocket and refuse connections for secs (default 1)
crash                - put the websocket server into a slack crashloop
truncate             - empty all tables
reset                - drop + reload the schema
db                   - print the psql url for the scratch database
quit                 - exit";

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Help,
    Send(String),
    Spam(u64, String),
    AddReacji(String),
    DelReacji(String),
    Reconnect(Duration),
    Crash,
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
            "truncate" => Self::Truncate,
            "reset" => Self::Reset,
            "db" => Self::Db,
            "quit" | "exit" => Self::Quit,
            other => bail!("Command not found: {other:?} (try help)"),
        })
    }
}

impl Command {
    pub async fn execute(self, harness: &HarnessHandle) -> AResult<String> {
        let server = &harness.server;
        let mut out = String::new();
        match self {
            Self::Help => out.push_str(HELP),
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
    fn unknown() {
        assert!("nope".parse::<Command>().is_err());
    }
}
