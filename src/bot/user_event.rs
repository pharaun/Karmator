use tokio::sync::mpsc;

use deadpool_postgres::Pool;

use std::result::Result;
use std::str::FromStr as _;

use anyhow::anyhow;
use anyhow::Result as AResult;

use log::{error, info, warn};

use serde::Deserialize;

use crate::query::sanitizer;

use crate::bot::build;
use crate::bot::tz::timezone;
use crate::query::karma::add_karma;
use crate::query::partial::partial;
use crate::query::ranking::ranking;
use crate::query::reacji::add_reacji;
use crate::query::top_n::top_n;
use crate::query::{KarmaCol, KarmaTyp, OrdQuery, ReacjiAction};

use kcore::parse_command;
use kcore::send_text_message;
use kcore::Command;
use kcore::Reply;
use kcore::SlackClient;
use kcore::SlackReply;
use kcore::SlackSender;
use kcore::SlackTimezone;
use kcore::SlackUser;

#[derive(Clone)]
pub struct Event<S: SlackSender> {
    // Bot Data
    slack: SlackClient<S>,
    tx: mpsc::Sender<Reply>,

    // User Data
    pub channel_id: String,
    pub user_id: String,
    cached_user: Option<SlackUser>,
    pub thread_ts: Option<String>,
    text: Option<String>,
}

impl<S: SlackSender> Event<S> {
    async fn new(
        slack: SlackClient<S>,
        tx: mpsc::Sender<Reply>,
        channel_id: String,
        user_id: String,
        thread_ts: Option<String>,
        text: Option<String>,
    ) -> AResult<Self> {
        let cached_user = slack
            .get_user(&user_id)
            .await
            .map_err(|err| anyhow!("Api error when querying for {user_id:?} - {err:?}"))?;

        Ok(Self {
            slack,
            tx,
            channel_id,
            user_id,
            cached_user,
            thread_ts,
            text,
        })
    }

    fn parse_command(&self) -> Result<Command<'_>, String> {
        match &self.text {
            None => Err("Empty input".to_owned()),
            Some(text) => parse_command(text),
        }
    }

    fn user(&self) -> Option<&SlackUser> {
        if self.cached_user.is_none() {
            error!("User probs was deleted or deactivated: {:?}", self.user_id);
        }
        self.cached_user.as_ref()
    }

    pub fn is_user_bot(&self) -> bool {
        self.user().is_none_or(|u| u.is_bot)
    }

    pub fn get_username(&self) -> Option<String> {
        self.user().map(|u| u.username.clone())
    }

    pub fn get_user_real_name(&self) -> Option<String> {
        self.user().map(|u| u.real_name.clone())
    }

    pub fn get_user_tz(&self) -> Option<SlackTimezone> {
        self.user().map(|u| u.timezone.clone())
    }

    pub async fn get_other_username(&self, user_id: &str) -> Option<String> {
        match self.slack.get_user(user_id).await {
            Ok(un) => un.map(|u| u.username),
            Err(e) => {
                error!("Api error when querying for user: {user_id:?} - {e:?}");

                // Bail out
                None
            }
        }
    }

    pub async fn get_other_user_real_name(&self, user_id: &str) -> Option<String> {
        match self.slack.get_user(user_id).await {
            Ok(rn) => rn.map(|u| u.real_name),
            Err(e) => {
                error!("Api error when querying for user: {user_id:?} - {e:?}");

                // Bail out
                None
            }
        }
    }

    // TODO: to support an existing bug, this will return the message's owner user_id
    pub async fn get_message(&mut self) -> Result<Option<String>, String> {
        match &self.thread_ts {
            None => Err("No timestamp set for reacji event!".to_owned()),
            Some(ts) => match self.slack.get_message(&self.channel_id, ts).await {
                Ok(Some(SlackReply::Message {
                    text,
                    user_id: Some(user_id),
                    bot_id: _,
                })) => {
                    self.text = Some(text);
                    Ok(Some(user_id))
                }
                Ok(Some(SlackReply::Message {
                    text: _,
                    user_id: None,
                    bot_id: Some(_),
                })) => {
                    info!("This message was posted by a bot");
                    Ok(None)
                }
                Ok(Some(SlackReply::Message {
                    text: _,
                    user_id: None,
                    bot_id: None,
                })) => Err("Slack failed to give a owner for this message".to_owned()),
                e => Err(format!("ERROR: [User Event] IDK here: {e:?}")),
            },
        }
    }

    pub async fn send_reply(&self, text: &str) {
        if let Err(e) = send_text_message(
            &self.tx,
            self.channel_id.clone(),
            self.thread_ts.clone(),
            format!("<@{}>: {}", &self.user_id, &text),
        )
        .await {
            error!("Failed to send message to slack: {e:?}");
        }
    }

    pub async fn sanitize(&self) -> String {
        match &self.text {
            None => String::new(),
            Some(text) => sanitizer(text, &self.slack).await,
        }
    }

    pub fn text(&self) -> String {
        self.text.clone().unwrap_or_default()
    }
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum UserEvent {
    Message {
        subtype: Option<String>,
        hidden: Option<bool>,
        #[serde(rename = "channel")]
        channel_id: Option<String>,
        #[serde(rename = "user")]
        user_id: Option<String>,
        text: Option<String>,
        ts: String,
        thread_ts: Option<String>,
    },
    ReactionAdded {
        /* who performed this */
        #[serde(rename = "user")]
        user_id: String,
        reaction: String,
        item_user: Option<String>,
        item: ReactionItem,
        event_ts: String,
    },
    ReactionRemoved {
        /* who performed this */
        #[serde(rename = "user")]
        user_id: String,
        reaction: String,
        item_user: Option<String>,
        item: ReactionItem,
        event_ts: String,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ReactionItem {
    Message {
        // I think this is mandatory?
        #[serde(rename = "channel")]
        channel_id: String,
        ts: String,
    },
    File {
        file: String,
    },
    FileComment {
        file: String,
        file_comment: String,
    },
}

fn parse_user_event(s: serde_json::Value) -> Option<UserEvent> {
    let res = serde_json::from_value::<UserEvent>(s).map_err(|e| format!("{e:?}"));

    if res.is_err() {
        warn!("parse_event - Error: {res:?}");
    }
    res.ok()
}

pub async fn process_user_message<S: SlackSender>(
    msg: serde_json::Value,
    slack: SlackClient<S>,
    tx: mpsc::Sender<Reply>,
    pool: &Pool,
) -> AResult<()> {
    // Check if its a message/certain string, if so, reply
    match parse_user_event(msg) {
        Some(UserEvent::Message {
            channel_id: Some(channel_id),
            subtype: None,
            text: Some(text),
            user_id: Some(user_id),
            thread_ts,
            hidden: _,
            ts: _,
        }) => {
            let event = Event::new(slack, tx, channel_id, user_id, thread_ts, Some(text)).await?;

            // Check if user is a bot, if so ignore (this applies to myself as well)
            if event.is_user_bot() {
                return Ok(());
            }

            // Check if text message field is empty, if so (ignore)
            if event.text().is_empty() {
                return Ok(());
            }

            let client = pool.get().await?;

            // Parse string to see if its a command one
            match event.parse_command() {
                Ok(Command("version", _)) => {
                    let ver = build::PKG_VERSION;
                    let dat = build::BUILT_TIME_UTC;
                    let sha = build::GIT_COMMIT_HASH.unwrap_or("Unknown");

                    event
                        .send_reply(&format!(
                            "Cargo Version: {ver} - Build Date: {dat}, - Build SHA: {sha}"
                        ))
                        .await;
                }

                Ok(Command("PING", _)) => {
                    event.send_reply("PONG").await;
                }

                Ok(Command("help", _)) => {
                    let help = "Available commands: !version !github !sidevotes !karma !givers !rank !ranksidevote !topkarma !topgivers !topsidevotes !tz";
                    event.send_reply(help).await;
                }

                Ok(Command("github", _)) => {
                    let github = "Github repo: https://github.com/pharaun/Karmator";
                    event.send_reply(github).await;
                }

                Ok(Command("tz", arg)) => {
                    timezone(&event, arg).await;
                }

                // asdf「asdf」asdf
                // TODO: Implement a 'slack id' to unix name mapper
                //  - for now can probs query it as needed, but should
                //      be cached
                Ok(Command(c @ ("karma" | "givers" | "sidevotes"), arg)) => {
                    if arg.is_empty() {
                        match c {
                            "karma" => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Received,
                                    OrdQuery::Desc,
                                    KarmaCol::Received,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("highest karma", "lowest karma"),
                                    3u32,
                                )
                                .await;
                            }
                            "givers" => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("most positive", "most negative"),
                                    3u32,
                                )
                                .await;
                            }
                            "sidevotes" => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Received,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaTyp::Side,
                                    ("most sidevotes received", "most sidevotes given"),
                                    3u32,
                                )
                                .await;
                            }
                            _ => (),
                        }
                    } else {
                        match c {
                            "karma" => {
                                partial(&event, &client, KarmaCol::Received, arg).await;
                            }
                            "givers" => {
                                partial(&event, &client, KarmaCol::Given, arg).await;
                            }
                            "sidevotes" => event.send_reply("Not supported!").await,
                            _ => (),
                        }
                    }
                }

                Ok(Command(c @ ("topkarma" | "topgivers" | "topsidevotes"), arg)) => {
                    if arg.len() == 1 {
                        // Parse the argument
                        let limit = u32::from_str(arg.first().unwrap_or(&"1"));

                        match (c, limit) {
                            ("topkarma", Ok(lim @ 1..=25)) => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Received,
                                    OrdQuery::Desc,
                                    KarmaCol::Received,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nhighest karma", "\nlowest karma"),
                                    lim,
                                )
                                .await;
                            }
                            ("topgivers", Ok(lim @ 1..=25)) => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nmost positive", "\nmost negative"),
                                    lim,
                                )
                                .await;
                            }
                            ("topsidevotes", Ok(lim @ 1..=25)) => {
                                top_n(
                                    &event,
                                    &client,
                                    KarmaCol::Received,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaTyp::Side,
                                    ("\nmost sidevotes received", "\nmost sidevotes given"),
                                    lim,
                                )
                                .await;
                            }
                            _ => {
                                event
                                    .send_reply(
                                        "Please specify a positive integer between 1 and 25.",
                                    )
                                    .await;
                            }
                        }
                    } else {
                        event
                            .send_reply("Please specify a positive integer between 1 and 25.")
                            .await;
                    }
                }

                Ok(Command(c @ ("rank" | "ranksidevote"), arg)) => {
                    let t_typ = if c == "rank" {
                        KarmaTyp::Total
                    } else {
                        KarmaTyp::Side
                    };
                    if arg.is_empty() {
                        // Rank with yourself
                        match event.get_username() {
                            Some(ud) => {
                                ranking(&event, &client, t_typ, &ud, "Your").await;
                            }
                            _ => {
                                event
                                    .send_reply("Cant find your display name, thanks slack")
                                    .await;
                            }
                        }
                    } else if arg.len() == 1 {
                        // Rank up with one target
                        let target = arg.first().unwrap_or(&"INVALID");
                        ranking(&event, &client, t_typ, target, target).await;
                    } else {
                        event
                            .send_reply("Can only rank one karma entry at a time!")
                            .await;
                    }
                }

                Ok(Command(x, _)) => info!("No handler: {x:?}"),

                Err(_) => add_karma(&event, pool).await,
            }
        }

        Some(UserEvent::ReactionAdded {
            user_id,
            reaction,
            item: ReactionItem::Message { channel_id, ts },
            ..
        }) => {
            let mut event = Event::new(slack, tx, channel_id, user_id, Some(ts), None).await?;

            add_reacji(&mut event, pool, &reaction, ReacjiAction::Add).await?;
        }

        Some(UserEvent::ReactionRemoved {
            user_id,
            reaction,
            item: ReactionItem::Message { channel_id, ts },
            ..
        }) => {
            let mut event = Event::new(slack, tx, channel_id, user_id, Some(ts), None).await?;

            add_reacji(&mut event, pool, &reaction, ReacjiAction::Del).await?;
        }

        // TODO: improve error logging to log unhandled events or errors in parsing here
        _ => (),
    }
    Ok(())
}
