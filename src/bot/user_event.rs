use tokio::sync::mpsc;
use tokio::sync::RwLock;

use tokio_postgres::Client;

use std::result::Result;
use std::str::FromStr;
use std::sync::Arc;

use anyhow::Result as AResult;

use log::{info, error};

use serde::Deserialize;

use crate::query::santizer;

use crate::query::karma::add_karma;
use crate::query::partial::partial;
use crate::query::ranking::ranking;
use crate::query::reacji::add_reacji;
use crate::query::top_n::top_n;
use crate::query::{KarmaCol, KarmaTyp, OrdQuery, ReacjiAction};
use crate::bot::tz::timezone;

use kcore::slack;
use kcore::command;
use kcore::event::Reply;
use kcore::event::send_simple_message;


#[derive(Clone)]
pub struct Event<S>
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    // Bot Data
    slack: slack::Client<S>,
    tx: mpsc::Sender<Reply>,

    // User Data
    pub channel_id: String,
    pub user_id: String,
    pub thread_ts: Option<String>,
    text: Option<String>,
}

impl <S> Event<S>
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    async fn is_user_bot(&self) -> bool {
        match self.slack.is_user_bot(&self.user_id).await {
            Ok(Some(is_bot)) => is_bot,
            Ok(None) => {
                error!("No info on user: {:?}", &self.user_id);

                // Bail out
                true
            },
            Err(e) => {
                error!("Api error when querying for user: {:?} - {:?}", &self.user_id, e);

                // Bail out
                true
            },
        }
    }

    fn parse_command(&self) -> Result<command::Command, String> {
        match &self.text {
            None => Err("Empty input".to_string()),
            Some(text) => command::parse(text),
        }
    }

    pub async fn get_user_tz(&self) -> Option<slack::Timezone> {
        match self.slack.get_user_tz(&self.user_id).await {
            Ok(tz) => tz,
            Err(e) => {
                error!("Api error when querying for user: {:?} - {:?}", &self.user_id, e);

                // Bail out
                None
            },
        }
    }

    pub async fn get_username(&self) -> Option<String> {
        self.get_other_username(&self.user_id).await
    }

    pub async fn get_other_username(&self, user_id: &str) -> Option<String> {
        match self.slack.get_username(user_id).await {
            Ok(un) => un,
            Err(e) => {
                error!("Api error when querying for user: {:?} - {:?}", user_id, e);

                // Bail out
                None
            },
        }
    }

    pub async fn get_user_real_name(&self) -> Option<String> {
        self.get_other_user_real_name(&self.user_id).await
    }

    pub async fn get_other_user_real_name(&self, user_id: &str) -> Option<String> {
        match self.slack.get_user_real_name(user_id).await {
            Ok(rn) => rn,
            Err(e) => {
                error!("Api error when querying for user: {:?} - {:?}", user_id, e);

                // Bail out
                None
            },
        }
    }

    // TODO: to support an existing bug, this will return the message's owner user_id
    pub async fn get_message(&mut self) -> Result<String, String> {
        match &self.thread_ts {
            None => Err("No timestamp set for reacji event!".to_string()),
            Some(ts) => {
                match self.slack.get_message(&self.channel_id, &ts).await {
                    Ok(Some(slack::ConversationHistoryMessage::Message { text, user_id: Some(user_id) })) => {
                        self.text = Some(text);
                        Ok(user_id)
                    },
                    Ok(Some(slack::ConversationHistoryMessage::Message { text: _, user_id: None })) => {
                        Err("Slack failed to give a owner for this message".to_string())
                    },
                    e => Err(format!("ERROR: [User Event] IDK here: {:?}", e)),
                }
            },
        }
    }

    // TODO: maybe better to consume the event?
    pub async fn send_reply(&mut self, text: &str) {
        // TODO: log the error
        let _ = send_simple_message(
            &mut self.tx,
            self.channel_id.clone(),
            self.thread_ts.clone(),
            format!("<@{}>: {}", &self.user_id, &text)
        ).await;
    }

    pub async fn santize(&self) -> String {
        match &self.text {
            None => "".to_string(),
            Some(text) => santizer(text, &self.slack).await,
        }
    }

    pub fn text(&self) -> String {
        self.text.clone().unwrap_or("".to_string())
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
    let res = serde_json::from_value::<UserEvent>(
        s.clone()
    ).map_err(
        |x| format!("{:?}", x)
    );

    if res.is_err() {
        error!("parse_event - Error: {:?}\n{:?}\n", res, s);
    }
    res.ok()
}


pub async fn process_user_message<S>(
    msg: serde_json::Value,
    tx: mpsc::Sender<Reply>,
    client: Arc<RwLock<Client>>,
    slack: slack::Client<S>,
) -> AResult<()>
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
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
            let mut event = Event {
                // Bot data
                slack, tx,
                // User data
                channel_id, user_id, thread_ts, text: Some(text)
            };

            // Check if user is a bot, if so ignore (this applies to myself as well)
            if event.is_user_bot().await {
                return Ok(());
            }

            // Check if text message field is empty, if so (ignore)
            if event.text().is_empty() {
                return Ok(());
            }

            // Parse string to see if its a command one
            match event.parse_command() {
                Ok(command::Command("version", _)) => {
                    let ver = crate::bot::PKG_VERSION;
                    let dat = crate::bot::BUILT_TIME_UTC;
                    let sha = crate::bot::GIT_COMMIT_HASH.unwrap_or("Unknown");

                    event.send_reply(
                        &format!("Cargo Version: {} - Build Date: {}, - Build SHA: {}", ver, dat, sha)
                    ).await;
                },

                Ok(command::Command("PING", _)) => {
                    event.send_reply("PONG").await;
                },

                Ok(command::Command("help", _)) => {
                    let help = "Available commands: !version !github !sidevotes !karma !givers !rank !ranksidevote !topkarma !topgivers !topsidevotes !tz";
                    event.send_reply(help).await;
                },

                Ok(command::Command("github", _)) => {
                    let github = "Github repo: https://github.com/pharaun/Karmator";
                    event.send_reply(github).await;
                },

                Ok(command::Command("tz", arg)) => {
                    timezone(&mut event.clone(), arg).await;
                },

                // asdf「asdf」asdf
                // TODO: Implement a 'slack id' to unix name mapper
                //  - for now can probs query it as needed, but should
                //      be cached
                Ok(command::Command(c@"karma", arg)) |
                Ok(command::Command(c@"givers", arg)) |
                Ok(command::Command(c@"sidevotes", arg)) => {
                    let client = client.read().await;
                    if arg.is_empty() {
                        match c {
                            "karma" => top_n(
                                &mut event.clone(), &*client,
                                KarmaCol::Received, OrdQuery::Desc,
                                KarmaCol::Received, OrdQuery::Asc,
                                KarmaTyp::Total,
                                ("highest karma", "lowest karma"),
                                3u32,
                            ).await,
                            "givers" => top_n(
                                &mut event.clone(), &*client,
                                KarmaCol::Given, OrdQuery::Desc,
                                KarmaCol::Given, OrdQuery::Asc,
                                KarmaTyp::Total,
                                ("most positive", "most negative"),
                                3u32,
                            ).await,
                            "sidevotes" => top_n(
                                &mut event.clone(), &*client,
                                KarmaCol::Received, OrdQuery::Desc,
                                KarmaCol::Given, OrdQuery::Desc,
                                KarmaTyp::Side,
                                ("most sidevotes received", "most sidevotes given"),
                                3u32,
                            ).await,
                            _ => (),
                        }
                    } else {
                        match c {
                            "karma" => partial(
                                &mut event.clone(), &*client,
                                KarmaCol::Received,
                                arg,
                            ).await,
                            "givers" => partial(
                                &mut event.clone(), &*client,
                                KarmaCol::Given,
                                arg,
                            ).await,
                            "sidevotes" => event.send_reply("Not supported!").await,
                            _ => (),
                        }
                    }
                },

                Ok(command::Command(c@"topkarma", arg)) |
                Ok(command::Command(c@"topgivers", arg)) |
                Ok(command::Command(c@"topsidevotes", arg)) => {
                    if arg.is_empty() {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else if arg.len() != 1 {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        let client = client.read().await;
                        match (c, limit) {
                            ("topkarma", Ok(lim@1..=25)) => {
                                top_n(
                                    &mut event.clone(), &*client,
                                    KarmaCol::Received, OrdQuery::Desc,
                                    KarmaCol::Received, OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nhighest karma", "\nlowest karma"),
                                    lim,
                                ).await;
                            },
                            ("topgivers", Ok(lim@1..=25)) => {
                                top_n(
                                    &mut event.clone(), &*client,
                                    KarmaCol::Given, OrdQuery::Desc,
                                    KarmaCol::Given, OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nmost positive", "\nmost negative"),
                                    lim,
                                ).await;
                            },
                            ("topsidevotes", Ok(lim@1..=25)) => {
                                top_n(
                                    &mut event.clone(), &*client,
                                    KarmaCol::Received, OrdQuery::Desc,
                                    KarmaCol::Given, OrdQuery::Desc,
                                    KarmaTyp::Side,
                                    ("\nmost sidevotes received", "\nmost sidevotes given"),
                                    lim,
                                ).await;
                            },
                            _ => {
                                event.send_reply("Please specify a positive integer between 1 and 25.").await;
                            },
                        }
                    }
                },

                Ok(command::Command(c@"rank", arg)) |
                Ok(command::Command(c@"ranksidevote", arg)) => {
                    let t_typ = if c == "rank" {KarmaTyp::Total} else {KarmaTyp::Side};
                    if arg.is_empty() {
                        // Rank with yourself
                        let client = client.read().await;
                        match event.get_username().await {
                            Some(ud) => {
                                ranking(
                                    &mut event.clone(), &*client,
                                    t_typ,
                                    &ud,
                                    "Your",
                                ).await;
                            },
                            _ => {
                                event.send_reply("Cant find your display name, thanks slack").await;
                            },
                        }
                    } else if arg.len() == 1 {
                        // Rank up with one target
                        let target = arg.get(0).unwrap_or(&"INVALID");
                        let client = client.read().await;
                        ranking(
                            &mut event.clone(), &*client,
                            t_typ,
                            target,
                            &format!("{}", target),
                        ).await;
                    } else {
                        event.send_reply("Can only rank one karma entry at a time!").await;
                    }
                },

                Ok(command::Command(x, _)) => info!("No handler: {:?}", x),

                Err(_) => add_karma(&event, client.clone()).await,
            }
        },

        Some(UserEvent::ReactionAdded {user_id, reaction, item: ReactionItem::Message {channel_id, ts}, ..}) => {
            let mut event = Event {
                // Bot data
                slack, tx,
                // User data
                channel_id, user_id, thread_ts: Some(ts), text: None
            };

            add_reacji(
                &mut event, client.clone(),
                &reaction,
                ReacjiAction::Add,
            ).await;
        },

        Some(UserEvent::ReactionRemoved {user_id, reaction, item: ReactionItem::Message {channel_id, ts}, ..}) => {
            let mut event = Event {
                // Bot data
                slack, tx,
                // User data
                channel_id, user_id, thread_ts: Some(ts), text: None
            };

            add_reacji(
                &mut event, client.clone(),
                &reaction,
                ReacjiAction::Del,
            ).await;
        },

        // TODO: improve error logging to log unhandled events or errors in parsing here
        _ => (),
    }
    Ok(())
}
