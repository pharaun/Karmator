use tokio::sync::mpsc;

use chrono::prelude::{Utc, DateTime};
use humantime::format_duration;

use std::result::Result;
use std::str::FromStr;
use std::time::Duration;

use crate::bot::build_info;
use crate::bot::query::santizer;

use crate::bot::query::karma::add_karma;
use crate::bot::query::partial::partial;
use crate::bot::query::ranking::ranking;
use crate::bot::query::reacji::add_reacji;
use crate::bot::query::top_n::top_n;
use crate::bot::query::{KarmaCol, KarmaTyp, OrdQuery, ReacjiAction};
use crate::bot::tz::timezone;

use crate::core::cache;
use crate::core::command;
use crate::core::database::Query;

use crate::core::event::ReactionItem;
use crate::core::event::UserEvent;
use crate::core::event::Reply;
use crate::core::event::send_simple_message;


#[derive(Clone)]
pub struct Event {
    // Bot Data
    cache: cache::Cache,
    tx: mpsc::Sender<Reply>,

    // User Data
    pub channel_id: String,
    pub user_id: String,
    pub thread_ts: Option<String>,
    text: Option<String>,
}

impl Event {
    async fn is_user_bot(&self) -> bool {
        match self.cache.is_user_bot(&self.user_id).await {
            None => {
                eprintln!("ERROR [User Event]: No info on user: {:?}", &self.user_id);

                // Bail out
                true
            },
            Some(is_bot) => is_bot,
        }
    }

    fn parse_command(&self) -> Result<command::Command, String> {
        match &self.text {
            None => Err("Empty input".to_string()),
            Some(text) => command::parse(text),
        }
    }

    pub async fn get_user_tz(&self) -> Option<cache::Timezone> {
        self.cache.get_user_tz(&self.user_id).await
    }

    pub async fn get_username(&self) -> Option<String> {
        self.get_other_username(&self.user_id).await
    }

    pub async fn get_other_username(&self, user_id: &str) -> Option<String> {
        self.cache.get_username(user_id).await
    }

    pub async fn get_user_real_name(&self) -> Option<String> {
        self.get_other_user_real_name(&self.user_id).await
    }

    pub async fn get_other_user_real_name(&self, user_id: &str) -> Option<String> {
        self.cache.get_user_real_name(user_id).await
    }

    // TODO: to support an existing bug, this will return the message's owner user_id
    pub async fn get_message(&mut self) -> Result<String, String> {
        match &self.thread_ts {
            None => Err("No timestamp set for reacji event!".to_string()),
            Some(ts) => {
                match self.cache.get_message(&self.channel_id, &ts).await {
                    Some(cache::ConversationHistoryMessage::Message { text, user_id: Some(user_id) }) => {
                        self.text = Some(text);
                        Ok(user_id)
                    },
                    Some(cache::ConversationHistoryMessage::Message { text: _, user_id: None }) => {
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
            Some(text) => santizer(text, &self.cache).await,
        }
    }

    pub fn text(&self) -> String {
        self.text.clone().unwrap_or("".to_string())
    }
}


pub async fn process_user_message(
    msg: UserEvent,
    tx: mpsc::Sender<Reply>,
    mut sql_tx: mpsc::Sender<Query>,
    start_time: DateTime<Utc>,
    cache: cache::Cache,
) -> Result<(), Box<dyn std::error::Error>> {
    // Check if its a message/certain string, if so, reply
    match msg {
        UserEvent::Message {
            channel_id: Some(channel_id),
            subtype: None,
            text: Some(text),
            user_id: Some(user_id),
            thread_ts,
            hidden: _,
            ts: _,
        } => {
            let mut event = Event {
                // Bot data
                cache, tx,
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
                    let ver = build_info::PKG_VERSION;
                    let dat = build_info::BUILT_TIME_UTC;
                    let sha = build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

                    event.send_reply(
                        &format!("Cargo Version: {} - Build Date: {}, - Build SHA: {}", ver, dat, sha)
                    ).await;
                },

                Ok(command::Command("uptime", _)) => {
                    let end_time: DateTime<Utc> = Utc::now();
                    let durt = end_time.signed_duration_since(start_time).to_std().unwrap_or(
                        Duration::from_secs(3122064000));

                    event.send_reply(&format_duration(durt).to_string()).await;
                },

                Ok(command::Command("help", _)) => {
                    let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote !topkarma !topgivers !topsidevotes !tz";
                    event.send_reply(help).await;
                },

                Ok(command::Command("github", _)) => {
                    let github = "Github repo: https://github.com/pharaun/Karmator";
                    event.send_reply(github).await;
                },

                Ok(command::Command("tz", arg)) => {
                    timezone(&mut event.clone(), arg).await;
                },

                Ok(command::Command("karma", arg)) => {
                    // asdf「asdf」asdf
                    // TODO: Implement a 'slack id' to unix name mapper
                    //  - for now can probs query it as needed, but should
                    //      be cached
                    if arg.is_empty() {
                        top_n(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaCol::Recieved,
                            OrdQuery::Desc,
                            KarmaCol::Recieved,
                            OrdQuery::Asc,
                            KarmaTyp::Total,
                            ("highest karma", "lowest karma"),
                            3u32,
                        ).await;
                    } else {
                        partial(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaCol::Recieved,
                            arg,
                        ).await;
                    }
                },

                Ok(command::Command("topkarma", arg)) => {
                    if arg.is_empty() {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else if arg.len() != 1 {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    &mut event.clone(),
                                    &mut sql_tx,
                                    KarmaCol::Recieved,
                                    OrdQuery::Desc,
                                    KarmaCol::Recieved,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nhighest karma", "\nlowest karma"),
                                    lim,
                                ).await;
                            },
                            _ => {
                                event.send_reply("Please specify a positive integer between 1 and 25.").await;
                            },
                        }
                    }
                },

                Ok(command::Command("givers", arg)) => {
                    if arg.is_empty() {
                        top_n(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaCol::Given,
                            OrdQuery::Desc,
                            KarmaCol::Given,
                            OrdQuery::Asc,
                            KarmaTyp::Total,
                            ("most positive", "most negative"),
                            3u32,
                        ).await;
                    } else {
                        partial(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaCol::Given,
                            arg,
                        ).await;
                    }
                },

                Ok(command::Command("topgivers", arg)) => {
                    if arg.is_empty() {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else if arg.len() != 1 {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    &mut event.clone(),
                                    &mut sql_tx,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Asc,
                                    KarmaTyp::Total,
                                    ("\nmost positive", "\nmost negative"),
                                    lim,
                                ).await;
                            },
                            _ => {
                                event.send_reply("Please specify a positive integer between 1 and 25.").await;
                            },
                        }
                    }
                },

                Ok(command::Command("sidevotes", arg)) => {
                    if arg.is_empty() {
                        top_n(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaCol::Recieved,
                            OrdQuery::Desc,
                            KarmaCol::Given,
                            OrdQuery::Desc,
                            KarmaTyp::Side,
                            ("most sidevotes recieved", "most sidevotes given"),
                            3u32,
                        ).await;
                    } else {
                        event.send_reply("Not supported!").await;
                    }
                },

                Ok(command::Command("topsidevotes", arg)) => {
                    if arg.is_empty() {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else if arg.len() != 1 {
                        event.send_reply("Please specify a positive integer between 1 and 25.").await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    &mut event.clone(),
                                    &mut sql_tx,
                                    KarmaCol::Recieved,
                                    OrdQuery::Desc,
                                    KarmaCol::Given,
                                    OrdQuery::Desc,
                                    KarmaTyp::Side,
                                    ("\nmost sidevotes recieved", "\nmost sidevotes given"),
                                    lim,
                                ).await;
                            },
                            _ => {
                                event.send_reply("Please specify a positive integer between 1 and 25.").await;
                            },
                        }
                    }
                },

                Ok(command::Command("rank", arg)) => {
                    if arg.is_empty() {
                        // Rank with yourself
                        match event.get_username().await {
                            Some(ud) => {
                                ranking(
                                    &mut event.clone(),
                                    &mut sql_tx,
                                    KarmaTyp::Total,
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

                        ranking(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaTyp::Total,
                            target,
                            &format!("{}", target),
                        ).await;
                    } else {
                        event.send_reply("Can only rank one karma entry at a time!").await;
                    }
                },

                Ok(command::Command("ranksidevote", arg)) => {
                    if arg.is_empty() {
                        // Rank with yourself
                        match event.get_username().await {
                            Some(ud) => {
                                ranking(
                                    &mut event.clone(),
                                    &mut sql_tx,
                                    KarmaTyp::Side,
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

                        ranking(
                            &mut event.clone(),
                            &mut sql_tx,
                            KarmaTyp::Side,
                            target,
                            &format!("{}", target),
                        ).await;
                    } else {
                        event.send_reply("Can only rank one karma entry at a time!").await;
                    }
                },

                Ok(command::Command(x, _)) => println!("INFO [User Event]: No handler: {:?}", x),

                Err(_) => add_karma(&mut sql_tx, &event).await,
            }
        },

        UserEvent::ReactionAdded {
            user_id, /* who performed this */
            reaction,
            item_user: _,
            item: ReactionItem::Message {
                channel_id,
                ts,
            },
            event_ts: _,
        } => {
            let mut event = Event {
                // Bot data
                cache, tx,
                // User data
                channel_id, user_id, thread_ts: Some(ts), text: None
            };

            add_reacji(
                &mut sql_tx,
                &mut event,
                &reaction,
                ReacjiAction::Add,
            ).await;
        },

        UserEvent::ReactionRemoved {
            user_id, /* who performed this */
            reaction,
            item_user: _,
            item: ReactionItem::Message {
                channel_id,
                ts,
            },
            event_ts: _,
        } => {
            let mut event = Event {
                // Bot data
                cache, tx,
                // User data
                channel_id, user_id, thread_ts: Some(ts), text: None
            };

            add_reacji(
                &mut sql_tx,
                &mut event,
                &reaction,
                ReacjiAction::Del,
            ).await;
        },

        _ => (),
    }
    Ok(())
}
