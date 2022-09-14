use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;

use chrono::prelude::{Utc, DateTime};
use humantime::format_duration;

use std::result::Result;
use std::str::FromStr;
use std::time::Duration;

use crate::bot::build_info;

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

use crate::core::event::MsgId;
use crate::core::event::ReactionItem;
use crate::core::event::UserEvent;
use crate::core::event::send_simple_message;


pub async fn process_user_message<R>(
    msg_id: MsgId,
    msg: UserEvent,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<Query>,
    start_time: DateTime<Utc>,
    cache: cache::Cache<R>,
) -> Result<(), Box<dyn std::error::Error>>
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    // Check if its a message/certain string, if so, reply
    match msg {
        UserEvent::Message {
            channel_id: Some(channel_id),
            subtype: None,
            text,
            user_id,
            thread_ts,
            hidden: _,
            ts: _,
        } => {
            // Check if user is a bot, if so ignore (this applies to myself as well)
            match cache.is_user_bot(&user_id).await {
                None => {
                    eprintln!("ERROR [User Event]: No info on user: {:?}", user_id);
                    return Ok(());
                },
                Some(is_bot) => {
                    if is_bot {
                        return Ok(());
                    }
                },
            }

            // Check if text message field is empty, if so (ignore)
            if text.is_empty() {
                return Ok(());
            }

            // Parse string to see if its a command one
            let res = command::parse(&text);

            match res {
                Ok(command::Command("version", _)) => {
                    let ver = build_info::PKG_VERSION;
                    let dat = build_info::BUILT_TIME_UTC;
                    let sha = build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

                    let _ = send_simple_message(
                        msg_id,
                        &mut tx,
                        channel_id,
                        thread_ts,
                        format!("<@{}>: Cargo Version: {} - Build Date: {}, - Build SHA: {}", user_id, ver, dat, sha),
                    ).await;
                },

                Ok(command::Command("uptime", _)) => {
                    let end_time: DateTime<Utc> = Utc::now();
                    let durt = end_time.signed_duration_since(start_time).to_std().unwrap_or(
                        Duration::from_secs(3122064000));

                    let _ = send_simple_message(
                        msg_id,
                        &mut tx,
                        channel_id,
                        thread_ts,
                        format!("<@{}>: {}", user_id, format_duration(durt).to_string()),
                    ).await;
                },

                Ok(command::Command("help", _)) => {
                    let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote !topkarma !topgivers !topsidevotes !tz";
                    let _ = send_simple_message(
                        msg_id,
                        &mut tx,
                        channel_id,
                        thread_ts,
                        format!("<@{}>: {}", user_id, help),
                    ).await;
                },

                Ok(command::Command("github", _)) => {
                    let github = "Github repo: https://github.com/pharaun/Karmator";

                    let _ = send_simple_message(
                        msg_id,
                        &mut tx,
                        channel_id,
                        thread_ts,
                        format!("<@{}>: {}", user_id, github),
                    ).await;
                },

                Ok(command::Command("tz", arg)) => {
                    timezone(
                        msg_id,
                        &mut tx,
                        &cache,
                        channel_id,
                        thread_ts,
                        user_id,
                        arg
                    ).await;
                },

                Ok(command::Command("karma", arg)) => {
                    // asdf「asdf」asdf
                    // TODO: Implement a 'slack id' to unix name mapper
                    //  - for now can probs query it as needed, but should
                    //      be cached
                    if arg.is_empty() {
                        top_n(
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
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
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
                            KarmaCol::Recieved,
                            arg,
                        ).await;
                    }
                },

                Ok(command::Command("topkarma", arg)) => {
                    let mut err_tx = tx.clone();
                    let err_future = send_simple_message(
                        msg_id.clone(),
                        &mut err_tx,
                        channel_id.clone(),
                        thread_ts.clone(),
                        format!("<@{}>: {}", user_id, "Please specify a positive integer between 1 and 25."),
                    );

                    if arg.is_empty() {
                        let _ = err_future.await;
                    } else if arg.len() != 1 {
                        let _ = err_future.await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    msg_id,
                                    &mut tx,
                                    &mut sql_tx,
                                    channel_id,
                                    thread_ts,
                                    user_id,
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
                                let _ = err_future.await;
                            },
                        }
                    }
                },

                Ok(command::Command("givers", arg)) => {
                    if arg.is_empty() {
                        top_n(
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
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
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
                            KarmaCol::Given,
                            arg,
                        ).await;
                    }
                },

                Ok(command::Command("topgivers", arg)) => {
                    let mut err_tx = tx.clone();
                    let err_future = send_simple_message(
                        msg_id.clone(),
                        &mut err_tx,
                        channel_id.clone(),
                        thread_ts.clone(),
                        format!("<@{}>: {}", user_id, "Please specify a positive integer between 1 and 25."),
                    );

                    if arg.is_empty() {
                        let _ = err_future.await;
                    } else if arg.len() != 1 {
                        let _ = err_future.await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    msg_id,
                                    &mut tx,
                                    &mut sql_tx,
                                    channel_id,
                                    thread_ts,
                                    user_id,
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
                                let _ = err_future.await;
                            },
                        }
                    }
                },

                Ok(command::Command("sidevotes", arg)) => {
                    if arg.is_empty() {
                        top_n(
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
                            KarmaCol::Recieved,
                            OrdQuery::Desc,
                            KarmaCol::Given,
                            OrdQuery::Desc,
                            KarmaTyp::Side,
                            ("most sidevotes recieved", "most sidevotes given"),
                            3u32,
                        ).await;
                    } else {
                        let _ = send_simple_message(
                            msg_id,
                            &mut tx,
                            channel_id,
                            thread_ts,
                            format!("<@{}>: {}", user_id, "Not supported!"),
                        ).await;
                    }
                },

                Ok(command::Command("topsidevotes", arg)) => {
                    let mut err_tx = tx.clone();
                    let err_future = send_simple_message(
                        msg_id.clone(),
                        &mut err_tx,
                        channel_id.clone(),
                        thread_ts.clone(),
                        format!("<@{}>: {}", user_id, "Please specify a positive integer between 1 and 25."),
                    );

                    if arg.is_empty() {
                        let _ = err_future.await;
                    } else if arg.len() != 1 {
                        let _ = err_future.await;
                    } else {
                        // Parse the argument
                        let limit = u32::from_str(arg.get(0).unwrap_or(&"1"));

                        match limit {
                            Ok(lim@1..=25) => {
                                top_n(
                                    msg_id,
                                    &mut tx,
                                    &mut sql_tx,
                                    channel_id,
                                    thread_ts,
                                    user_id,
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
                                let _ = err_future.await;
                            },
                        }
                    }
                },

                Ok(command::Command("rank", arg)) => {
                    if arg.is_empty() {
                        // Rank with yourself
                        let username = cache.get_username(&user_id).await;

                        match username {
                            Some(ud) => {
                                ranking(
                                    msg_id,
                                    &mut tx,
                                    &mut sql_tx,
                                    channel_id,
                                    thread_ts,
                                    user_id,
                                    KarmaTyp::Total,
                                    &ud,
                                    "Your",
                                ).await;
                            },
                            _ => {
                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    channel_id,
                                    thread_ts,
                                    format!("<@{}>: {}", user_id, "Cant find your display name, thanks slack"),
                                ).await;
                            },
                        }
                    } else if arg.len() == 1 {
                        // Rank up with one target
                        let target = arg.get(0).unwrap_or(&"INVALID");

                        ranking(
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
                            KarmaTyp::Total,
                            target,
                            &format!("{}", target),
                        ).await;
                    } else {
                        let _ = send_simple_message(
                            msg_id,
                            &mut tx,
                            channel_id,
                            thread_ts,
                            format!("<@{}>: {}", user_id, "Can only rank one karma entry at a time!"),
                        ).await;
                    }
                },

                Ok(command::Command("ranksidevote", arg)) => {
                    if arg.is_empty() {
                        // Rank with yourself
                        let username = cache.get_username(&user_id).await;

                        match username {
                            Some(ud) => {
                                ranking(
                                    msg_id,
                                    &mut tx,
                                    &mut sql_tx,
                                    channel_id,
                                    thread_ts,
                                    user_id,
                                    KarmaTyp::Side,
                                    &ud,
                                    "Your",
                                ).await;
                            },
                            _ => {
                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    channel_id,
                                    thread_ts,
                                    format!("<@{}>: {}", user_id, "Cant find your display name, thanks slack"),
                                ).await;
                            },
                        }
                    } else if arg.len() == 1 {
                        // Rank up with one target
                        let target = arg.get(0).unwrap_or(&"INVALID");

                        ranking(
                            msg_id,
                            &mut tx,
                            &mut sql_tx,
                            channel_id,
                            thread_ts,
                            user_id,
                            KarmaTyp::Side,
                            target,
                            &format!("{}", target),
                        ).await;
                    } else {
                        let _ = send_simple_message(
                            msg_id,
                            &mut tx,
                            channel_id,
                            thread_ts,
                            format!("<@{}>: {}", user_id, "Can only rank one karma entry at a time!"),
                        ).await;
                    }
                },

                Ok(command::Command(x, _)) => println!("INFO [User Event]: No handler: {:?}", x),

                Err(_) => {
                    add_karma(
                        &mut sql_tx,
                        &cache,
                        &text,
                        user_id,
                        channel_id,
                    ).await;
                },
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
            ts: _,
        } => {
            add_reacji(
                &mut sql_tx,
                &cache,
                &reaction,
                user_id,
                channel_id,
                ts,
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
            ts: _,
        } => {
            add_reacji(
                &mut sql_tx,
                &cache,
                &reaction,
                user_id,
                channel_id,
                ts,
                ReacjiAction::Del,
            ).await;
        },

        _ => (),
    }
    Ok(())
}
