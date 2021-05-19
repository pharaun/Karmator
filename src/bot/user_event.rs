use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;

use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;
use std::str::FromStr;


use crate::bot::user_database::{RunQuery, ResQuery, KarmaCol, KarmaTyp, OrdQuery, KarmaName, ReacjiAction};
use crate::bot::user_database::send_query;
use crate::bot::user_database::send_query_commit;
use crate::bot::build_info;
use crate::core::command;
use crate::bot::parser::karma;
use crate::core::santizer;
use crate::bot::parser::reacji_to_karma;
use crate::core::cache;
use crate::core::event::MsgId;
use crate::core::event::UserEvent;
use crate::core::event::ReactionItem;
use crate::core::event::send_simple_message;
use crate::core::database::Query;


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
                    let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote !topkarma !topgivers !topsidevotes";
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
                        let limit = u32::from_str(arg.get(0).unwrap());

                        match limit {
                            Ok(1..=25) => {
                                let limit = limit.unwrap();

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
                                    limit,
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
                        let limit = u32::from_str(arg.get(0).unwrap());

                        match limit {
                            Ok(1..=25) => {
                                let limit = limit.unwrap();

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
                                    limit,
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
                        let limit = u32::from_str(arg.get(0).unwrap());

                        match limit {
                            Ok(1..=25) => {
                                let limit = limit.unwrap();

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
                                    limit,
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
                        let target = arg.get(0).unwrap();

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
                        let target = arg.get(0).unwrap();

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


async fn add_reacji<R>(
    sql_tx: &mut mpsc::Sender<Query>,
    cache: &cache::Cache<R>,
    input: &str,
    user_id: String,
    channel_id: String,
    ts: String,
    action: ReacjiAction,
)
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    match reacji_to_karma(input) {
        Some(karma) => {
            let message_id = send_query(
                sql_tx,
                RunQuery::QueryReacjiMessage {
                    channel_id: channel_id.clone(),
                    message_ts: ts.clone(),
                }
            ).await.map(|t| {
                match t {
                    ResQuery::MessageId(id) => id,
                    _ => None,
                }
            }).ok().flatten();

            let message_id = match message_id {
                None => {
                    match cache.get_message(&channel_id, &ts).await {
                        Some(cache::ConversationHistoryMessage::Message { text, user_id: Some(user_id) }) => {
                            // We have the message content, insert it into the table and
                            // get its row id
                            let santized_text = santizer(&text, &cache).await;

                            match (
                                cache.get_username(&user_id).await,
                                cache.get_user_real_name(&user_id).await,
                            ) {
                                (Some(ud), Some(rn)) => {
                                    send_query(
                                        sql_tx,
                                        RunQuery::AddReacjiMessage {
                                            user_id: user_id,
                                            username: KarmaName::new(&ud),
                                            real_name: KarmaName::new(&rn),
                                            channel_id: channel_id.clone(),
                                            message_ts: ts.clone(),
                                            message: santized_text,
                                        }
                                    ).await.map(|t| {
                                        match t {
                                            ResQuery::MessageId(id) => id,
                                            _ => None,
                                        }
                                    }).ok().flatten()
                                },
                                _ => {
                                    eprintln!("ERROR: [User Event] Wasn't able to get a username/real_name from slack");
                                    None
                                },
                            }
                        },
                        // This should already have been logged by the api call function
                        _ => None,
                    }
                },
                Some(mid) => Some(mid),
            };

            match message_id {
                None => eprintln!(
                    "ERROR: [User Event] Wasn't able to get/store an reactji message, vote isn't recorded"
                ),
                Some(mid) => {
                    match (
                        cache.get_username(&user_id).await,
                        cache.get_user_real_name(&user_id).await,
                    ) {
                        (Some(ud), Some(rn)) => {
                            let _ = send_query(
                                sql_tx,
                                RunQuery::AddReacji {
                                    timestamp: Utc::now(),
                                    user_id: user_id,
                                    username: KarmaName::new(&ud),
                                    real_name: KarmaName::new(&rn),
                                    action: action,
                                    message_id: mid,
                                    result: karma,
                                }
                            ).await;
                        },
                        _ => eprintln!("ERROR: [User Event] Wasn't able to get a username/real_name from slack"),
                    }
                },
            }
        },
        None => (),
    }
}


async fn add_karma<R>(
    sql_tx: &mut mpsc::Sender<Query>,
    cache: &cache::Cache<R>,
    input: &str,
    user_id: String,
    channel_id: String,
)
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    let santized_text = santizer(&input, &cache).await;
    let res = karma::parse(&santized_text);

    match res {
        Ok(mut karma) if !karma.is_empty() => {
            println!("INFO [User Event]: Parsed Karma: {:?}", karma);
            let username = cache.get_username(&user_id).await;
            let user_real_name = cache.get_user_real_name(&user_id).await;

            // Filter karma of any entity that is same as
            // username, and check if any got filtered, if
            // so, log.
            let before = karma.len();
            match username {
                None     => (),
                Some(ref ud) => karma.retain(|&karma::KST(ref t, _)| {
                    KarmaName::new(t) != KarmaName::new(ud)
                }),
            }
            let after = karma.len();

            if before != after {
                println!("INFO [User Event]: User self-voted: {:?}", username);
            }

            if !karma.is_empty() {
                match (username, user_real_name) {
                    (Some(ud), Some(rn)) => {
                        let _ = send_query_commit(
                            sql_tx,
                            RunQuery::AddKarma {
                                timestamp: Utc::now(),
                                user_id: user_id,
                                username: KarmaName::new(&ud),
                                real_name: KarmaName::new(&rn),
                                channel_id: Some(channel_id),
                                karma: karma,
                            }
                        ).await;
                    },
                    _ => eprintln!("ERROR: [User Event] Wasn't able to get a username/real_name from slack"),
                }
            }
        },

        Ok(_) => (),

        Err(e) => {
            // The parse should return empty if its valid, something
            // broke, should log it here
            eprintln!("ERROR [User Event]: Failed to parse karma: {:?}", e);
        },
    }
}


async fn santizer<R>(
    input: &str,
    cache: &cache::Cache<R>,
) -> String
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    match santizer::parse(input).ok() {
        None      => {
            eprintln!("ERROR [Santizer]: Failed to santize: {:?}", input);
            input.to_string()
        },

        Some(san) => {
            let mut safe_text = vec![];

            for seg in san.iter() {
                // TODO: do other id reprocessing such as:
                // 1. channel...
                match seg {
                    santizer::Segment::User(uid, l) => {
                        // Do a user id lookup
                        let username = cache.get_username(&uid).await;

                        match username {
                            Some(name) => safe_text.push(name),
                            None => {
                                // TODO: Log this, but for now fallback to
                                // just rendering it straight into the db
                                safe_text.push(
                                    santizer::Segment::User(uid, l).to_string()
                                );
                            },
                        }
                    },
                    // Everything else
                    s => safe_text.push(s.to_string()),
                }
            }

            safe_text.join("")
        },
    }
}


async fn top_n(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<Query>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    kcol1: KarmaCol,
    kord1: OrdQuery,
    kcol2: KarmaCol,
    kord2: OrdQuery,
    ktyp: KarmaTyp,
    label: (&str, &str),
    limit: u32,
) {
    let high = send_query(
        sql_tx,
        RunQuery::TopNDenormalized {
            karma_col: kcol1,
            karma_typ: ktyp,
            limit: limit,
            ord: kord1,
        }
    ).await.map(|h| {
        // Format it
        match h {
            ResQuery::TopN(e) => {
                e.iter().map(
                    |(e, c)| format!("{}, ({})", e, c)
                ).collect::<Vec<String>>().join("; ")
            },
            _ => "Wrong Result".to_string(),
        }
    });

    let low = send_query(
        sql_tx,
        RunQuery::TopNDenormalized {
            karma_col: kcol2,
            karma_typ: ktyp,
            limit: limit,
            ord: kord2,
        }
    ).await.map(|l| {
        // Format it
        match l {
            ResQuery::TopN(e) => {
                e.iter().map(
                    |(e, c)| format!("{}, ({})", e, c)
                ).collect::<Vec<String>>().join("; ")
            },
            _ => "Wrong Result".to_string(),
        }
    });

    // TODO: do something about this
    let _ = match (high, low) {
        (Ok(h), Ok(l)) => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}: {}. {}: {}.", user, label.0, h, label.1, l),
        ).await,
        _ => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}", user, "Something went wrong"),
        ).await,
    };
}

async fn partial(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<Query>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    let res = send_query(
        sql_tx,
        RunQuery::Partial {
            karma_col: kcol,
            users: arg.into_iter().map(|i| KarmaName::new(i)).collect(),
        }
    ).await.map(|p| {
        // Format it
        match p {
            ResQuery::Partial(e) => {
                e.iter().map(|(entity, up, down, side)| {
                    format!(
                        "{}, {} ({}++/{}--/{}+-)",
                        entity, (up - down), up, down, side
                    )
                }).collect::<Vec<String>>().join("; ")
            },
            _ => "Wrong Result".to_string(),
        }
    });

    // TODO: do something here
    let _ = match res {
        Ok(x) => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}", user, x),
        ).await,
        _ => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}", user, "Something went wrong"),
        ).await,
    };
}


async fn ranking(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<Query>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
) {
    let target_recieved = send_query(
        sql_tx,
        RunQuery::RankingDenormalized {
            karma_col: KarmaCol::Recieved,
            karma_typ: ktyp,
            user: KarmaName::new(target),
        }
    ).await.map(|t| {
        // Format it
        match t {
            ResQuery::OCount(e) => e.map(|c| format!("{}", c)),
            _ => None,
        }
    });

    let total_recieved = send_query(
        sql_tx,
        RunQuery::Count(KarmaCol::Recieved),
    ).await.map(|t| {
        // Format it
        match t {
            ResQuery::Count(e) => format!("{}", e),
            _ => "Wrong Result".to_string(),
        }
    });

    let target_given = send_query(
        sql_tx,
        RunQuery::RankingDenormalized {
            karma_col: KarmaCol::Given,
            karma_typ: ktyp,
            user: KarmaName::new(target),
        }
    ).await.map(|t| {
        // Format it
        match t {
            ResQuery::OCount(e) => e.map(|c| format!("{}", c)),
            _ => None,
        }
    });

    let total_given = send_query(
        sql_tx,
        RunQuery::Count(KarmaCol::Given),
    ).await.map(|t| {
        // Format it
        match t {
            ResQuery::Count(e) => format!("{}", e),
            _ => "Wrong Result".to_string(),
        }
    });


    // Formatting the ranks
    let receiving = match (target_recieved, total_recieved) {
        (Ok(Some(r)), Ok(tr)) => Some(format!("{} rank is {} of {} in receiving", label, r, tr)),
        _ => None,
    };

    let giving = match (target_given, total_given) {
        (Ok(Some(g)), Ok(tg)) => Some(format!("{} rank is {} of {} in giving", label, g, tg)),
        _ => None,
    };

    let rank = match (receiving, giving) {
        (Some(r), Some(g)) => format!("{} and {}.", r, g),
        (Some(r), None)    => format!("{}.", r),
        (None, Some(g))    => format!("{}.", g),
        (None, None)       => format!("No ranking available"),
    };

    let _ = send_simple_message(
        msg_id,
        tx,
        channel,
        thread_ts,
        format!("<@{}>: {}", user, rank),
    ).await;
}