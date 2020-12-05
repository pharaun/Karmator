use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;
use tokio::sync::oneshot;

use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;


use crate::database::{RunQuery, ResQuery, KarmaCol, KarmaTyp, OrdQuery, KarmaName};
use crate::cache;
use crate::build_info;
use crate::parser::command;
use crate::parser::karma;
use crate::parser::santizer;
use crate::event::MsgId;
use crate::event::UserEvent;
use crate::event::send_simple_message;
use crate::event::send_query;


pub async fn process_user_message<R>(
    msg_id: MsgId,
    msg: UserEvent,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
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
            text,
            user_id,
            thread_ts,
            subtype: _,
            hidden: _,
            ts: _,
        } => {
            // Check if user is a bot, if so ignore (this applies to myself as well)
            match cache.is_user_bot(&user_id).await {
                None => {
                    println!("Inbound - No Info on user: {:?}", user_id);
                    return Ok(());
                },
                Some(is_bot) => {
                    if is_bot {
                        println!("Inbound - User: {:?} is a bot, ignoring", user_id);
                        return Ok(());
                    }
                }
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
                    println!("Inbound - \t\t!version");

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
                    println!("Inbound - \t\t!uptime");

                },
                Ok(command::Command("help", _)) => {
                    let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote";
                    let _ = send_simple_message(
                        msg_id,
                        &mut tx,
                        channel_id,
                        thread_ts,
                        format!("<@{}>: {}", user_id, help),
                    ).await;
                    println!("Inbound - \t\t!help");

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
                    println!("Inbound - \t\t!github");

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
                Ok(command::Command("rank", arg)) => {
                    if arg.is_empty() {
                        // Rank with yourself
                        let user_display = cache.get_user_display(&user_id).await;

                        match user_display {
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
                        let user_display = cache.get_user_display(&user_id).await;

                        match user_display {
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
                Ok(command::Command(x, _)) => println!("Input - Parse - No Handler: {:?}", x),

                // TODO: query + double check the user isn't a bot (if so ignore)
                Err(_) => {
                    // Not a command parse, time to consider allkarma parser
                    println!("Input - All Karma");

                    // Santize the incoming stream
                    match santizer::parse(&text).ok() {
                        None      => println!("Input - All Karma - Failed to santize: {:?}", &text),
                        Some(san) => {
                            let mut safe_text = vec![];

                            for seg in san.iter() {
                                // TODO: do other id reprocessing such as:
                                // 1. channel...
                                match seg {
                                    santizer::Segment::User(uid, l) => {
                                        // Do a user id lookup
                                        let user_display = cache.get_user_display(&user_id).await;

                                        match user_display {
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

                            // Parse karma now
                            let safe_text = safe_text.join("");
                            let res = karma::parse(&safe_text);

                            match res {
                                Ok(mut karma) if !karma.is_empty() => {
                                    println!("Input - All Karma - {:?}", karma);
                                    let user_display = cache.get_user_display(&user_id).await;
                                    let user_name = cache.get_user_name(&user_id).await;

                                    // Filter karma of any entity that is same as
                                    // user_display, and check if any got filtered, if
                                    // so, log.
                                    let before = karma.len();
                                    match user_display {
                                        None     => (),
                                        Some(ref ud) => karma.retain(|&karma::KST(ref t, _)| t != ud),
                                    }
                                    let after = karma.len();

                                    if before != after {
                                        println!("Input - All Karma - user: {:?} self-voted", user_display);
                                    }

                                    if !karma.is_empty() {
                                        println!("Input - All Karma - Actual: {:?}", karma);

                                        let _ = sql_tx.send((
                                            RunQuery::AddKarma {
                                                timestamp: Utc::now(),
                                                user_id: user_id,
                                                username: user_display.map(|ud| KarmaName::new(&ud)),
                                                real_name: user_name.map(|rn| KarmaName::new(&rn)),
                                                channel_id: Some(channel_id),
                                                karma: karma,
                                            },
                                            None
                                        )).await;
                                    }
                                },
                                Ok(_) => (),
                                Err(e) => {
                                    // The parse should return empty if its valid, something
                                    // broke, should log it here
                                    eprintln!("All Karma - Error - {:?}", e);
                                },
                            }
                        },
                    }
                },
            }
        },

        _ => println!("Inbound - \t\tNo action taken"),
    }
    Ok(())
}


async fn top_n(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    kcol1: KarmaCol,
    kord1: OrdQuery,
    kcol2: KarmaCol,
    kord2: OrdQuery,
    ktyp: KarmaTyp,
    label: (&str, &str),
) {
    println!("Input - Karma - Denormalized");

    let high = send_query(
        sql_tx,
        RunQuery::TopNDenormalized {
            karma_col: kcol1,
            karma_typ: ktyp,
            limit: 3,
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
            limit: 3,
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
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    println!("Input - Karma - Partial");

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
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
) {
    println!("Input - Karma - Ranking");

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