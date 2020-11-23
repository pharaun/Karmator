use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;
use tokio::sync::oneshot;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::sync::Arc;
use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;


use crate::database::{RunQuery, ResQuery, KarmaCol, KarmaTyp, OrdQuery};
use crate::cache;
use crate::build_info;
use crate::parser;


// Type alias for msg_id
pub type MsgId = Arc<RelaxedCounter>;


#[derive(Debug)]
enum Event {
    UserEvent(UserEvent),
    SystemControl(SystemControl),
    MessageControl(MessageControl),
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum UserEvent {
    // These events are what plugins will care for:
    //
    // Slack messages
    // TODO: more involved here for now basics
    Message {
        subtype: Option<String>,
        hidden: Option<bool>,
        channel: Option<String>,
        user: String,
        text: String,
        ts: String,
    },

    // TODO: consider looking at slack_api for types to reuse here
    ReactionAdded {
        user: String,
        reaction: String,
        item_user: Option<String>,
        // item: ListResponseItem
        event_ts: String,
    },
    ReactionRemoved {
        user: String,
        reaction: String,
        item_user: Option<String>,
        // item: ListResponseItem
        event_ts: String,
    },
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum SystemControl {
    // First message upon successful connection establishment
    Hello,

    // Client should reconnect after getting such message
    Goodbye,

    // Reply to Ping = { type: ping, id: num }
    Pong { reply_to: usize },
}


#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum MessageControl {
    // Reply to message sent
    MessageSent {
        ok: bool,
        reply_to: usize,
        text: String,
        ts: String
    },

    // Reply to failed message sent
    MessageError {
        ok: bool,
        reply_to: usize,
        error: ErrorDetail,
    },
}

#[derive(Debug, Deserialize)]
struct ErrorDetail {
    code: usize,
    msg: String,
}


fn parse_event(s: String) -> Option<Event> {
    // Return User Events first, then the other two
    serde_json::from_str(&s).and_then(|ue| {
        println!("Inbound - \tUser Event = {:?}", ue);
        Ok(Event::UserEvent(ue))
    }).or_else(|_| {
        serde_json::from_str(&s).and_then(|sc| {
            println!("Inbound - \tSystem Control = {:?}", sc);
            Ok(Event::SystemControl(sc))
        }).or_else(|_| {
            serde_json::from_str(&s).and_then(|mc| {
                println!("Inbound - \tMessage Control = {:?}", mc);
                Ok(Event::MessageControl(mc))
            }).or_else(|x| {
                // TODO: for now print to stderr what didn't get deserialized
                // later can have config option to log to file the error or not
                //eprintln!("Inbound - \tFail parse = {:?}", s);
                Err(x)
            })
        })
    }).ok()
}


async fn send_simple_message(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    channel: String,
    text: String
) -> Result<(), &'static str> {
    // TODO: register this message send to be tracked later
    let ws_msg = json!({
        "id": msg_id.inc(),
        "type": "message",
        "channel": channel,
        "text": text,
    }).to_string();
    tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await.map_err(|_| "Error sending")
}


async fn send_query(
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    query: RunQuery,
) -> Result<ResQuery, &'static str> {
    let (tx, rx) = oneshot::channel();
    sql_tx.send((query, Some(tx))).await.map_err(|_| "Error sending")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn process_inbound_message<R>(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    start_time: DateTime<Utc>,
    cache: cache::Cache<R>,
) -> Result<(), Box<dyn std::error::Error>>
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{

    // Parse incoming message
    let raw_msg = match msg {
        tungstenite::tungstenite::Message::Text(x) => Some(x),
        _ => {
            println!("Inbound - \tUnsupported ws message type - {:?}", msg);
            None
        },
    };
    //println!("Inbound - raw event = {:?}", raw_msg);

    if let Some(e) = raw_msg.and_then(parse_event) {
        match e {
            Event::UserEvent(event) => {
                // Check if its a message/certain string, if so, reply
                match event {
                    // TODO: support DM, right now its only in channels
                    UserEvent::Message {
                        channel: Some(c),
                        text: t,
                        user: u,
                        ..
                    } if c == "CAF6S4TRT".to_string() => {
                        // TODO: don't react to myself
                        // TODO: check if it is a bot, if so, ignore as well
                        //       Should add a bot flag to the user_display cache as well
                        //       Should also make sure to throttle cache update rate as well
                        if u == "UALB533UK".to_string() {
                            println!("Inbound - Ignoring myself");
                            return Ok(());
                        }

                        // Parse string to see if its a command one
                        let res = parser::parse_command(&t);

                        match res {
                            Ok(parser::Command("version", _)) => {
                                let ver = build_info::PKG_VERSION;
                                let dat = build_info::BUILT_TIME_UTC;
                                let sha = build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    c,
                                    format!("<@{}>: Cargo Version: {} - Build Date: {}, - Build SHA: {}", u, ver, dat, sha),
                                );
                                println!("Inbound - \t\t!version");

                            },
                            Ok(parser::Command("uptime", _)) => {
                                let end_time: DateTime<Utc> = Utc::now();
                                let durt = end_time.signed_duration_since(start_time).to_std().unwrap_or(
                                    Duration::from_secs(3122064000));

                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    c,
                                    format!("<@{}>: {}", u, format_duration(durt).to_string()),
                                );
                                println!("Inbound - \t\t!uptime");

                            },
                            Ok(parser::Command("help", _)) => {
                                let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote";
                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    c,
                                    format!("<@{}>: {}", u, help),
                                );
                                println!("Inbound - \t\t!help");

                            },
                            Ok(parser::Command("github", _)) => {
                                let github = "Github repo: https://github.com/pharaun/Karmator";

                                let _ = send_simple_message(
                                    msg_id,
                                    &mut tx,
                                    c,
                                    format!("<@{}>: {}", u, github),
                                );
                                println!("Inbound - \t\t!github");

                            },
                            Ok(parser::Command("karma", arg)) => {
                                // asdf「asdf」asdf
                                // TODO: Implement a 'slack id' to unix name mapper
                                //  - for now can probs query it as needed, but should
                                //      be cached
                                if arg.is_empty() {
                                    top_n(
                                        msg_id,
                                        &mut tx,
                                        &mut sql_tx,
                                        c,
                                        u,
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
                                        c,
                                        u,
                                        KarmaCol::Recieved,
                                        arg,
                                    ).await;
                                }
                            },
                            Ok(parser::Command("givers", arg)) => {
                                if arg.is_empty() {
                                    top_n(
                                        msg_id,
                                        &mut tx,
                                        &mut sql_tx,
                                        c,
                                        u,
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
                                        c,
                                        u,
                                        KarmaCol::Given,
                                        arg,
                                    ).await;
                                }
                            },
                            Ok(parser::Command("sidevotes", arg)) => {
                                if arg.is_empty() {
                                    top_n(
                                        msg_id,
                                        &mut tx,
                                        &mut sql_tx,
                                        c,
                                        u,
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
                                        c,
                                        format!("<@{}>: {}", u, "Not supported!"),
                                    ).await;
                                }
                            },
                            Ok(parser::Command("rank", arg)) => {
                                if arg.is_empty() {
                                    // Rank with yourself
                                    let user_display = cache.get_user_display(&u).await;

                                    match user_display {
                                        Some(ud) => {
                                            ranking(
                                                msg_id,
                                                &mut tx,
                                                &mut sql_tx,
                                                c,
                                                u,
                                                KarmaTyp::Total,
                                                &ud,
                                                "Your",
                                            ).await;
                                        },
                                        _ => {
                                            let _ = send_simple_message(
                                                msg_id,
                                                &mut tx,
                                                c,
                                                format!("<@{}>: {}", u, "Cant find your display name, thanks slack"),
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
                                        c,
                                        u,
                                        KarmaTyp::Total,
                                        target,
                                        &format!("{}", target),
                                    ).await;
                                } else {
                                    let _ = send_simple_message(
                                        msg_id,
                                        &mut tx,
                                        c,
                                        format!("<@{}>: {}", u, "Can only rank one karma entry at a time!"),
                                    ).await;
                                }
                            },
                            Ok(parser::Command("ranksidevote", arg)) => {
                                if arg.is_empty() {
                                    // Rank with yourself
                                    let user_display = cache.get_user_display(&u).await;

                                    match user_display {
                                        Some(ud) => {
                                            ranking(
                                                msg_id,
                                                &mut tx,
                                                &mut sql_tx,
                                                c,
                                                u,
                                                KarmaTyp::Side,
                                                &ud,
                                                "Your",
                                            ).await;
                                        },
                                        _ => {
                                            let _ = send_simple_message(
                                                msg_id,
                                                &mut tx,
                                                c,
                                                format!("<@{}>: {}", u, "Cant find your display name, thanks slack"),
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
                                        c,
                                        u,
                                        KarmaTyp::Side,
                                        target,
                                        &format!("{}", target),
                                    ).await;
                                } else {
                                    let _ = send_simple_message(
                                        msg_id,
                                        &mut tx,
                                        c,
                                        format!("<@{}>: {}", u, "Can only rank one karma entry at a time!"),
                                    ).await;
                                }
                            },
                            Ok(parser::Command(x, _)) => println!("Input - Parse - No Handler: {:?}", x),

                            Err(_) => {
                                // Not a command parse, time to consider allkarma parser
                                println!("Input - All Karma");

                                //let res = parser::all_karma(&t);
                                //println!("Input - All Karma - {:?}", res);
                            },
                        }
                    },

                    _ => println!("Inbound - \t\tNo action taken"),
                }
            },

            Event::SystemControl(_sc) => (),
            Event::MessageControl(_mc) => (),
        }
    }
    Ok(())
}


async fn top_n(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
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
            format!("<@{}>: {}: {}. {}: {}.", user, label.0, h, label.1, l),
        ).await,
        _ => send_simple_message(
            msg_id,
            tx,
            channel,
            format!("<@{}>: {}", user, "Something went wrong"),
        ).await,
    };
}

async fn partial(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
    user: String,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    println!("Input - Karma - Partial");

    let res = send_query(
        sql_tx,
        RunQuery::Partial {
            karma_col: kcol,
            users: arg.into_iter().map(|i| i.to_string()).collect(),
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
            format!("<@{}>: {}", user, x),
        ).await,
        _ => send_simple_message(
            msg_id,
            tx,
            channel,
            format!("<@{}>: {}", user, "Something went wrong"),
        ).await,
    };
}


async fn ranking(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    channel: String,
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
            user: target.to_string(),
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
            user: target.to_string(),
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
        format!("<@{}>: {}", user, rank),
    ).await;
}
