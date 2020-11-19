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


pub async fn process_inbound_message<R>(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<(crate::database::RunQuery, Option<oneshot::Sender<crate::database::ResQuery>>)>,
    start_time: DateTime<Utc>,
    cache: crate::cache::Cache<R>,
) -> Result<(), Box<dyn std::error::Error>>
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    // TODO: this should really go into the 'message sender' not here
    let id = msg_id.inc();
    println!("Inbound - id = {:?}", id);

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
                        subtype: _,
                        hidden: _,
                        user: u,
                        ts: _,
                    } if c == "CAF6S4TRT".to_string() => {
                        if t == "!version".to_string() {
                            let ver = crate::build_info::PKG_VERSION;
                            let dat = crate::build_info::BUILT_TIME_UTC;
                            let sha = crate::build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": c,
                                "text": format!("<@{}>: Cargo Version: {} - Build Date: {}, - Build SHA: {}", u, ver, dat, sha),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!version");

                        } else if t == "!uptime".to_string() {
                            let end_time: DateTime<Utc> = Utc::now();
                            let durt = end_time.signed_duration_since(start_time).to_std().unwrap_or(
                                Duration::from_secs(3122064000));

                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": c,
                                "text": format!("<@{}>: {}", u, format_duration(durt).to_string()),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!uptime");

                        } else if t == "!help".to_string() {
                            let help = "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote";
                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": c,
                                "text": format!("<@{}>: {}", u, help),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!help");

                        } else if t == "!github".to_string() {
                            let github = "Github repo: https://github.com/pharaun/Karmator";
                            let ws_msg = json!({
                                "id": id,
                                "type": "message",
                                "channel": c,
                                "text": format!("<@{}>: {}", u, github),
                            }).to_string();

                            tx.send(tungstenite::tungstenite::Message::from(ws_msg)).await;
                            println!("Inbound - \t\t!github");

                        } else if t == "!karma".to_string() {
                            // asdf「asdf」asdf
                            // TODO: Implement a 'slack id' to unix name mapper
                            //  - for now can probs query it as needed, but should
                            //      be cached
                            //
                            // Query:
                            // count = 3
                            // topNDenormalizedT (karmaRecievedName) (karmaRecievedTotal) 3 desc
                            // topNDenormalizedT (karmaRecievedName) (karmaRecievedTotal) 3 asc
                            //
                            // If '!karma a b' specify do
                            // partalKarma (KarmaRecieved) [list of entity]

                            let user_display = crate::cache::get_user_display(cache.clone(), &u).await;
                            println!("User id: {:?}, Display: {:?}", u, user_display);


                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Asc
                                },
                                None
                            )).await;

                            // If a list of name is given query this
                            sql_tx.send((
                                crate::database::RunQuery::Partial {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    users: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()].into_iter().collect(),
                                },
                                None
                            )).await;

//                            // Dummy get/send stuff
//                            let (tx1, rx1) = oneshot::channel();
//                            sql_tx.send((
//                                crate::database::RunQuery::Insert("hi".to_string(), "bye".to_string()),
//                                Some(tx1)
//                            )).await;
//
//                            // Await for the results
//                            let res1 = rx1.await;
//                            let ws_msg = json!({
//                                "id": id,
//                                "type": "message",
//                                "channel": "CAF6S4TRT",
//                                "text": format!("<@{}>: insert: {:?}", u, res1),
//                            }).to_string();

                            println!("Inbound - \t\t!karma");

                        } else if t == "!givers".to_string() {
                            // Query:
                            // count = 3
                            // topNDenormalizedT (karmaGivenName) (karmaGivenTotal) 3 desc
                            // topNDenormalizedT (karmaGivenName) (karmaGivenTotal) 3 asc
                            //
                            // If '!givers a b' specify do
                            // partalKarma (KarmaGiven) [list of entity]
                            //
                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Given,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Given,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Asc
                                },
                                None
                            )).await;

                            // If a list of name is given query this
                            sql_tx.send((
                                crate::database::RunQuery::Partial {
                                    karma_col: crate::database::KarmaCol::Given,
                                    users: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()].into_iter().collect(),
                                },
                                None
                            )).await;

                            println!("Inbound - \t\t!givers");

                        } else if t == "!sidevotes".to_string() {
                            // Query:
                            // count = 3
                            // topNDenormalizedT (karmaRecievedName) (karmaRecievedSide) 3 desc
                            // topNDenormalizedT (karmaGivenName) (karmaGivenSide) 3 desc
                            //
                            // if '!sidevotes a b' specify do
                            // not supported - error

                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    karma_typ: crate::database::KarmaTyp::Side,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::TopNDenormalized {
                                    karma_col: crate::database::KarmaCol::Given,
                                    karma_typ: crate::database::KarmaTyp::Side,
                                    limit: 3,
                                    ord: crate::database::OrdQuery::Desc
                                },
                                None
                            )).await;

                            println!("Inbound - \t\t!sidevotes");

                        } else if t == "!rank".to_string() {
                            // Query:
                            // rankingDenormalizedT (karmaRecievedName) (karmaRecievedTotal) user
                            // countT (karmaRecievedName)
                            // rankingDenormalizedT (karmaGivenName) (karmaGivenTotal) user
                            // countT (karmaGivenName)

                            sql_tx.send((
                                crate::database::RunQuery::RankingDenormalized {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::Count(crate::database::KarmaCol::Recieved),
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::RankingDenormalized {
                                    karma_col: crate::database::KarmaCol::Given,
                                    karma_typ: crate::database::KarmaTyp::Total,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::Count(crate::database::KarmaCol::Given),
                                None
                            )).await;

                            println!("Inbound - \t\t!rank");

                        } else if t == "!ranksidevote".to_string() {
                            // Query:
                            // rankingDenormalizedT (karmaRecievedName) (karmaRecievedSide) user
                            // countT (karmaRecievedName)
                            // rankingDenormalizedT (karmaGivenName) (karmaGivenSide) user
                            // countT (karmaGivenName)

                            sql_tx.send((
                                crate::database::RunQuery::RankingDenormalized {
                                    karma_col: crate::database::KarmaCol::Recieved,
                                    karma_typ: crate::database::KarmaTyp::Side,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::Count(crate::database::KarmaCol::Recieved),
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::RankingDenormalized {
                                    karma_col: crate::database::KarmaCol::Given,
                                    karma_typ: crate::database::KarmaTyp::Side,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                crate::database::RunQuery::Count(crate::database::KarmaCol::Given),
                                None
                            )).await;

                            println!("Inbound - \t\t!sidevote");
                        }
                    },

                    _ => println!("Inbound - \t\tNo action taken"),
                }
            },

            Event::SystemControl(sc) => (),
            Event::MessageControl(mc) => (),
        }
    }
    Ok(())
}
