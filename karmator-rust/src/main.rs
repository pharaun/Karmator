use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::collections::HashSet;
use std::sync::Arc;
use std::default::Default;
use std::env;
use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;

// SQlite worker thread
use std::thread;
use futures::executor::block_on_stream;

use rusqlite as rs;
use std::path::Path;
use tokio::sync::oneshot;

// User id -> display name cache
// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;

// Test data
mod schema_sample;


// Use of a mod or pub mod is not actually necessary.
pub mod build_info {
   // The file has been placed there by the build script.
   include!(concat!(env!("OUT_DIR"), "/built.rs"));
}


// Type alias for msg_id
type MsgId = Arc<RelaxedCounter>;
type UserCache = Arc<DashMap<String, String>>;


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Shared integer counter for message ids
    let msg_id = Arc::new(RelaxedCounter::new(0));

    // Uptime of program start
    let start_time: DateTime<Utc> = Utc::now();

    // User id -> User Display name cache
    let user_cache: UserCache = Arc::new(DashMap::new());


    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    // Post a message
    let msg = slack::chat::PostMessageRequest {
        channel: "karmator-devs",
        text: "Pogger, i'm starting up!",
        as_user: Some(true),
        ..Default::default()
    };
    slack::chat::post_message(&client, &token, &msg).await;

    // Work to establish the WS connection
    let response = slack::rtm::connect(&client, &token).await.map_err(|e| format!("Control - {:?}", e))?;
    println!("Control - Got an ok reply");

    let ws_url = response.url.ok_or(format!("Control - \tNo Ws url"))?;
    println!("Control - \tGot a WS url: {:?}", ws_url);

    let (ws_stream, _) = tungstenite::connect_async(ws_url).await.map_err(|e| format!("Control - \t\t{:?}", e))?;
    println!("Control - \t\tWS connection established");

    // Setup the mpsc channel
    let (tx, mut rx) = mpsc::channel(32);

    // Split the stream
    let (mut ws_write, mut ws_read) = ws_stream.split();


    // Setup the mpsc channel for sqlite and spawn the sqlite worker thread
    // TODO: define a data format for asking the db to query something on your behalf, for now
    // pass in the raw query string?
    let (sql_tx, sql_rx) = mpsc::channel(32);

    let sql_worker = thread::spawn(move || {
        println!("Sql Worker - Launching");
        process_queries(sql_rx);
        println!("Sql Worker - Exiting");
    });

    // Spawn the inbound ws stream processor
    let inbound = tokio::spawn(async move {
        loop {
            if let Some(Ok(ws_msg)) = ws_read.next().await {
                let msg_id = msg_id.clone();
                let tx2 = tx.clone();
                let sql_tx2 = sql_tx.clone();
                let user_cache = user_cache.clone();
                let token = token.clone();

                // TODO: not 100% for sure here but seems like inner handle is behind ARC so should
                // be safe to clone a reqwest client
                let client = client.clone();

                tokio::spawn(async move {
                    process_inbound_message(
                        msg_id,
                        ws_msg,
                        tx2,
                        sql_tx2,
                        start_time,
                        user_cache,
                        &token,
                        client
                    ).await;
                });

            }
        }
    });

    // Spawn outbound ws processor
    // TODO: need to make sure to wait on sending till we recieve the hello event
    // could be a oneshot or some sort of flag which it waits till inbound has
    // gotten the hello
    let outbound = tokio::spawn(async move {
        loop {
            while let Some(message) = rx.recv().await {
                println!("Outbound - {:?}", message);

                // TODO: present some way to do plain vs fancy message, and if
                // fancy do a webapi post, otherwise dump into the WS
                //
                // TODO: look into tracking the sent message with a confirmation
                // that it was sent (via msg id) and if it fails, resend with
                // backoff
                //
                // TODO: find a way to handle the ping/pong cycle and monitor
                ws_write.send(message).await;
            }
        }
    });

    // Wait till either exits (error) then begin recovery
    match tokio::try_join!(inbound, outbound) {
        Ok((first, second)) => println!("Control - \t\t\tBoth exited fine"),
        Err(err) => println!("Control - \t\t\tSomething failed: {:?}", err),
    }

    // The sql_tx got moved into the inbound tokio async, so when that dies....
    let res = sql_worker.join();
    println!("Control - \t\t\tSql worker: {:?}", res);

    Ok(())
}


fn process_queries(
    sql_rx: mpsc::Receiver<(RunQuery, Option<oneshot::Sender<ResQuery>>)>
) {
    let mut block_sql_rx = block_on_stream(sql_rx);

    //let filename = "db.sqlite";
    //let conn = rs::Connection::open_with_flags(
    //    Path::new(filename),
    //    rs::OpenFlags::SQLITE_OPEN_READ_WRITE
    //).expect(&format!("Connection error: {}", filename));
    let conn = rs::Connection::open_in_memory().expect("Failure");

    // Test data/build tables
    schema_sample::setup_table(&conn);
    schema_sample::setup_data(&conn);


    // Listen for inbound query
    while let Some((query, res_tx)) = block_sql_rx.next() {
        match query {
            RunQuery::TopNDenormalized{
                karma_col: kcol,
                karma_typ: ktyp,
                limit: lim,
                ord: ord
            } => {
                println!(
                    "Sql Worker - TopNDenormalized - kcol: {:?}, ktyp: {:?}, lim: {:?}, ord: {:?}",
                    kcol, ktyp, lim, ord
                );

                let table = match kcol {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                let tcol = match ktyp {
                    KarmaTyp::Total => "up - down",
                    KarmaTyp::Side  => "side",
                };

                let qOrd = match ord {
                    OrdQuery::Asc  => "ASC",
                    OrdQuery::Desc => "DESC",
                };

                let mut stmt = conn.prepare(&format!(
                        "SELECT name, {tcol} as total FROM {table} ORDER BY total {qord} LIMIT {lim}",
                        tcol=tcol,
                        table=table,
                        qord=qOrd,
                        lim=lim
                )).unwrap();
                let mut rows = stmt.query(rs::NO_PARAMS).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let name: String = row.get(0).unwrap();
                    let count: i32 = row.get(1).unwrap();

                    println!("Sql Worker - TopNDenormalized - Name: {:?}, Count: {:?}", name, count);
                } else {
                    println!("Sql Worker - TopNDenormalized - Error");
                }
            },
            RunQuery::RankingDenormalized{
                karma_col: kcol,
                karma_typ: ktyp,
                user: user
            } => {
                println!("Sql Worker - RankingDenormalized - kcol: {:?}, ktyp: {:?}, user: {:?}", kcol, ktyp, user);

                let table = match kcol {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                let tcol1 = match ktyp {
                    KarmaTyp::Total => "up - down",
                    KarmaTyp::Side  => "side",
                };

                let tcol2 = match ktyp {
                    KarmaTyp::Total => "kcol2.up - kcol2.down",
                    KarmaTyp::Side  => "kcol2.side",
                };

                let mut stmt = conn.prepare(&format!(
                    "SELECT CASE WHEN (
                        EXISTS (SELECT TRUE FROM {table} WHERE name = ?1)
                    ) THEN (
                        SELECT (COUNT(name) + 1) FROM {table} WHERE (
                            {tcol1}
                        ) > (
                            SELECT ({tcol2}) FROM {table} AS kcol2 WHERE kcol2.name = ?2
                        )
                    ) ELSE NULL END",
                    table=table, tcol1=tcol1, tcol2=tcol2
                )).unwrap();
                let mut rows = stmt.query(rs::params![user, user]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let count: i32 = row.get(0).unwrap();

                    println!("Sql Worker - RankingDenormalized - Count: {:?}", count);
                } else {
                    println!("Sql Worker - RankingDenormalized - Error");
                }
            },
            RunQuery::Count(kcol) => {
                println!("Sql Worker - Count - kcol: {:?}", kcol);

                let table = match kcol {
                    Given => "karma_given_count",
                    Recieved => "karma_received_count",
                };

                let mut stmt = conn.prepare(&format!(
                    "SELECT COUNT(name) FROM {table}",
                    table=table
                )).unwrap();
                let mut rows = stmt.query(rs::NO_PARAMS).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let count: u32 = row.get(0).unwrap();

                    println!("Sql Worker - Count - Tally: {:?}", count);
                } else {
                    println!("Sql Worker - Count - Error");
                }
            },
            RunQuery::Partial{
                karma_col: kcol,
                users: users
            } => {
                println!("Sql Worker - Partial - kcol: {:?}, users: {:?}", kcol, users);

                let table = match kcol {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                // Hack to insert enough parameterizers into the query
                let pUsers = {
                    let mut pUsers: String = "?1".to_string();
                    for i in 2..(users.len() + 1) {
                        pUsers.push_str(&format!(", ?{}", i));
                    }
                    pUsers
                };

                // Params
                let param: Vec<&String> = users.iter().collect();

                let mut stmt = conn.prepare(&format!(
                    "SELECT name, up, down, side FROM {table} WHERE name in ({pUsers}) ORDER BY name DESC",
                    table=table, pUsers=pUsers
                )).unwrap();
                let mut rows = stmt.query(&param).unwrap();

                // A bit more additional work than usual
                let mut ret: Vec<(String, i32, i32, i32)> = vec![];
                let mut has: HashSet<String> = HashSet::new();

                while let Ok(Some(row)) = rows.next() {
                    let name: String = row.get(0).unwrap();
                    let up: i32 = row.get(1).unwrap();
                    let down: i32 = row.get(2).unwrap();
                    let side: i32 = row.get(3).unwrap();

                    ret.push((name.clone(), up, down, side));
                    has.insert(name);
                }

                // Evaulate if there's missing ones and add if so
                for n in users.difference(&has) {
                    ret.push((n.to_string(), 0, 0, 0));
                }

                // Print out for records
                println!("Sql Worker - Partial - Dump: {:?}", ret);
            },
        }
    }
}


// TODO: should be able to replace all of these stuff maybe by some trait or some other tricks
// so that we can have a extensible query where i define the query/process/result and then send it
// to the query engine and get the result back in a good format
#[derive(Debug)]
enum RunQuery {
    TopNDenormalized {
        karma_col: KarmaCol,
        karma_typ: KarmaTyp,
        limit: u32,
        ord: OrdQuery
    },
    RankingDenormalized {
        karma_col: KarmaCol,
        karma_typ: KarmaTyp,
        user: String, // TODO: make it support a Set of user
    },
    Count(KarmaCol),
    Partial {
        karma_col: KarmaCol,
        users: HashSet<String>,
    }
}

#[derive(Debug)]
enum OrdQuery { Asc, Desc }

#[derive(Debug)]
enum KarmaCol { Given, Recieved }

#[derive(Debug)]
enum KarmaTyp { Total, Side }



#[derive(Debug)]
enum ResQuery {
    Insert(usize),
    Query(String),
}


async fn process_inbound_message<R>(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>,
    mut sql_tx: mpsc::Sender<(RunQuery, Option<oneshot::Sender<ResQuery>>)>,
    start_time: DateTime<Utc>,
    user_cache: UserCache,
    token: &str,
    client: R
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
                            let ver = build_info::PKG_VERSION;
                            let dat = build_info::BUILT_TIME_UTC;
                            let sha = build_info::GIT_COMMIT_HASH.unwrap_or("Unknown");

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

                            let user_display = get_user_display(&token, client.clone(), &user_cache, &u).await;
                            println!("User id: {:?}, Display: {:?}", u, user_display);


                            sql_tx.send((
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Recieved,
                                    karma_typ: KarmaTyp::Total,
                                    limit: 3,
                                    ord: OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Recieved,
                                    karma_typ: KarmaTyp::Total,
                                    limit: 3,
                                    ord: OrdQuery::Asc
                                },
                                None
                            )).await;

                            // If a list of name is given query this
                            sql_tx.send((
                                RunQuery::Partial {
                                    karma_col: KarmaCol::Recieved,
                                    users: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()].into_iter().collect(),
                                },
                                None
                            )).await;

//                            // Dummy get/send stuff
//                            let (tx1, rx1) = oneshot::channel();
//                            sql_tx.send((
//                                RunQuery::Insert("hi".to_string(), "bye".to_string()),
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
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Given,
                                    karma_typ: KarmaTyp::Total,
                                    limit: 3,
                                    ord: OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Given,
                                    karma_typ: KarmaTyp::Total,
                                    limit: 3,
                                    ord: OrdQuery::Asc
                                },
                                None
                            )).await;

                            // If a list of name is given query this
                            sql_tx.send((
                                RunQuery::Partial {
                                    karma_col: KarmaCol::Given,
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
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Recieved,
                                    karma_typ: KarmaTyp::Side,
                                    limit: 3,
                                    ord: OrdQuery::Desc
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::TopNDenormalized {
                                    karma_col: KarmaCol::Given,
                                    karma_typ: KarmaTyp::Side,
                                    limit: 3,
                                    ord: OrdQuery::Desc
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
                                RunQuery::RankingDenormalized {
                                    karma_col: KarmaCol::Recieved,
                                    karma_typ: KarmaTyp::Total,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::Count(KarmaCol::Recieved),
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::RankingDenormalized {
                                    karma_col: KarmaCol::Given,
                                    karma_typ: KarmaTyp::Total,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::Count(KarmaCol::Given),
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
                                RunQuery::RankingDenormalized {
                                    karma_col: KarmaCol::Recieved,
                                    karma_typ: KarmaTyp::Side,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::Count(KarmaCol::Recieved),
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::RankingDenormalized {
                                    karma_col: KarmaCol::Given,
                                    karma_typ: KarmaTyp::Side,
                                    user: "a".to_string(),
                                },
                                None
                            )).await;
                            sql_tx.send((
                                RunQuery::Count(KarmaCol::Given),
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

async fn get_user_display<R>(
    token: &str,
    client: R,
    user_cache: &UserCache,
    user_id: &String
) -> Option<String>
where
    R: slack::requests::SlackWebRequestSender
{
    let ud = user_cache.get(user_id);
    match ud {
        Some(ud) => Some(ud.clone()),
        None     => {
            let resp = slack_api::users::info(
                &client,
                &token,
                &slack_api::users::InfoRequest { user: &user_id }
            ).await;

            let ud = resp.ok().map(
                |ui| ui.user
            ).flatten().map(
                |ui| ui.name
            ).flatten();

            match ud {
                Some(ud) => {
                    user_cache.insert(user_id.clone(), ud.clone());
                    Some(ud)
                },
                _ => None,
            }
        },
    }
}

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
