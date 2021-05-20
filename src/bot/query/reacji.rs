use rusqlite as rs;
use slack_api as slack;

use chrono::prelude::{Utc, DateTime};

use std::result::Result;

use tokio::sync::mpsc;

use crate::bot::parser::karma::Karma;
use crate::bot::parser::reacji_to_karma;

use crate::bot::query::santizer;
use crate::bot::query::{KarmaName, ReacjiAction};

use crate::core::cache;

use crate::core::database::Query;
use crate::core::database::send_query;


pub async fn add_reacji<R>(
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
            let message_id = query_reacji_message(
                sql_tx,
                channel_id.clone(),
                ts.clone()
            ).await.ok().flatten();

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
                                    add_reacji_message(
                                        sql_tx,
                                        user_id,
                                        KarmaName::new(&ud),
                                        KarmaName::new(&rn),
                                        channel_id.clone(),
                                        ts.clone(),
                                        santized_text
                                    ).await.ok().flatten()
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
                            let _ = add_reacji_query(
                                sql_tx,
                                Utc::now(),
                                user_id,
                                KarmaName::new(&ud),
                                KarmaName::new(&rn),
                                action,
                                mid,
                                karma,
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


async fn query_reacji_message(
    sql_tx: &mut mpsc::Sender<Query>,
    channel_id: String,
    message_ts: String,
) -> Result<Option<i64>, &'static str> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let channel_id: Option<i64> = {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?").unwrap();
                let mut rows = stmt.query(rs::params![channel_id]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).ok()
                } else {
                    None
                }
            };

            if let Some(channel_id) = channel_id {
                // We good let's proceed, otherwise abort since if channel id isn't here its
                // not going to be in reacji_message either
                let mut stmt = conn.prepare("SELECT id FROM reacji_message WHERE ts = ? AND chan_id = ?").unwrap();
                let mut rows = stmt.query(rs::params![message_ts, channel_id]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let msg_id: Option<i64> = row.get(0).ok();
                    Ok(msg_id)
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        })
    ).await
}


async fn add_reacji_message(
    sql_tx: &mut mpsc::Sender<Query>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: String,
    message_ts: String,
    message: String,
) -> Result<Option<i64>, &'static str> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let nick_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM nick_metadata WHERE username = ?").unwrap();
                let mut rows = stmt.query(rs::params![user_id]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).unwrap()
                } else {
                    let mut stmt = conn.prepare(
                        "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?)"
                    ).unwrap();
                    stmt.insert(rs::params![username, real_name, user_id, "SlackServer"]).unwrap()
                }
            };

            let channel_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?").unwrap();
                let mut rows = stmt.query(rs::params![channel_id]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).unwrap()
                } else {
                    let mut stmt = conn.prepare("INSERT INTO chan_metadata (channel) VALUES (?)").unwrap();
                    stmt.insert(rs::params![channel_id]).unwrap()
                }
            };

            // Insert the reacji_message content now
            let mut stmt = conn.prepare(
                "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES (?, ?, ?, ?)"
            ).unwrap();

            Ok(
                Some(stmt.insert(rs::params![message_ts, channel_id, nick_id, message]).unwrap())
            )
        })
    ).await
}


async fn add_reacji_query(
    sql_tx: &mut mpsc::Sender<Query>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    action: ReacjiAction,
    message_id: i64,
    amount: Karma,
) -> Result<Option<i64>, &'static str> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let nick_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM nick_metadata WHERE username = ?").unwrap();
                let mut rows = stmt.query(rs::params![user_id]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).unwrap()
                } else {
                    let mut stmt = conn.prepare(
                        "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?)"
                    ).unwrap();

                    stmt.insert(rs::params![username, real_name, user_id, "SlackServer"]).unwrap()
                }
            };

            let ts = timestamp.to_rfc3339();
            let ts = ts.trim_end_matches("+00:00");

            // Insert the reacji into the database
            let mut stmt = conn.prepare(
                "INSERT INTO reacji_votes
                    (voted_at, by_whom_name, action, reacji_message_id, amount, nick_id)
                VALUES
                    (?, ?, ?, ?, ?, ?)"
            ).unwrap();

            stmt.insert(rs::params![ts, username, action, message_id, amount, nick_id]).unwrap();

            Ok(None)
        })
    ).await
}
