use rusqlite as rs;

use chrono::prelude::{Utc, DateTime};

use tokio::sync::mpsc;

use crate::bot::parser::karma::Karma;
use crate::bot::parser::reacji_to_karma;

use crate::bot::query::{KarmaName, ReacjiAction};
use crate::bot::user_event::Event;

use crate::core::database::DbResult;
use crate::core::database::Query;
use crate::core::database::send_query;


pub async fn add_reacji(
    sql_tx: &mut mpsc::Sender<Query>,
    event: &mut Event,
    input: &str,
    action: ReacjiAction,
) {
    match reacji_to_karma(input) {
        Some(karma) => {
            let message_id = query_reacji_message(
                sql_tx,
                event.channel_id.clone(),
                event.thread_ts.clone().unwrap()
            ).await.map_or_else(
                |e| Err(e),
                |v| v.ok_or("NONE".to_string())
            );

            let message_id = match message_id {
                Err(_) => match event.get_message().await {
                    Ok(message_user_id) => {
                        // We have the message content, insert it into the table and
                        // get its row id
                        let santized_text = event.santize().await;

                        match (
                            event.get_other_username(&message_user_id).await,
                            event.get_other_user_real_name(&message_user_id).await,
                        ) {
                            (Some(ud), Some(rn)) => {
                                add_reacji_message(
                                    sql_tx,
                                    message_user_id,
                                    KarmaName::new(&ud),
                                    KarmaName::new(&rn),
                                    event.channel_id.clone(),
                                    // TODO: should add error check here
                                    event.thread_ts.clone().unwrap(),
                                    santized_text
                                ).await
                            },
                            e => Err(format!("ERROR: [User Event] Querying for user/name failed: {:?}", e)),
                        }
                    },

                    // This happens when there is no user_id (is none)
                    // This shouldn't happen but .... slack....
                    Err(_) => Ok(None),
                },
                e => Err(format!("ERROR: [User Event] IDK here: {:?}", e)),
            };

            match message_id {
                Err(e) => {
                    eprintln!("ERROR: [User Event] Wasn't able to get/store an reactji message, vote isn't recorded");
                    eprintln!("ERROR: [User Event] Returned error: {:?}", e);
                },
                Ok(None) => (), // These are expected error, drop
                Ok(Some(mid)) => {
                    match (
                        event.get_username().await,
                        event.get_user_real_name().await,
                    ) {
                        (Some(ud), Some(rn)) => {
                            let e = add_reacji_query(
                                sql_tx,
                                Utc::now(),
                                event.user_id.clone(),
                                KarmaName::new(&ud),
                                KarmaName::new(&rn),
                                action,
                                mid,
                                karma,
                            ).await;

                            if e.is_err() {
                                eprintln!("ERROR: [User Event] Query failed: {:?}", e);
                            }
                        },
                        e => eprintln!("ERROR: [User Event] Querying for user/name failed: {:?}", e),
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
) -> DbResult<Option<i64>> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let channel_id: Option<i64> = {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?")?;
                let mut rows = stmt.query(rs::params![channel_id])?;

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).ok()
                } else {
                    None
                }
            };

            if let Some(channel_id) = channel_id {
                // We good let's proceed, otherwise abort since if channel id isn't here its
                // not going to be in reacji_message either
                let mut stmt = conn.prepare("SELECT id FROM reacji_message WHERE ts = ? AND chan_id = ?")?;
                let mut rows = stmt.query(rs::params![message_ts, channel_id])?;

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
) -> DbResult<Option<i64>> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let nick_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM nick_metadata WHERE username = ?")?;
                let mut rows = stmt.query(rs::params![user_id])?;

                if let Ok(Some(row)) = rows.next() {
                    row.get(0)?
                } else {
                    let mut stmt = conn.prepare(
                        "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?)"
                    )?;
                    stmt.insert(rs::params![username, real_name, user_id, "SlackServer"])?
                }
            };

            let channel_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?")?;
                let mut rows = stmt.query(rs::params![channel_id])?;

                if let Ok(Some(row)) = rows.next() {
                    row.get(0)?
                } else {
                    let mut stmt = conn.prepare("INSERT INTO chan_metadata (channel) VALUES (?)")?;
                    stmt.insert(rs::params![channel_id])?
                }
            };

            // Insert the reacji_message content now
            let reacji_message_id: i64 = {
                let mut stmt = conn.prepare("SELECT id, nick_id, message FROM reacji_message WHERE ts = ? AND chan_id = ?")?;
                let mut rows = stmt.query(rs::params![message_ts, channel_id])?;

                if let Ok(Some(row)) = rows.next() {
                    // Compare the 2 and if its not the same, warn in log, otherwise return
                    let id = row.get(0)?;
                    let sql_nick: i64 = row.get(1)?;
                    let sql_message: String = row.get(2)?;

                    // Compare
                    if sql_nick != nick_id || sql_message != message {
                        eprintln!("ERROR: [Reacji Message] duplicate channel + ts");
                        eprintln!("ERROR: [Reacji Message] \tSlack Nick: {}", nick_id);
                        eprintln!("ERROR: [Reacji Message] \tSql Nick:   {}", sql_nick);
                        eprintln!("ERROR: [Reacji Message] \tSlack Msg:  {}", message);
                        eprintln!("ERROR: [Reacji Message] \tSql Msg:    {}", sql_message);
                    }

                    // Return one anyway for now
                    id
                } else {
                    let mut stmt = conn.prepare(
                        "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES (?, ?, ?, ?)"
                    )?;
                    stmt.insert(rs::params![message_ts, channel_id, nick_id, message])?
                }
            };

            Ok(Some(reacji_message_id))
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
) -> DbResult<Option<i64>> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let nick_id: i64 = {
                let mut stmt = conn.prepare("SELECT id FROM nick_metadata WHERE username = ?")?;
                let mut rows = stmt.query(rs::params![user_id])?;

                if let Ok(Some(row)) = rows.next() {
                    row.get(0)?
                } else {
                    let mut stmt = conn.prepare(
                        "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?)"
                    )?;

                    stmt.insert(rs::params![username, real_name, user_id, "SlackServer"])?
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
            )?;

            stmt.insert(rs::params![ts, username, action, message_id, amount, nick_id])?;

            Ok(None)
        })
    ).await
}
