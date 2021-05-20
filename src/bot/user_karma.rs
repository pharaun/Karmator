use rusqlite as rs;
use slack_api as slack;

use chrono::prelude::{Utc, DateTime};

use std::result::Result;

use tokio::sync::mpsc;

use crate::bot::parser::karma;
use crate::bot::user_database::KarmaName;
use crate::core::cache;
use crate::core::database::Query;
use crate::core::database::send_query;
use crate::bot::parser::karma::KST;

use crate::bot::user_event::santizer;
use crate::bot::user_database::normalize;

pub async fn add_karma<R>(
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
                        let _ = add_karma_query(
                            sql_tx,
                            Utc::now(),
                            user_id,
                            KarmaName::new(&ud),
                            KarmaName::new(&rn),
                            Some(channel_id),
                            karma
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


async fn add_karma_query(
    sql_tx: &mut mpsc::Sender<Query>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
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

            let channel_id: Option<i64> = if let Some(cid) = channel_id {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?").unwrap();
                let mut rows = stmt.query(rs::params![cid]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).ok()
                } else {
                    let mut stmt = conn.prepare("INSERT INTO chan_metadata (channel) VALUES (?)").unwrap();
                    stmt.insert(rs::params![cid]).ok()
                }
            } else {
                None
            };

            // Shove the karma into the db now
            let txn = conn.transaction().expect("txn error");
            {
                let mut stmt = txn.prepare(
                    "INSERT INTO votes
                        (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id)
                    VALUES
                        (?, ?, ?, ?, ?, ?)"
                ).unwrap();

                for KST(karma_text, amount) in karma.iter() {
                    let ts = timestamp.to_rfc3339();
                    let ts = ts.trim_end_matches("+00:00");
                    let karma_text = normalize(karma_text);

                    // TODO: better handle failure of username, maybe we should make username
                    // mandatory before inserting?
                    match channel_id {
                        Some(cid) => stmt.insert(
                            rs::params![ts, username, karma_text, amount, nick_id, cid]
                        ).unwrap(),
                        None      => stmt.insert(
                            rs::params![ts, username, karma_text, amount, nick_id, &rs::types::Null]
                        ).unwrap(),
                    };
                }
            }
            let _ = txn.commit();
            Ok(None)
        })
    ).await
}
