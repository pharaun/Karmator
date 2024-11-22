use rusqlite as rs;

use chrono::prelude::{Utc, DateTime};

use tokio::sync::mpsc;
use tokio_postgres::Client;
use std::sync::Arc;

use crate::bot::parser::karma::KST;
use crate::bot::parser::karma;

use crate::bot::query::KarmaName;
use crate::bot::query::normalize;
use crate::bot::user_event::Event;

use crate::core::database::DbResult;
use crate::core::database::Query;
use crate::core::database::send_query;


pub async fn add_karma(
    client: Arc<Client>,
    event: &Event,
) {
    match karma::parse(&event.santize().await) {
        Ok(mut karma) if !karma.is_empty() => {
            println!("INFO [User Event]: Parsed Karma: {:?}", karma);
            let username = event.get_username().await;
            let user_real_name = event.get_user_real_name().await;

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
                            client.clone(),
                            Utc::now(),
                            event.user_id.clone(),
                            KarmaName::new(&ud),
                            KarmaName::new(&rn),
                            Some(event.channel_id.clone()),
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
    client: Arc<Client>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
) -> DbResult<Option<i64>> {
    send_query(
        client.clone(),
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

            let channel_id: Option<i64> = if let Some(cid) = channel_id {
                let mut stmt = conn.prepare("SELECT id FROM chan_metadata WHERE channel = ?")?;
                let mut rows = stmt.query(rs::params![cid])?;

                if let Ok(Some(row)) = rows.next() {
                    row.get(0).ok()
                } else {
                    let mut stmt = conn.prepare("INSERT INTO chan_metadata (channel) VALUES (?)")?;
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
                )?;

                for KST(karma_text, amount) in karma.iter() {
                    let ts = timestamp.to_rfc3339();
                    let ts = ts.trim_end_matches("+00:00");
                    let karma_text = normalize(karma_text);

                    // TODO: better handle failure of username, maybe we should make username
                    // mandatory before inserting?
                    match channel_id {
                        Some(cid) => stmt.insert(
                            rs::params![ts, username, karma_text, amount, nick_id, cid]
                        )?,
                        None      => stmt.insert(
                            rs::params![ts, username, karma_text, amount, nick_id, &rs::types::Null]
                        )?,
                    };
                }
            }
            let _ = txn.commit();
            Ok(None)
        })
    ).await
}
