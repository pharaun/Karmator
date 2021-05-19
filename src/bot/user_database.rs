use rusqlite as rs;
use tokio::sync::oneshot;
use std::collections::HashSet;
use unicase::UniCase;

use unicode_normalization::{
    UnicodeNormalization,
    is_nfc_quick,
    IsNormalized,
};

use chrono::prelude::{Utc, DateTime};

use crate::bot::parser::karma::KST;
use crate::bot::parser::karma::Karma;
use crate::core::database::Query;
use tokio::sync::mpsc;


pub async fn send_query(
    sql_tx: &mut mpsc::Sender<Query>,
    query: RunQuery,
) -> Result<ResQuery, &'static str> {
    let (tx, rx) = oneshot::channel();

    let c_query = query_to_closure(
        query,
        Some(tx)
    ).map_err(|_| "Error converting")?;

    sql_tx.send(c_query).await.map_err(|_| "Error sending")?;
    rx.await.map_err(|_| "Error recieving")
}

pub async fn send_query_commit(
    sql_tx: &mut mpsc::Sender<Query>,
    query: RunQuery,
) -> Result<(), &'static str> {
    let c_query = query_to_closure(
        query,
        None
    ).map_err(|_| "Error converting")?;

    sql_tx.send(c_query).await.map_err(|_| "Error sending")
}

// Normalize any incoming string to be stored in the database
fn normalize(input: &str) -> String {
    match is_nfc_quick(input.chars()) {
        IsNormalized::Yes => input.to_string(),
        _ => input.nfc().collect(),
    }
}

// Custom Type to handle unicase for the query users
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct KarmaName(UniCase<String>);

impl KarmaName {
    pub fn new(name: &str) -> KarmaName {
        KarmaName(UniCase::new(normalize(name)))
    }
}

impl ToString for KarmaName {
    fn to_string(&self) -> String {
        let KarmaName(n) = self;
        n.to_string()
    }
}

impl rs::ToSql for KarmaName {
    fn to_sql(&self) -> rs::Result<rs::types::ToSqlOutput<'_>> {
        Ok(rs::types::ToSqlOutput::from(self.to_string()))
    }
}


// TODO: should be able to replace all of these stuff maybe by some trait or some other tricks
// so that we can have a extensible query where i define the query/process/result and then send it
// to the query engine and get the result back in a good format
#[derive(Debug)]
pub enum RunQuery {
    TopNDenormalized {
        karma_col: KarmaCol,
        karma_typ: KarmaTyp,
        limit: u32,
        ord: OrdQuery
    },
    RankingDenormalized {
        karma_col: KarmaCol,
        karma_typ: KarmaTyp,
        user: KarmaName, // TODO: make it support a Set of user
    },
    Count(KarmaCol),
    Partial {
        karma_col: KarmaCol,
        users: HashSet<KarmaName>,
    },
    AddKarma {
        timestamp: DateTime<Utc>,
        user_id: String,
        username: KarmaName,
        real_name: KarmaName,
        channel_id: Option<String>,
        karma: Vec<KST>,
    },
    QueryReacjiMessage{
        channel_id: String,
        message_ts: String,
    },
    AddReacjiMessage {
        user_id: String,
        username: KarmaName,
        real_name: KarmaName,
        channel_id: String,
        message_ts: String,
        message: String,
    },
    AddReacji {
        timestamp: DateTime<Utc>,
        user_id: String,
        username: KarmaName,
        real_name: KarmaName,
        action: ReacjiAction,
        message_id: i64,
        result: Karma,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ReacjiAction { Add, Del }

#[derive(Debug, Clone, Copy)]
pub enum OrdQuery { Asc, Desc }

#[derive(Debug, Clone, Copy)]
pub enum KarmaCol { Given, Recieved }

#[derive(Debug, Clone, Copy)]
pub enum KarmaTyp { Total, Side }


#[derive(Debug)]
pub enum ResQuery {
    // RankingDenormalized
    OCount(Option<u32>),
    // Count
    Count(u32),
    // TopNDenormalized
    TopN(Vec<(String, i32)>),
    // Partial
    Partial(Vec<(String, i32, i32, i32)>),
    // Message Id for query reacji message -and- add reacji message
    MessageId(Option<i64>),
}


fn maybe_send(
    res_tx: Option<oneshot::Sender<ResQuery>>,
    res: ResQuery,
) -> Result<(), String> {
    match res_tx {
        Some(tx) => tx.send(res).map_err(|_| "Cant send ResQuery".to_string()),
        None     => Ok(()),
    }
}

// For now transform the incoming queries and etc into closure to then feed to the core engine
pub fn query_to_closure(
    query: RunQuery,
    res_tx: Option<oneshot::Sender<ResQuery>>
) -> Result<Query, Box<dyn std::error::Error>> {
    match query {
        RunQuery::TopNDenormalized{karma_col, karma_typ, limit, ord} => {
            let query = move |conn: &mut rs::Connection| {
                let table = match karma_col {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                let t_col = match karma_typ {
                    KarmaTyp::Total => "up - down",
                    KarmaTyp::Side  => "side",
                };

                let q_ord = match ord {
                    OrdQuery::Asc  => "ASC",
                    OrdQuery::Desc => "DESC",
                };

                let mut stmt = conn.prepare(&format!(
                        "SELECT name, {t_col} as total FROM {table} ORDER BY total {q_ord} LIMIT {limit}",
                        t_col=t_col,
                        table=table,
                        q_ord=q_ord,
                        limit=limit
                )).unwrap();
                let mut rows = stmt.query(rs::NO_PARAMS).unwrap();

                let mut ret: Vec<(String, i32)> = vec![];

                while let Ok(Some(row)) = rows.next() {
                    let name: String = row.get(0).unwrap();
                    let count: i32 = row.get(1).unwrap();

                    ret.push((name.clone(), count));
                }
                maybe_send(res_tx, ResQuery::TopN(ret))
            };

            Ok(Box::new(query))
        },
        RunQuery::RankingDenormalized{karma_col, karma_typ, user} => {
            let query = move |conn: &mut rs::Connection| {
                let table = match karma_col {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                let t_col1 = match karma_typ {
                    KarmaTyp::Total => "up - down",
                    KarmaTyp::Side  => "side",
                };

                let t_col2 = match karma_typ {
                    KarmaTyp::Total => "kcol2.up - kcol2.down",
                    KarmaTyp::Side  => "kcol2.side",
                };

                let mut stmt = conn.prepare(&format!(
                    "SELECT CASE WHEN (
                        EXISTS (SELECT TRUE FROM {table} WHERE name = ?1)
                    ) THEN (
                        SELECT (COUNT(name) + 1) FROM {table} WHERE (
                            {t_col1}
                        ) > (
                            SELECT ({t_col2}) FROM {table} AS kcol2 WHERE kcol2.name = ?2
                        )
                    ) ELSE NULL END",
                    table=table, t_col1=t_col1, t_col2=t_col2
                )).unwrap();
                let user = user.to_string();
                let mut rows = stmt.query(rs::params![user, user]).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let count: Option<u32> = row.get(0).ok();

                    maybe_send(res_tx, ResQuery::OCount(count))
                } else {
                    Err(format!(
                        "ERROR [Sql Worker]: RankingDenormalized - karma_col: {:?}, karma_typ: {:?}, user: {:?}",
                        karma_col, karma_typ, user
                    ))
                }
            };

            Ok(Box::new(query))
        },
        RunQuery::Count(karma_col) => {
            let query = move |conn: &mut rs::Connection| {
                let table = match karma_col {
                    KarmaCol::Given => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                let mut stmt = conn.prepare(&format!(
                    "SELECT COUNT(name) FROM {table}",
                    table=table
                )).unwrap();
                let mut rows = stmt.query(rs::NO_PARAMS).unwrap();

                if let Ok(Some(row)) = rows.next() {
                    let count: u32 = row.get(0).unwrap();

                    maybe_send(res_tx, ResQuery::Count(count))
                } else {
                    Err(format!("ERROR [Sql Worker]: Count - ERROR - karma_col: {:?}", karma_col))
                }
            };
            Ok(Box::new(query))
        },
        RunQuery::Partial{karma_col, users} => {
            let query = move |conn: &mut rs::Connection| {
                let table = match karma_col {
                    KarmaCol::Given    => "karma_given_count",
                    KarmaCol::Recieved => "karma_received_count",
                };

                // Hack to insert enough parameterizers into the query
                let p_user = {
                    let mut p_user: String = "?1".to_string();
                    for i in 2..(users.len() + 1) {
                        p_user.push_str(&format!(", ?{}", i));
                    }
                    p_user
                };

                // Params
                let param: Vec<String> = users.iter().map(|i| i.to_string()).collect();

                let mut stmt = conn.prepare(&format!(
                    "SELECT name, up, down, side FROM {table} WHERE name in ({p_user}) ORDER BY name DESC",
                    table=table, p_user=p_user
                )).unwrap();
                let mut rows = stmt.query(&param).unwrap();

                // A bit more additional work than usual
                let mut ret: Vec<(String, i32, i32, i32)> = vec![];
                let mut has: HashSet<KarmaName> = HashSet::new();

                while let Ok(Some(row)) = rows.next() {
                    let name: String = row.get(0).unwrap();
                    let up: i32 = row.get(1).unwrap();
                    let down: i32 = row.get(2).unwrap();
                    let side: i32 = row.get(3).unwrap();

                    has.insert(KarmaName::new(&name));
                    ret.push((name, up, down, side));
                }

                // Evaulate if there's missing ones and add if so
                // TODO: this should be case insensitive (Ie database can return B and we have b)
                for n in users.difference(&has) {
                    ret.push((n.to_string(), 0, 0, 0));
                }
                maybe_send(res_tx, ResQuery::Partial(ret))
            };

            Ok(Box::new(query))
        },
        RunQuery::AddKarma{timestamp, user_id, username, real_name, channel_id, karma} => {
            let query = move |conn: &mut rs::Connection| {
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
                let tx = conn.transaction().expect("txn error");
                {
                    let mut stmt = tx.prepare(
                        "INSERT INTO votes
                            (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id)
                        VALUES
                            (?, ?, ?, ?, ?, ?)"
                    ).unwrap();

                    for KST(karma_text, k) in karma.iter() {
                        let amount: i8 = match k {
                            Karma::Up   => 1,
                            Karma::Down => -1,
                            Karma::Side => 0,
                        };

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
                let _ = tx.commit();
                Ok(())
            };

            Ok(Box::new(query))
        },
        RunQuery::QueryReacjiMessage{channel_id, message_ts} => {
            let query = move |conn: &mut rs::Connection| {
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
                        maybe_send(
                            res_tx,
                            ResQuery::MessageId(row.get(0).ok())
                        )
                    } else {
                        maybe_send(res_tx, ResQuery::MessageId(None))
                    }
                } else {
                    maybe_send(res_tx, ResQuery::MessageId(None))
                }
            };

            Ok(Box::new(query))
        },
        RunQuery::AddReacjiMessage{user_id, username, real_name, channel_id, message_ts, message} => {
            let query = move |conn: &mut rs::Connection| {
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

                maybe_send(
                    res_tx,
                    ResQuery::MessageId(Some(
                        stmt.insert(rs::params![message_ts, channel_id, nick_id, message]).unwrap()
                    ))
                )
            };

            Ok(Box::new(query))
        },
        RunQuery::AddReacji{timestamp, user_id, username, real_name, action, message_id, result} => {
            let query = move |conn: &mut rs::Connection| {
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

                // Setup the reacji bits
                let amount: i8 = match result {
                    Karma::Up   => 1,
                    Karma::Down => -1,
                    Karma::Side => 0,
                };

                let action: i8 = match action {
                    ReacjiAction::Add => 1,
                    ReacjiAction::Del => -1,
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

                Ok(())
            };

            Ok(Box::new(query))
        },
    }
}
