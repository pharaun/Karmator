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


#[derive(Debug, Clone, Copy)]
pub enum ReacjiAction { Add, Del }

#[derive(Debug, Clone, Copy)]
pub enum OrdQuery { Asc, Desc }

#[derive(Debug, Clone, Copy)]
pub enum KarmaCol { Given, Recieved }

#[derive(Debug, Clone, Copy)]
pub enum KarmaTyp { Total, Side }



fn maybe_send<T>(
    res_tx: oneshot::Sender<T>,
    res: T,
) -> Result<(), String> {
    res_tx.send(res).map_err(|_| "Cant send ResQuery".to_string())
}


pub async fn top_n_denormalized(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    limit: u32,
    ord: OrdQuery
) -> Result<Vec<(String, i32)>, &'static str> {
    let (tx, rx) = oneshot::channel();

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
        maybe_send(tx, ret)
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn ranking_denormalized(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    user: KarmaName
) -> Result<Option<u32>, &'static str> {
    let (tx, rx) = oneshot::channel();

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

            maybe_send(tx, count)
        } else {
            Err(format!(
                "ERROR [Sql Worker]: RankingDenormalized - karma_col: {:?}, karma_typ: {:?}, user: {:?}",
                karma_col, karma_typ, user
            ))
        }
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn count(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
) -> Result<u32, &'static str> {
    let (tx, rx) = oneshot::channel();

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

            maybe_send(tx, count)
        } else {
            Err(format!("ERROR [Sql Worker]: Count - ERROR - karma_col: {:?}", karma_col))
        }
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn partial(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> Result<Vec<(String, i32, i32, i32)>, &'static str> {
    let (tx, rx) = oneshot::channel();

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
        maybe_send(tx, ret)
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}



pub async fn add_karma(
    sql_tx: &mut mpsc::Sender<Query>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
) -> Result<Option<i64>, &'static str> {
    let (tx, rx) = oneshot::channel();

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
        let txn = conn.transaction().expect("txn error");
        {
            let mut stmt = txn.prepare(
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
        let _ = txn.commit();
        maybe_send::<Option<i64>>(tx, None)
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn query_reacji_message(
    sql_tx: &mut mpsc::Sender<Query>,
    channel_id: String,
    message_ts: String,
) -> Result<Option<i64>, &'static str> {
    let (tx, rx) = oneshot::channel();

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
                let msg_id: Option<i64> = row.get(0).ok();
                maybe_send(tx, msg_id)
            } else {
                maybe_send::<Option<i64>>(tx, None)
            }
        } else {
            maybe_send::<Option<i64>>(tx, None)
        }
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn add_reacji_message(
    sql_tx: &mut mpsc::Sender<Query>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: String,
    message_ts: String,
    message: String,
) -> Result<Option<i64>, &'static str> {
    let (tx, rx) = oneshot::channel();

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
            tx,
            Some(stmt.insert(rs::params![message_ts, channel_id, nick_id, message]).unwrap())
        )
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


pub async fn add_reacji(
    sql_tx: &mut mpsc::Sender<Query>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    action: ReacjiAction,
    message_id: i64,
    result: Karma,
) -> Result<Option<i64>, &'static str> {
    let (tx, rx) = oneshot::channel();

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

        maybe_send::<Option<i64>>(tx, None)
    };

    sql_tx.send(Box::new(query)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}
