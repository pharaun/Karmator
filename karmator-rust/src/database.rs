use tokio::sync::mpsc;

// SQlite worker thread
use futures::executor::block_on_stream;

use rusqlite as rs;
use tokio::sync::oneshot;
use std::collections::HashSet;
use std::path::Path;
use unicase::UniCase;

use unicode_normalization::{
    UnicodeNormalization,
    is_nfc_quick,
    IsNormalized,
};

use chrono::prelude::{Utc, DateTime};

use crate::parser::karma::KST;
use crate::parser::karma::Karma;


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
        username: Option<KarmaName>,
        real_name: Option<KarmaName>,
        channel_id: Option<String>,
        karma: Vec<KST>,
    },
}

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

// TODO: additional check for shutting down
pub fn process_queries(
    filename: &Path,
    sql_rx: mpsc::Receiver<(RunQuery, Option<oneshot::Sender<ResQuery>>)>
) -> Result<(), Box<dyn std::error::Error>> {
    let mut block_sql_rx = block_on_stream(sql_rx);

    let mut conn = rs::Connection::open_with_flags(
        filename,
        rs::OpenFlags::SQLITE_OPEN_READ_WRITE
    ).expect(&format!("Connection error: {:?}", filename.to_str()));

    // Listen for inbound query
    while let Some((query, res_tx)) = block_sql_rx.next() {
        match query {
            RunQuery::TopNDenormalized{karma_col, karma_typ, limit, ord} => {
                println!(
                    "Sql Worker - TopNDenormalized - karma_col: {:?}, karma_typ: {:?}, limit: {:?}, ord: {:?}",
                    karma_col, karma_typ, limit, ord
                );

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

                // Print out for records
                println!("Sql Worker - TopNDenormalized - Dump: {:?}", &ret);
                maybe_send(res_tx, ResQuery::TopN(ret))?;
            },
            RunQuery::RankingDenormalized{karma_col, karma_typ, user} => {
                println!("Sql Worker - RankingDenormalized - karma_col: {:?}, karma_typ: {:?}, user: {:?}", karma_col, karma_typ, user);

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

                    maybe_send(res_tx, ResQuery::OCount(count))?;
                    println!("Sql Worker - RankingDenormalized - Count: {:?}", count);
                } else {
                    println!("Sql Worker - RankingDenormalized - Error");
                }
            },
            RunQuery::Count(karma_col) => {
                println!("Sql Worker - Count - karma_col: {:?}", karma_col);

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

                    maybe_send(res_tx, ResQuery::Count(count))?;
                    println!("Sql Worker - Count - Tally: {:?}", count);
                } else {
                    println!("Sql Worker - Count - Error");
                }
            },
            RunQuery::Partial{karma_col, users} => {
                println!("Sql Worker - Partial - karma_col: {:?}, users: {:?}", karma_col, users);

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

                // Print out for records
                println!("Sql Worker - Partial - Dump: {:?}", &ret);
                maybe_send(res_tx, ResQuery::Partial(ret))?;
            },
            RunQuery::AddKarma{timestamp, user_id, username, real_name, channel_id, karma} => {
                println!(
                    "Sql Worker - AddKarma - timestamp: {:?}, user_id: {:?}, username: {:?}, real_name: {:?}, channel_id: {:?}, karma: {:?}",
                    timestamp, user_id, username, real_name, channel_id, karma
                );
                let un = username.map(|un| un.to_string()).unwrap_or("".to_string());
                let urn = real_name.map(|rn| rn.to_string()).unwrap_or("".to_string());

                // TODO: find out which column we use (and which has slack id vs user name)
                let nick_id: i64 = {
                    let mut stmt = conn.prepare("SELECT id FROM nick_metadata WHERE username = ?").unwrap();
                    let mut rows = stmt.query(rs::params![user_id]).unwrap();

                    if let Ok(Some(row)) = rows.next() {
                        row.get(0).unwrap()
                    } else {
                        let mut stmt = conn.prepare(
                            "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?)"
                        ).unwrap();

                        stmt.insert(rs::params![un, urn, user_id, "SlackServer"]).unwrap()
                    }
                };
                println!("Sql Worker - AddKarma - nick_id: {:?}", nick_id);

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
                println!("Sql Worker - AddKarma - channel_id: {:?}", channel_id);

                // Shove the karma into the db now
                println!("Sql Worker - AddKarma - Begin Transaction");
                let tx = conn.transaction()?;
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

                        println!(
                            "Sql Worker - AddKarma - \tInserting Karma - {:?}, {:?}, {:?}",
                            un, karma_text, amount
                        );

                        // TODO: better handle failure of username, maybe we should make username
                        // mandatory before inserting?
                        match channel_id {
                            Some(cid) => stmt.insert(
                                rs::params![ts, un, karma_text, amount, nick_id, cid]
                            ).unwrap(),
                            None      => stmt.insert(
                                rs::params![ts, un, karma_text, amount, nick_id, &rs::types::Null]
                            ).unwrap(),
                        };
                    }
                }
                tx.commit()?;
                println!("Sql Worker - AddKarma - End Transaction");
            },

        }
    }

    Ok(())
}
