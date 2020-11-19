use tokio::sync::mpsc;

use std::default::Default;
use std::result::Result;

// SQlite worker thread
use std::thread;
use futures::executor::block_on_stream;

use rusqlite as rs;
use std::path::Path;
use tokio::sync::oneshot;
use std::collections::HashSet;


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
        user: String, // TODO: make it support a Set of user
    },
    Count(KarmaCol),
    Partial {
        karma_col: KarmaCol,
        users: HashSet<String>,
    }
}

#[derive(Debug)]
pub enum OrdQuery { Asc, Desc }

#[derive(Debug)]
pub enum KarmaCol { Given, Recieved }

#[derive(Debug)]
pub enum KarmaTyp { Total, Side }


#[derive(Debug)]
pub enum ResQuery {
    Insert(usize),
    Query(String),
}


pub fn process_queries(
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
    crate::schema_sample::setup_table(&conn);
    crate::schema_sample::setup_data(&conn);


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
