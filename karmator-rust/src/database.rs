use tokio::sync::mpsc;

// SQlite worker thread
use futures::executor::block_on_stream;

use rusqlite as rs;
use tokio::sync::oneshot;
use std::collections::HashSet;

use crate::schema_sample;


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


pub fn process_queries(
    sql_rx: mpsc::Receiver<(RunQuery, Option<oneshot::Sender<ResQuery>>)>
) -> Result<(), Box<dyn std::error::Error>> {
    let mut block_sql_rx = block_on_stream(sql_rx);

    //let filename = "db.sqlite";
    //let conn = rs::Connection::open_with_flags(
    //    Path::new(filename),
    //    rs::OpenFlags::SQLITE_OPEN_READ_WRITE
    //).expect(&format!("Connection error: {}", filename));
    let conn = rs::Connection::open_in_memory().expect("Failure");

    // Test data/build tables
    schema_sample::setup_table(&conn)?;
    schema_sample::setup_data(&conn)?;


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
                let param: Vec<&String> = users.iter().collect();

                let mut stmt = conn.prepare(&format!(
                    "SELECT name, up, down, side FROM {table} WHERE name in ({p_user}) ORDER BY name DESC",
                    table=table, p_user=p_user
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
                println!("Sql Worker - Partial - Dump: {:?}", &ret);
                maybe_send(res_tx, ResQuery::Partial(ret))?;
            },
        }
    }

    Ok(())
}
