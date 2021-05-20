use tokio_tungstenite as tungstenite;
use rusqlite as rs;

use tokio::sync::mpsc;

use std::result::Result;
use std::collections::HashSet;


use crate::bot::user_database::{KarmaCol, KarmaName};
use crate::core::event::MsgId;
use crate::core::database::send_query;
use crate::core::event::send_simple_message;
use crate::core::database::Query;


pub async fn partial(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    sql_tx: &mut mpsc::Sender<Query>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    let res = partial_query(
        sql_tx,
        kcol,
        arg.into_iter().map(|i| KarmaName::new(i)).collect(),
    ).await.map(|e| {
        e.iter().map(|(entity, up, down, side)| {
            format!(
                "{}, {} ({}++/{}--/{}+-)",
                entity, (up - down), up, down, side
            )
        }).collect::<Vec<String>>().join("; ")
    });

    // TODO: do something here
    let _ = match res {
        Ok(x) => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}", user, x),
        ).await,
        _ => send_simple_message(
            msg_id,
            tx,
            channel,
            thread_ts,
            format!("<@{}>: {}", user, "Something went wrong"),
        ).await,
    };
}


async fn partial_query(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> Result<Vec<(String, i32, i32, i32)>, &'static str> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
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
                table=karma_col, p_user=p_user
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
            Ok(ret)
        })
    ).await
}
