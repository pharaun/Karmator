use rusqlite as rs;

use tokio::sync::mpsc;

use std::collections::HashSet;

use crate::bot::query::{KarmaCol, KarmaName};
use crate::bot::user_event::Event;

use crate::core::database::DbResult;
use crate::core::database::Query;
use crate::core::database::send_query;


pub async fn partial(
    event: &mut Event,
    sql_tx: &mut mpsc::Sender<Query>,
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
        Ok(x) => event.send_reply(&x).await,
        _ => event.send_reply("Something went wrong").await,
    };
}


async fn partial_query(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> DbResult<Vec<(String, i32, i32, i32)>> {
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
            ))?;
            let mut rows = stmt.query(rs::params_from_iter(param))?;

            // A bit more additional work than usual
            let mut ret: Vec<(String, i32, i32, i32)> = vec![];
            let mut has: HashSet<KarmaName> = HashSet::new();

            while let Ok(Some(row)) = rows.next() {
                let name: String = row.get(0)?;
                let up: i32 = row.get(1)?;
                let down: i32 = row.get(2)?;
                let side: i32 = row.get(3)?;

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
