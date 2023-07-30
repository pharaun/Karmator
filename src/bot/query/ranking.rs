use rusqlite as rs;

use tokio::sync::mpsc;

use crate::bot::query::{KarmaCol, KarmaTyp, KarmaName};

use crate::core::database::DbResult;
use crate::core::database::Query;
use crate::core::database::send_query;

use crate::core::event::Reply;
use crate::core::event::send_simple_message;


pub async fn ranking(
    tx: &mut mpsc::Sender<Reply>,
    sql_tx: &mut mpsc::Sender<Query>,
    channel: String,
    thread_ts: Option<String>,
    user: String,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
) {
    let target_recieved = ranking_denormalized(
        sql_tx,
        KarmaCol::Recieved,
        ktyp,
        KarmaName::new(target),
    ).await.map(|e| e.map(|c| format!("{}", c)));

    let total_recieved = count(
        sql_tx,
        KarmaCol::Recieved,
    ).await.map(|e| format!("{}", e));

    let target_given = ranking_denormalized(
        sql_tx,
        KarmaCol::Given,
        ktyp,
        KarmaName::new(target),
    ).await.map(|e| e.map(|c| format!("{}", c)));

    let total_given = count(
        sql_tx,
        KarmaCol::Given,
    ).await.map(|e| format!("{}", e));

    // Formatting the ranks
    let receiving = match (target_recieved, total_recieved) {
        (Ok(Some(r)), Ok(tr)) => Some(format!("{} rank is {} of {} in receiving", label, r, tr)),
        _ => None,
    };

    let giving = match (target_given, total_given) {
        (Ok(Some(g)), Ok(tg)) => Some(format!("{} rank is {} of {} in giving", label, g, tg)),
        _ => None,
    };

    let rank = match (receiving, giving) {
        (Some(r), Some(g)) => format!("{} and {}.", r, g),
        (Some(r), None)    => format!("{}.", r),
        (None, Some(g))    => format!("{}.", g),
        (None, None)       => format!("No ranking available"),
    };

    let _ = send_simple_message(
        tx,
        channel,
        thread_ts,
        format!("<@{}>: {}", user, rank),
    ).await;
}

async fn ranking_denormalized(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    user: KarmaName
) -> DbResult<Option<u32>> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            // Default won't work here, override
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
                table=karma_col, t_col1=karma_typ, t_col2=t_col2
            ))?;
            let user = user.to_string();
            let mut rows = stmt.query(rs::params![user, user])?;

            if let Ok(Some(row)) = rows.next() {
                let count: Option<u32> = row.get(0).ok();

                Ok(count)
            } else {
                Err(format!(
                    "ERROR [Sql Worker]: RankingDenormalized - karma_col: {:?}, karma_typ: {:?}, user: {:?}",
                    karma_col, karma_typ, user
                ).into())
            }
        })
    ).await
}

async fn count(
    sql_tx: &mut mpsc::Sender<Query>,
    karma_col: KarmaCol,
) -> DbResult<u32> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let mut stmt = conn.prepare(&format!(
                "SELECT COUNT(name) FROM {table}",
                table=karma_col
            ))?;
            let mut rows = stmt.query([])?;

            if let Ok(Some(row)) = rows.next() {
                let count: u32 = row.get(0)?;

                Ok(count)
            } else {
                Err(format!("ERROR [Sql Worker]: Count - ERROR - karma_col: {:?}", karma_col).into())
            }
        })
    ).await
}
