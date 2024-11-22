use tokio::sync::mpsc;
use tokio_postgres::Client;
use std::sync::Arc;

use crate::bot::query::{KarmaCol, KarmaTyp, KarmaName};
use crate::bot::user_event::Event;


pub async fn ranking(
    event: &mut Event,
    client: Arc<Client>,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
) {
    let target_recieved = ranking_denormalized(
        client.clone(),
        KarmaCol::Recieved,
        ktyp,
        KarmaName::new(target),
    ).await.map(|e| e.map(|c| format!("{}", c)));

    let total_recieved = count(
        client.clone(),
        KarmaCol::Recieved,
    ).await.map(|e| format!("{}", e));

    let target_given = ranking_denormalized(
        client.clone(),
        KarmaCol::Given,
        ktyp,
        KarmaName::new(target),
    ).await.map(|e| e.map(|c| format!("{}", c)));

    let total_given = count(
        client.clone(),
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

    event.send_reply(&rank).await;
}

async fn ranking_denormalized(
    client: Arc<Client>,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    user: KarmaName
) -> Result<Option<u32>, String> {
    // Default won't work here, override
    let t_col2 = match karma_typ {
        KarmaTyp::Total => "kcol2.up - kcol2.down",
        KarmaTyp::Side  => "kcol2.side",
    };

    let user = user.to_string();
    let rows = client.query_one(&format!(
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
    ), &[user, user]).await.map_err(|x| x.to_string())?;

    let count: Option<u32> = row.get(0).ok();
    Ok(count)
}

async fn count(
    client: Arc<Client>,
    karma_col: KarmaCol,
) -> Result<u32, String> {
    let rows = client.query_one(&format!(
        "SELECT COUNT(name) FROM {table}",
        table=karma_col
    ), &[]).await.map_err(|x| x.to_string())?;

    let count: Option<u32> = row.get(0).ok();
    Ok(count)
}
