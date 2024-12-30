use tokio_postgres::Client;
use std::sync::Arc;
use log::error;

use anyhow::Result as AResult;

use kcore::slack;

use crate::query::{KarmaCol, KarmaTyp, KarmaName};
use crate::bot::user_event::Event;


pub async fn ranking<S>(
    event: &mut Event<S>,
    client: Arc<Client>,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
)
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    let target_received = ranking_denormalized(
        client.clone(),
        KarmaCol::Received,
        ktyp,
        KarmaName::new(target),
    ).await.map(|e| e.map(|c| format!("{}", c)));


    let total_received = count(
        client.clone(),
        KarmaCol::Received,
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
    let receiving = match (target_received, total_received) {
        (Ok(Some(r)), Ok(tr)) => Some(format!("{} rank is {} of {} in receiving", label, r, tr)),
        (Err(a), Err(b)) => {
            error!("Database Error - target: {:?} total: {:?}", a, b);
            None
        },
        (Err(a), _) => {
            error!("Database Error - target: {:?}", a);
            None
        },
        (_, Err(b)) => {
            error!("Database Error - total: {:?}", b);
            None
        },
        _ => None,
    };

    let giving = match (target_given, total_given) {
        (Ok(Some(g)), Ok(tg)) => Some(format!("{} rank is {} of {} in giving", label, g, tg)),
        (Err(a), Err(b)) => {
            error!("Database Error - target: {:?} total: {:?}", a, b);
            None
        },
        (Err(a), _) => {
            error!("Database Error - target: {:?}", a);
            None
        },
        (_, Err(b)) => {
            error!("Database Error - total: {:?}", b);
            None
        },
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
) -> AResult<Option<i64>> {
    // Default won't work here, override
    let t_col2 = match karma_typ {
        KarmaTyp::Total => "kcol2.up - kcol2.down",
        KarmaTyp::Side  => "kcol2.side",
    };

    Ok(client.query_one(&format!(
        "SELECT CASE WHEN (
            EXISTS (SELECT TRUE FROM {table} WHERE md5(lower(name)) = md5(lower($1)))
        ) THEN (
            SELECT (COUNT(name) + 1) FROM {table} WHERE (
                {t_col1}
            ) > (
                SELECT ({t_col2}) FROM {table} AS kcol2 WHERE md5(lower(kcol2.name)) = md5(lower($1))
            )
        ) ELSE NULL END",
        table=karma_col, t_col1=karma_typ, t_col2=t_col2), &[&user]).await?.try_get(0)?
    )
}

async fn count(
    client: Arc<Client>,
    karma_col: KarmaCol,
) -> AResult<i64> {
    Ok(client.query_one(&format!(
        "SELECT COUNT(name) FROM {table}",
        table=karma_col), &[]).await?.try_get(0)?
    )
}
