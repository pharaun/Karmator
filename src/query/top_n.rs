use tokio_postgres::Client;
use std::sync::Arc;
use log::error;

use anyhow::Result as AResult;

use kcore::slack;

use crate::query::{KarmaCol, KarmaTyp, OrdQuery};
use crate::bot::user_event::Event;


pub async fn top_n<S>(
    event: &mut Event<S>,
    client: Arc<Client>,
    kcol1: KarmaCol,
    kord1: OrdQuery,
    kcol2: KarmaCol,
    kord2: OrdQuery,
    ktyp: KarmaTyp,
    label: (&str, &str),
    limit: u32,
)
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    let high = top_n_denormalized(
        client.clone(),
        kcol1,
        ktyp,
        limit,
        kord1,
    ).await.map(|e| {
        e.iter().map(
            |(e, c)| format!("{}, ({})", e, c)
        ).collect::<Vec<String>>().join("; ")
    });

    let low = top_n_denormalized(
        client.clone(),
        kcol2,
        ktyp,
        limit,
        kord2,
    ).await.map(|e| {
        e.iter().map(
            |(e, c)| format!("{}, ({})", e, c)
        ).collect::<Vec<String>>().join("; ")
    });

    // TODO: do something about this
    let _ = match (high, low) {
        (Ok(h), Ok(l)) => event.send_reply(&format!("{}: {}. {}: {}.", label.0, h, label.1, l)).await,
        e => {
            error!("Top-n something went wrong - {:?}", e);
            event.send_reply("Something went wrong").await
        },
    };
}


async fn top_n_denormalized(
    client: Arc<Client>,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    limit: u32,
    ord: OrdQuery
) -> AResult<Vec<(String, i64)>> {
    let rows = client.query(&format!(
        "SELECT name, {t_col} as total FROM {table} ORDER BY total {q_ord} LIMIT {limit}",
        t_col=karma_typ,
        table=karma_col,
        q_ord=ord,
        limit=limit
    ), &[]).await?;

    let mut ret: Vec<(String, i64)> = vec![];
    for row in rows {
        let name: String = row.try_get(0)?;
        let count: i64 = row.try_get(1)?;

        ret.push((name.clone(), count));
    }
    Ok(ret)
}
