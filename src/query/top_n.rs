use futures_util::future;
use log::error;
use tokio_postgres::GenericClient;

use anyhow::Result as AResult;

use kcore::slack;

use crate::bot::user_event::Event;
use crate::query::{KarmaCol, KarmaTyp, OrdQuery};

pub async fn top_n<S, C: GenericClient>(
    event: &mut Event<S>,
    client: &C,
    kcol1: KarmaCol,
    kord1: OrdQuery,
    kcol2: KarmaCol,
    kord2: OrdQuery,
    ktyp: KarmaTyp,
    label: (&str, &str),
    limit: u32,
) where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    let query = future::try_join(
        top_n_denormalized(client, kcol1, ktyp, limit, kord1),
        top_n_denormalized(client, kcol2, ktyp, limit, kord2),
    );

    match query.await {
        Ok((high, low)) => {
            event
                .send_reply(&format!(
                    "{}: {}. {}: {}.",
                    label.0,
                    high.iter()
                        .map(|(e, c)| format!("{}, ({})", e, c))
                        .collect::<Vec<String>>()
                        .join("; "),
                    label.1,
                    low.iter()
                        .map(|(e, c)| format!("{}, ({})", e, c))
                        .collect::<Vec<String>>()
                        .join("; "),
                ))
                .await
        }
        Err(e) => {
            error!("Top-n something went wrong - {:?}", e);
            event.send_reply("Something went wrong").await
        }
    };
}

async fn top_n_denormalized<C: GenericClient>(
    client: &C,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    limit: u32,
    ord: OrdQuery,
) -> AResult<Vec<(String, i64)>> {
    let rows = client
        .query(
            &format!(
                "SELECT name, {t_col} as total FROM {table} ORDER BY total {q_ord} LIMIT {limit}",
                t_col = karma_typ,
                table = karma_col,
                q_ord = ord,
                limit = limit
            ),
            &[],
        )
        .await?;

    let mut ret: Vec<(String, i64)> = vec![];
    for row in rows {
        let name: String = row.try_get(0)?;
        let count: i64 = row.try_get(1)?;

        ret.push((name.clone(), count));
    }
    Ok(ret)
}
