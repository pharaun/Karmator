use deadpool_postgres::GenericClient;
use futures_util::future;
use log::error;

use anyhow::Result as AResult;

use crate::bot::user_event::Event;
use crate::query::{KarmaCol, KarmaTyp, OrdQuery};

use kcore::SlackSender;

pub struct HiLo<'a> {
    label: &'a str,
    col: KarmaCol,
    ord: OrdQuery,
}

impl<'a> HiLo<'a> {
    #[expect(clippy::use_self)]
    pub fn new(label: &'a str, col: KarmaCol, ord: OrdQuery) -> HiLo<'a> {
        Self { label, col, ord }
    }
}

pub async fn top_n<S: SlackSender, C: GenericClient>(
    event: &Event<S>,
    client: &C,
    high: HiLo<'_>,
    low: HiLo<'_>,
    ktyp: KarmaTyp,
    limit: u32,
) {
    let query = future::try_join(
        top_n_denormalized(client, high.col, ktyp, limit, high.ord),
        top_n_denormalized(client, low.col, ktyp, limit, low.ord),
    );

    match query.await {
        Ok((high_rows, low_rows)) => {
            event
                .send_reply(&format!(
                    "{}: {}. {}: {}.",
                    high.label,
                    high_rows
                        .iter()
                        .map(|(e, c)| format!("{e}, ({c})"))
                        .collect::<Vec<String>>()
                        .join("; "),
                    low.label,
                    low_rows
                        .iter()
                        .map(|(e, c)| format!("{e}, ({c})"))
                        .collect::<Vec<String>>()
                        .join("; "),
                ))
                .await;
        }
        Err(e) => {
            error!("Top-n something went wrong - {e:?}");
            event.send_reply("Something went wrong").await;
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
                "SELECT name, {karma_typ} as total FROM {karma_col} ORDER BY total {ord} LIMIT $1"
            ),
            &[&i64::from(limit)],
        )
        .await?;

    rows.into_iter()
        .map(|row| Ok((row.try_get(0)?, row.try_get(1)?)))
        .collect()
}
