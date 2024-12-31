use tokio_postgres::GenericClient;
use log::error;

use futures_util::future;

use anyhow::Result as AResult;

use kcore::slack;

use crate::query::{KarmaCol, KarmaTyp, KarmaName};
use crate::bot::user_event::Event;


pub async fn ranking<S, C: GenericClient>(
    event: &mut Event<S>,
    client: &C,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
)
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    let query = future::try_join4(
        ranking_denormalized(client, KarmaCol::Received, ktyp, KarmaName::new(target)),
        count(client, KarmaCol::Received),
        ranking_denormalized(client, KarmaCol::Given, ktyp, KarmaName::new(target)),
        count(client, KarmaCol::Given),
    );

    match query.await {
        Ok((target_received, total_received, target_given, total_given)) => {
            // Formatting the ranks
            let receiving = match (target_received, total_received) {
                (Some(r), tr) => Some(format!("{} rank is {} of {} in receiving", label, r, tr)),
                (None, _) => None,
            };

            let giving = match (target_given, total_given) {
                (Some(g), tg) => Some(format!("{} rank is {} of {} in giving", label, g, tg)),
                (None, _) => None,
            };

            let rank = match (receiving, giving) {
                (Some(r), Some(g)) => format!("{} and {}.", r, g),
                (Some(r), None)    => format!("{}.", r),
                (None, Some(g))    => format!("{}.", g),
                (None, None)       => format!("No ranking available"),
            };

            event.send_reply(&rank).await;
        },
        Err(e) => {
            error!("Ranking something went wrong - {:?}", e);
            event.send_reply("Something went wrong").await
        },
    }
}

async fn ranking_denormalized<C: GenericClient>(
    client: &C,
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

async fn count<C: GenericClient>(
    client: &C,
    karma_col: KarmaCol,
) -> AResult<i64> {
    Ok(client.query_one(&format!(
        "SELECT COUNT(name) FROM {table}",
        table=karma_col), &[]).await?.try_get(0)?
    )
}
