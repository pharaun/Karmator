use deadpool_postgres::GenericClient;
use log::error;

use futures_util::future;

use anyhow::Result as AResult;

use crate::bot::user_event::Event;
use crate::query::{KarmaCol, KarmaName, KarmaTyp};

use kcore::SlackSender;

pub async fn ranking<S: SlackSender, C: GenericClient>(
    event: &Event<S>,
    client: &C,
    ktyp: KarmaTyp,
    target: &str,
    label: &str,
) {
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
                (Some(r), tr) => Some(format!("{label} rank is {r} of {tr} in receiving")),
                (None, _) => None,
            };

            let giving = match (target_given, total_given) {
                (Some(g), tg) => Some(format!("{label} rank is {g} of {tg} in giving")),
                (None, _) => None,
            };

            let rank = match (receiving, giving) {
                (Some(r), Some(g)) => format!("{r} and {g}."),
                (Some(r), None) => format!("{r}."),
                (None, Some(g)) => format!("{g}."),
                (None, None) => "No ranking available".to_owned(),
            };

            event.send_reply(&rank).await;
        }
        Err(e) => {
            error!("Ranking something went wrong - {e:?}");
            event.send_reply("Something went wrong").await;
        }
    }
}

async fn ranking_denormalized<C: GenericClient>(
    client: &C,
    karma_col: KarmaCol,
    karma_typ: KarmaTyp,
    user: KarmaName,
) -> AResult<Option<i64>> {
    Ok(client
        .query_one(
            &format!(
                "SELECT rank FROM (
                SELECT name, RANK() OVER (ORDER BY ({karma_typ}) DESC) as rank FROM {karma_col}
            ) WHERE md5(lower(name)) = md5(lower($1))"
            ),
            &[&user],
        )
        .await?
        .try_get(0)?)
}

async fn count<C: GenericClient>(client: &C, karma_col: KarmaCol) -> AResult<i64> {
    Ok(client
        .query_one(&format!("SELECT COUNT(*) FROM {karma_col}"), &[])
        .await?
        .try_get(0)?)
}
