use deadpool_postgres::GenericClient;

use log::error;
use std::collections::HashSet;

use anyhow::Result as AResult;
use anyhow::anyhow;

use crate::bot::user_event::Event;
use crate::query::{KarmaCol, KarmaName};

use kcore::SlackSender;

pub async fn partial<S: SlackSender, C: GenericClient>(
    event: &Event<S>,
    client: &C,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    let res = partial_query(client, kcol, arg.into_iter().map(KarmaName::new).collect())
        .await
        .map(|e| {
            e.iter()
                .map(|(entity, up, down, side)| {
                    format!(
                        "{}, {} ({}++/{}--/{}+-)",
                        entity,
                        (up - down),
                        up,
                        down,
                        side
                    )
                })
                .collect::<Vec<String>>()
                .join("; ")
        });

    match res {
        Ok(x) => event.send_reply(&x).await,
        e => {
            error!("partial - Error: {e:?}");
            event.send_reply("Something went wrong").await;
        }
    }
}

async fn partial_query<C: GenericClient>(
    client: &C,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> AResult<Vec<(String, i64, i64, i64)>> {
    // Will cause postgres error due to zero value passed despite a parameterized query,
    // only use this function if there is at least 1 user.
    if users.is_empty() {
        return Err(anyhow!("Users hash is 0 (no users)"));
    }

    // We can pass in an array to postgres and unnest it to have a variable number of entity
    let params: Vec<String> = users.iter().map(ToString::to_string).collect();
    let rows = client
        .query(
            &format!(
                "SELECT name, up, down, side FROM {karma_col}
                WHERE md5(lower(name)) = ANY(
                    SELECT md5(lower(unnest)) FROM unnest($1::text[])
                ) ORDER BY name DESC"
            ),
            &[&params],
        )
        .await?;

    // A bit more additional work than usual
    let ret: AResult<Vec<(String, i64, i64, i64)>> = rows
        .into_iter()
        .map(|row| {
            Ok((
                row.try_get(0)?,
                row.try_get(1)?,
                row.try_get(2)?,
                row.try_get(3)?,
            ))
        })
        .collect();

    match ret {
        Ok(mut val) => {
            let has: HashSet<KarmaName> = val.iter().map(|row| KarmaName::new(&row.0)).collect();

            // Evaulate if there's missing ones and add if so
            // TODO: this should be case insensitive (Ie database can return B and we have b)
            let mut diff = users
                .difference(&has)
                .map(|un| (un.to_string(), 0, 0, 0))
                .collect();
            val.append(&mut diff);
            Ok(val)
        }
        Err(e) => Err(e),
    }
}
