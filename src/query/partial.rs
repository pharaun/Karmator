use deadpool_postgres::GenericClient;

use log::error;
use std::collections::HashSet;
use std::fmt::Write as _;

use anyhow::anyhow;
use anyhow::Result as AResult;

use futures_util::pin_mut;
use futures_util::TryStreamExt as _;

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

    // Hack to insert enough parameterizers into the query
    let p_user = {
        let mut p_user: String = "md5(lower($1))".to_owned();
        for i in 2..=users.len() {
            let _ = write!(p_user, ", md5(lower(${i}))");
        }
        p_user
    };
    let query: String = format!(
        "SELECT name, up, down, side FROM {karma_col} WHERE md5(lower(name)) in ({p_user}) ORDER BY name DESC"
    );
    let params: Vec<String> = users.iter().map(ToString::to_string).collect();

    // Maxmium Pain here we go!
    let it = client.query_raw(&query, params).await?;

    // A bit more additional work than usual
    let mut ret: Vec<(String, i64, i64, i64)> = vec![];
    let mut has: HashSet<KarmaName> = HashSet::new();

    pin_mut!(it);
    while let Some(row) = it.try_next().await? {
        let name: String = row.try_get(0)?;
        let up: i64 = row.try_get(1)?;
        let down: i64 = row.try_get(2)?;
        let side: i64 = row.try_get(3)?;

        has.insert(KarmaName::new(&name));
        ret.push((name, up, down, side));
    }

    // Evaulate if there's missing ones and add if so
    // TODO: this should be case insensitive (Ie database can return B and we have b)
    for n in users.difference(&has) {
        ret.push((n.to_string(), 0, 0, 0));
    }
    Ok(ret)
}
