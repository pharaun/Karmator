use tokio_postgres::GenericClient;

use log::error;
use std::collections::HashSet;

use anyhow::Result as AResult;

use futures_util::{pin_mut, TryStreamExt};

use kcore::slack;

use crate::bot::user_event::Event;
use crate::query::{KarmaCol, KarmaName};

pub async fn partial<S, C: GenericClient>(
    event: &mut Event<S>,
    client: &C,
    kcol: KarmaCol,
    arg: Vec<&str>,
) where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
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
            error!("partial - Error: {:?}", e);
            event.send_reply("Something went wrong").await
        }
    };
}

async fn partial_query<C: GenericClient>(
    client: &C,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> AResult<Vec<(String, i64, i64, i64)>> {
    // Hack to insert enough parameterizers into the query
    let p_user = {
        let mut p_user: String = "md5(lower($1))".to_string();
        for i in 2..(users.len() + 1) {
            p_user.push_str(&format!(", md5(lower(${}))", i));
        }
        p_user
    };
    let query: String = format!(
        "SELECT name, up, down, side FROM {table} WHERE md5(lower(name)) in ({p_user}) ORDER BY name DESC",
        table=karma_col, p_user=p_user
    );
    let params: Vec<String> = users.iter().map(|x| x.to_string()).collect();

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
