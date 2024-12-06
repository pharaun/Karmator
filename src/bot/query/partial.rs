use tokio_postgres::Client;

use std::collections::HashSet;
use std::sync::Arc;
use std::error::Error;

use futures_util::{pin_mut, TryStreamExt};

use crate::bot::query::{KarmaCol, KarmaName};
use crate::bot::user_event::Event;


pub async fn partial(
    event: &mut Event,
    client: Arc<Client>,
    kcol: KarmaCol,
    arg: Vec<&str>,
) {
    let res = partial_query(
        client.clone(),
        kcol,
        arg.into_iter().map(|i| KarmaName::new(i)).collect(),
    ).await.map(|e| {
        e.iter().map(|(entity, up, down, side)| {
            format!(
                "{}, {} ({}++/{}--/{}+-)",
                entity, (up - down), up, down, side
            )
        }).collect::<Vec<String>>().join("; ")
    });

    // TODO: do something here
    let _ = match res {
        Ok(x) => event.send_reply(&x).await,
        e => {
            eprintln!("partial something went wrong - {:?}", e);
            event.send_reply("Something went wrong").await
        },
    };
}


async fn partial_query(
    client: Arc<Client>,
    karma_col: KarmaCol,
    users: HashSet<KarmaName>,
) -> Result<Vec<(String, i64, i64, i64)>, Box<dyn Error + Send + Sync>> {
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

    // TODO: figure out how to debug this one

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
