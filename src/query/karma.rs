use chrono::prelude::{DateTime, Utc};

use deadpool_postgres::Pool;
use log::{error, info};
use tokio_postgres::IsolationLevel;

use futures_util::future;

use anyhow::Result as AResult;
use anyhow::anyhow;

use crate::parser::karma;
use crate::parser::karma::KST;

use crate::query::Karma;
use crate::query::KarmaName;
use crate::query::add_channel_opt;
use crate::query::add_nick;
use crate::query::normalize;

use crate::bot::user_event::Event;

use kcore::SlackSender;

pub async fn add_karma<S: SlackSender>(event: &Event<S>, pool: &Pool) {
    match karma::parse(&event.sanitize().await) {
        Ok(mut karma) if !karma.is_empty() => {
            info!("Parsed Karma: {karma:?}");
            let username = event.get_username();
            let user_real_name = event.get_user_real_name();

            // Filter karma of any entity that is same as
            // username, and check if any got filtered, if
            // so, log.
            let before = karma.len();
            match username {
                None => (),
                Some(ref ud) => {
                    karma.retain(|KST(t, _)| KarmaName::new(t) != KarmaName::new(ud));
                }
            }
            let after = karma.len();

            if before != after {
                info!("User self-voted: {username:?}");
            }

            if !karma.is_empty() {
                match (username, user_real_name) {
                    (Some(ud), Some(rn)) => {
                        let res = add_karma_query(
                            pool,
                            Utc::now(),
                            event.user_id.clone(),
                            KarmaName::new(&ud),
                            KarmaName::new(&rn),
                            Some(event.channel_id.clone()),
                            karma,
                        )
                        .await;

                        match res {
                            Ok(()) => (),
                            Err(e) => error!("Database Error: {e:?}"),
                        }
                    }
                    _ => error!("Wasn't able to get a username/real_name from slack"),
                }
            }
        }

        Ok(_) => (),

        Err(e) => {
            // The parse should return empty if its valid, something
            // broke, should log it here
            error!("Failed to parse karma: {e:?}");
        }
    }
}

async fn add_karma_query(
    pool: &Pool,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
) -> AResult<()> {
    if karma.is_empty() {
        return Err(anyhow!("The karma vector is empty"));
    }
    let mut client = pool.get().await?;
    let txn = client
        .build_transaction()
        .isolation_level(IsolationLevel::ReadCommitted)
        .start()
        .await?;

    let (nick_id, channel_id) = future::try_join(
        add_nick(&txn, user_id, username.clone(), real_name),
        add_channel_opt(&txn, channel_id),
    )
    .await?;

    // Prepare the arrays for bulk inserts into postgres
    let (karma_texts, amounts): (Vec<String>, Vec<Karma>) = karma
        .into_iter()
        .map(|KST(text, amount)| (normalize(&text), amount))
        .unzip();

    // Shove it into the database now, its a mixture of scalar and arrays
    txn.execute(
        "INSERT INTO votes
            (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id)
        SELECT $1, $2, unnest($3::text[]), unnest($4::smallint[]), $5, $6",
        &[
            &timestamp,
            &username,
            &karma_texts,
            &amounts,
            &nick_id,
            &channel_id,
        ],
    )
    .await?;
    txn.commit().await?;
    Ok(())
}
