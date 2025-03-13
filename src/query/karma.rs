use chrono::prelude::{DateTime, Utc};

use log::{error, info};
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio_postgres::Client;

use futures_util::future;

use anyhow::Result as AResult;

use kcore::slack;

use crate::parser::karma;
use crate::parser::karma::KST;

use crate::query::add_channel_opt;
use crate::query::add_nick;
use crate::query::normalize;
use crate::query::KarmaName;

use crate::bot::user_event::Event;

pub async fn add_karma<S>(event: &Event<S>, client: Arc<RwLock<Client>>)
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    match karma::parse(&event.santize().await) {
        Ok(mut karma) if !karma.is_empty() => {
            info!("Parsed Karma: {:?}", karma);
            let username = event.get_username().await;
            let user_real_name = event.get_user_real_name().await;

            // Filter karma of any entity that is same as
            // username, and check if any got filtered, if
            // so, log.
            let before = karma.len();
            match username {
                None => (),
                Some(ref ud) => {
                    karma.retain(|karma::KST(t, _)| KarmaName::new(t) != KarmaName::new(ud))
                }
            }
            let after = karma.len();

            if before != after {
                info!("User self-voted: {:?}", username);
            }

            if !karma.is_empty() {
                match (username, user_real_name) {
                    (Some(ud), Some(rn)) => {
                        let res = add_karma_query(
                            client.clone(),
                            Utc::now(),
                            event.user_id.clone(),
                            KarmaName::new(&ud),
                            KarmaName::new(&rn),
                            Some(event.channel_id.clone()),
                            karma,
                        )
                        .await;

                        match res {
                            Ok(_) => (),
                            Err(x) => error!("Database Error: {:?}", x),
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
            error!("Failed to parse karma: {:?}", e);
        }
    }
}

async fn add_karma_query(
    client: Arc<RwLock<Client>>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
) -> AResult<()> {
    let mut client = client.write().await;
    let txn = client.transaction().await?;

    let (nick_id, channel_id) = future::try_join(
        add_nick(&txn, user_id, username.clone(), real_name),
        add_channel_opt(&txn, channel_id),
    )
    .await?;

    // Shove the karma into the db now
    for KST(karma_text, amount) in karma.iter() {
        let karma_text = normalize(karma_text);

        // TODO: better handle failure of username, maybe we should make username mandatory before inserting?
        txn.execute(
            "INSERT INTO votes
                (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id)
            VALUES
                ($1, $2, $3, $4, $5, $6)",
            &[
                &timestamp,
                &username,
                &karma_text,
                &amount,
                &nick_id,
                &channel_id,
            ],
        )
        .await?;
    }
    txn.commit().await?;
    Ok(())
}
