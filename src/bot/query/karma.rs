use chrono::prelude::{Utc, DateTime};

use tokio_postgres::Client;
use std::sync::Arc;
use std::error::Error;
use log::{info, error};

use crate::bot::parser::karma::KST;
use crate::bot::parser::karma;

use crate::bot::query::KarmaName;
use crate::bot::query::normalize;
use crate::bot::user_event::Event;

use crate::bot::query::add_nick;
use crate::bot::query::add_channel;


pub async fn add_karma(
    client: Arc<Client>,
    event: &Event,
) {
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
                None     => (),
                Some(ref ud) => karma.retain(|&karma::KST(ref t, _)| {
                    KarmaName::new(t) != KarmaName::new(ud)
                }),
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
                            karma
                        ).await;

                        match res {
                            Ok(_) => (),
                            Err(x) => error!("Database Error: {:?}", x),
                        }
                    },
                    _ => error!("Wasn't able to get a username/real_name from slack"),
                }
            }
        },

        Ok(_) => (),

        Err(e) => {
            // The parse should return empty if its valid, something
            // broke, should log it here
            error!("Failed to parse karma: {:?}", e);
        },
    }
}


async fn add_karma_query(
    client: Arc<Client>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: Option<String>,
    karma: Vec<KST>,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    let nick_id: i64 = add_nick(client.clone(), user_id, username.clone(), real_name).await?;
    let channel_id: Option<i64> = match channel_id {
        Some(cid) => Some(add_channel(client.clone(), cid).await?),
        None => None,
    };

    // Shove the karma into the db now
    for KST(karma_text, amount) in karma.iter() {
        let karma_text = normalize(karma_text);

        // TODO: better handle failure of username, maybe we should make username mandatory before inserting?
        client.execute(
            "INSERT INTO votes
                (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id)
            VALUES
                ($1, $2, $3, $4, $5, $6)"
            , &[&timestamp, &username, &karma_text, &amount, &nick_id, &channel_id]).await?;
    }
    Ok(())
}
