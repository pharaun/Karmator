use chrono::prelude::{Utc, DateTime};

use tokio_postgres::Client;
use std::sync::Arc;
use std::error::Error;

use crate::bot::parser::karma::KST;
use crate::bot::parser::karma;

use crate::bot::query::KarmaName;
use crate::bot::query::normalize;
use crate::bot::user_event::Event;


pub async fn add_karma(
    client: Arc<Client>,
    event: &Event,
) {
    match karma::parse(&event.santize().await) {
        Ok(mut karma) if !karma.is_empty() => {
            println!("INFO [User Event]: Parsed Karma: {:?}", karma);
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
                println!("INFO [User Event]: User self-voted: {:?}", username);
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
                            Err(x) => eprintln!("ERROR: [User Event] database error - {:?}", x),
                        }
                    },
                    _ => eprintln!("ERROR: [User Event] Wasn't able to get a username/real_name from slack"),
                }
            }
        },

        Ok(_) => (),

        Err(e) => {
            // The parse should return empty if its valid, something
            // broke, should log it here
            eprintln!("ERROR [User Event]: Failed to parse karma: {:?}", e);
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
) -> Result<(), Box<dyn Error>> {
    let nick_id: i64 = {
        let row = client.query_opt("SELECT id FROM nick_metadata WHERE username = $1", &[&user_id]).await?;

        match row {
            Some(r) => r.try_get(0),
            None => {
                let row = client.query_one(
                    "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES ($1, $2, $3, $4) RETURNING id",
                    &[&username, &real_name, &user_id, &"SlackServer"]
                ).await?;

                row.try_get(0)
            },
        }
    }?;

    let channel_id: Option<i64> = if let Some(cid) = channel_id {
        let rows = client.query_one("SELECT id FROM chan_metadata WHERE channel = $1", &[&cid]).await;

        if let Ok(row) = rows {
            row.try_get(0)
        } else {
            let row = client.query_one("INSERT INTO chan_metadata (channel) VALUES ($1) RETURNING id", &[&cid]).await?;
            row.try_get(0)
        }
    } else {
        Ok(None)
    }?;

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
