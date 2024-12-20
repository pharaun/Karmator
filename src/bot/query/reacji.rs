use chrono::prelude::{Utc, DateTime};

use tokio_postgres::Client;
use std::sync::Arc;
use std::error::Error;

use crate::bot::parser::karma::Karma;
use crate::bot::parser::reacji_to_karma;

use crate::bot::query::{KarmaName, ReacjiAction};
use crate::bot::user_event::Event;

use crate::bot::query::add_nick;
use crate::bot::query::add_channel;


pub async fn add_reacji(
    client: Arc<Client>,
    event: &mut Event,
    input: &str,
    action: ReacjiAction,
) {
    match reacji_to_karma(input) {
        Some(karma) => {
            let message_id = query_reacji_message(
                client.clone(),
                event.channel_id.clone(),
                event.thread_ts.clone().unwrap()
            ).await.map_or_else(
                |e| Err(e.to_string()),
                |v| v.ok_or("NONE".to_string())
            );

            let message_id = match message_id {
                Ok(message_id) => Ok(Some(message_id)),
                Err(_) => match event.get_message().await {
                    Ok(message_user_id) => {
                        // We have the message content, insert it into the table and
                        // get its row id
                        let santized_text = event.santize().await;

                        match (
                            event.get_other_username(&message_user_id).await,
                            event.get_other_user_real_name(&message_user_id).await,
                        ) {
                            (Some(ud), Some(rn)) => {
                                add_reacji_message(
                                    client.clone(),
                                    message_user_id,
                                    KarmaName::new(&ud),
                                    KarmaName::new(&rn),
                                    event.channel_id.clone(),
                                    // TODO: should add error check here
                                    event.thread_ts.clone().unwrap(),
                                    santized_text
                                ).await.map_err(|x| x.to_string())
                            },
                            e => Err(format!("ERROR: [Reacji] Querying for user/name failed: {:?}", e)),
                        }
                    },
                    e => Err(format!("ERROR: [Reacji] Querying for message failed: {:?}", e)),
                },
            };

            match message_id {
                Err(e) => {
                    eprintln!("ERROR: [Reacji] Wasn't able to get/store an reactji message, vote isn't recorded");
                    eprintln!("ERROR: [Reacji] Returned error: {:?}", e);
                },
                Ok(None) => (), // These are expected error, drop
                Ok(Some(mid)) => {
                    match (
                        event.get_username().await,
                        event.get_user_real_name().await,
                    ) {
                        (Some(ud), Some(rn)) => {
                            let e = add_reacji_query(
                                client.clone(),
                                Utc::now(),
                                event.user_id.clone(),
                                KarmaName::new(&ud),
                                KarmaName::new(&rn),
                                action,
                                mid,
                                karma,
                            ).await;

                            if e.is_err() {
                                eprintln!("ERROR: [Reacji] Query failed: {:?}", e);
                            }
                        },
                        e => eprintln!("ERROR: [Reacji] Querying for user/name failed: {:?}", e),
                    }
                },
            }
        },
        None => (),
    }
}


async fn query_reacji_message(
    client: Arc<Client>,
    channel_id: String,
    message_ts: String,
) -> Result<Option<i64>, Box<dyn Error + Send + Sync>> {
    let row = client.query_opt(
        "SELECT id FROM chan_metadata WHERE channel = $1",
        &[&channel_id]
    ).await?;

    if let Some(r) = row {
        let cid: i64 = r.try_get(0)?;

        let row = client.query_opt(
            "SELECT id FROM reacji_message WHERE ts = ? AND chan_id = ?",
            &[&message_ts, &cid]
        ).await?;

        if let Some(r) = row {
            return Ok(r.try_get(0)?);
        }
    }

    Ok(None)
}


async fn add_reacji_message(
    client: Arc<Client>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: String,
    message_ts: String,
    message: String,
) -> Result<Option<i64>, Box<dyn Error + Send + Sync>> {
    let nick_id: i64 = add_nick(client.clone(), user_id, username.clone(), real_name).await?;
    let channel_id: i64 = add_channel(client.clone(), channel_id).await?;

    // Insert the reacji_message content now
    let rows = client.query_opt(
        "SELECT id, nick_id, message FROM reacji_message WHERE ts = $1 AND chan_id = $2",
        &[&message_ts, &channel_id]).await;

    if let Ok(Some(row)) = rows {
        // Compare the 2 and if its not the same, warn in log, otherwise return
        let id = row.get(0);
        let sql_nick: i64 = row.get(1);
        let sql_message: String = row.get(2);

        // Compare
        if sql_nick != nick_id || sql_message != message {
            eprintln!("ERROR: [Reacji Message] duplicate channel + ts");
            eprintln!("ERROR: [Reacji Message] \tSlack Nick: {}", nick_id);
            eprintln!("ERROR: [Reacji Message] \tSql Nick:   {}", sql_nick);
            eprintln!("ERROR: [Reacji Message] \tSlack Msg:  {}", message);
            eprintln!("ERROR: [Reacji Message] \tSql Msg:    {}", sql_message);
        }

        // Return one anyway for now
        Ok(Some(id))
    } else {
        Ok(Some(client.query_one(
            "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES ($1, $2, $3, $4) RETURNING id",
            &[&message_ts, &channel_id, &nick_id, &message]
        ).await?.try_get(0)?))
    }
}


async fn add_reacji_query(
    client: Arc<Client>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    action: ReacjiAction,
    message_id: i64,
    amount: Karma,
) -> Result<Option<i64>, Box<dyn Error + Send + Sync>> {
    let nick_id: i64 = add_nick(client.clone(), user_id, username.clone(), real_name).await?;

    // Insert the reacji into the database
    Ok(Some(client.query_one(
        "INSERT INTO reacji_votes
            (voted_at, by_whom_name, action, reacji_message_id, amount, nick_id)
        VALUES
            ($1, $2, $3, $4, $5, $6)
        RETURNING id",
        &[&timestamp, &username, &action, &message_id, &amount, &nick_id]
    ).await?.try_get(0)?))
}
