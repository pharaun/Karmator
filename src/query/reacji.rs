use chrono::prelude::{DateTime, Utc};

use futures_util::future;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio_postgres::Client;

use anyhow::Result as AResult;

use log::error;

use kcore::slack;

use crate::parser::karma::Karma;
use crate::parser::reacji_to_karma;

use crate::query::add_channel;
use crate::query::add_nick;
use crate::query::{KarmaName, ReacjiAction};

use crate::bot::user_event::Event;

pub async fn add_reacji<S>(
    event: &mut Event<S>,
    client: Arc<RwLock<Client>>,
    input: &str,
    action: ReacjiAction,
) where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    if let Some(karma) = reacji_to_karma(input) {
        let message_id = match query_reacji_message(
            client.clone(),
            event.channel_id.clone(),
            event.thread_ts.clone().unwrap(),
        )
        .await
        {
            Ok(Some(message_id)) => Ok(Some(message_id)),
            Ok(None) => match event.get_message().await {
                // We have the message content, insert it into the table and get its row id
                Ok(Some(message_user_id)) => {
                    match future::join3(
                        event.santize(),
                        event.get_other_username(&message_user_id),
                        event.get_other_user_real_name(&message_user_id),
                    )
                    .await
                    {
                        (santized_text, Some(ud), Some(rn)) => {
                            add_reacji_message(
                                client.clone(),
                                message_user_id,
                                KarmaName::new(&ud),
                                KarmaName::new(&rn),
                                event.channel_id.clone(),
                                // TODO: should add error check here
                                event.thread_ts.clone().unwrap(),
                                santized_text,
                            )
                            .await
                            .map_err(|x| x.to_string())
                        }
                        (_, ud, rn) => {
                            Err(format!("Querying for user/name failed: {:?}", (ud, rn)))
                        }
                    }
                }
                // This is a bot-owned message so abort out
                Ok(None) => return,
                e => Err(format!(
                    "ERROR: [Reacji] Querying for message failed: {:?}",
                    e
                )),
            },
            Err(e) => Err(format!("Database error: {:?}", e)),
        };

        match message_id {
            Err(e) => {
                error!("Failed to get reacji message - Error: {:?}", e);
            }
            Ok(None) => (), // These are expected error, drop
            Ok(Some(mid)) => {
                match future::join(event.get_username(), event.get_user_real_name()).await {
                    (Some(ud), Some(rn)) => {
                        if let Err(e) = add_reacji_query(
                            client.clone(),
                            Utc::now(),
                            event.user_id.clone(),
                            KarmaName::new(&ud),
                            KarmaName::new(&rn),
                            action,
                            mid,
                            karma,
                        )
                        .await
                        {
                            error!("Query failed: {:?}", e);
                        }
                    }
                    e => error!("Querying for user/name failed: {:?}", e),
                }
            }
        }
    }
}

async fn query_reacji_message(
    client: Arc<RwLock<Client>>,
    channel_id: String,
    message_ts: String,
) -> AResult<Option<i64>> {
    if let Some(row) = (*client.read().await)
        .query_opt(
            "SELECT rm.id FROM reacji_message AS rm
            JOIN chan_metadata AS cm ON rm.chan_id = cm.id
            WHERE rm.ts = $1 AND cm.channel = $2",
            &[&message_ts, &channel_id],
        )
        .await?
    {
        Ok(row.try_get(0)?)
    } else {
        Ok(None)
    }
}

async fn add_reacji_message(
    client: Arc<RwLock<Client>>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: String,
    message_ts: String,
    message: String,
) -> AResult<Option<i64>> {
    let mut client = client.write().await;
    let txn = client.transaction().await?;

    let (nick_id, channel_id) = future::try_join(
        add_nick(&txn, user_id, username.clone(), real_name),
        add_channel(&txn, channel_id),
    )
    .await?;

    // Insert the reacji_message content now
    let rows = txn
        .query_opt(
            "SELECT id, nick_id, message FROM reacji_message WHERE ts = $1 AND chan_id = $2",
            &[&message_ts, &channel_id],
        )
        .await;

    let ret = if let Ok(Some(row)) = rows {
        // Compare the 2 and if its not the same, warn in log, otherwise return
        let id = row.get(0);
        let sql_nick: i64 = row.get(1);
        let sql_message: String = row.get(2);

        // Compare
        if sql_nick != nick_id || sql_message != message {
            error!(
                "Duplicate Channel+TS - Slack/Sql - Nick {} / {} - Msg: {} / {}",
                nick_id, sql_nick, message, sql_message
            );
        }

        // Return one anyway for now
        Ok(Some(id))
    } else {
        Ok(Some(txn.query_one(
            "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES ($1, $2, $3, $4) RETURNING id",
            &[&message_ts, &channel_id, &nick_id, &message]
        ).await?.try_get(0)?))
    };
    txn.commit().await?;
    ret
}

async fn add_reacji_query(
    client: Arc<RwLock<Client>>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    action: ReacjiAction,
    message_id: i64,
    amount: Karma,
) -> AResult<Option<i64>> {
    let mut client = client.write().await;
    let txn = client.transaction().await?;

    let nick_id: i64 = add_nick(&txn, user_id, username.clone(), real_name).await?;

    // Insert the reacji into the database
    let ret = Ok(Some(
        txn.query_one(
            "INSERT INTO reacji_votes
            (voted_at, by_whom_name, action, reacji_message_id, amount, nick_id)
        VALUES
            ($1, $2, $3, $4, $5, $6)
        RETURNING id",
            &[
                &timestamp,
                &username,
                &action,
                &message_id,
                &amount,
                &nick_id,
            ],
        )
        .await?
        .try_get(0)?,
    ));
    txn.commit().await?;
    ret
}
