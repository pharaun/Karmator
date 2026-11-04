use chrono::prelude::{DateTime, Utc};

use deadpool_postgres::GenericClient;
use deadpool_postgres::Pool;
use futures_util::future;
use tokio_postgres::IsolationLevel;
use tokio_postgres::Transaction;

use anyhow::Result as AResult;
use anyhow::anyhow;

use crate::parser::karma::Karma;
use crate::parser::reacji_to_karma;

use crate::query::add_channel;
use crate::query::add_nick;
use crate::query::upsert_returning_id;
use crate::query::{KarmaName, ReacjiAction};

use crate::bot::user_event::Event;

use kcore::SlackSender;

enum ReacjiMessage {
    // Row Id
    Existing(i64),
    New {
        user_id: String,
        username: String,
        real_name: String,
        text: String,
    },
}

pub async fn add_reacji<S: SlackSender>(
    event: &mut Event<S>,
    pool: &Pool,
    input: &str,
    action: ReacjiAction,
) -> AResult<()> {
    let channel_id = event.channel_id.clone();
    let message_ts = event.thread_ts.clone().ok_or(anyhow!(
        "The manually constructed message_ts was somehow None"
    ))?;

    let Some(karma) = reacji_to_karma(input) else {
        return Ok(());
    };

    // Phase 1: Check if we've seen it before
    let existing = {
        let client = pool.get().await?;
        query_reacji_message(&client, &channel_id, &message_ts)
            .await
            .map_err(|e| anyhow!("Database error: {e:?}"))?
    };

    // Phase 2: If new, go ahead and fetch the information from slack
    let message = if let Some(mid) = existing {
        ReacjiMessage::Existing(mid)
    } else {
        match event.get_message().await {
            Ok(Some(message_user_id)) => {
                let (text, owner_username, owner_real_name) = future::join3(
                    event.sanitize(),
                    event.get_other_username(&message_user_id),
                    event.get_other_user_real_name(&message_user_id),
                )
                .await;
                match (owner_username, owner_real_name) {
                    (Some(username), Some(real_name)) => ReacjiMessage::New {
                        user_id: message_user_id,
                        username,
                        real_name,
                        text,
                    },
                    e => {
                        return Err(anyhow!(
                            "Failed to get reacji message - Querying for user/name failed: {e:?}"
                        ));
                    }
                }
            }
            // This is a bot-owned message so abort out
            Ok(None) => return Ok(()),
            Err(e) => {
                return Err(anyhow!(
                    "Failed to get reacji message - Querying for message failed: {e:?}"
                ));
            }
        }
    };

    // This is already in the cache due to the fetch earlier
    let (Some(ud), Some(rn)) = (event.get_username(), event.get_user_real_name()) else {
        return Err(anyhow!("Querying for reacting user/name failed"));
    };

    // Phase 3: insert message (if new) and the vote
    let mut client = pool.get().await?;
    let txn = client
        .build_transaction()
        .isolation_level(IsolationLevel::ReadCommitted)
        .start()
        .await?;

    let message_id = match message {
        ReacjiMessage::Existing(mid) => mid,
        // Upsert, if another reacji raced us on this message we get its existing row id
        ReacjiMessage::New {
            user_id,
            username,
            real_name,
            text,
        } => {
            add_reacji_message(
                &txn,
                user_id,
                KarmaName::new(&username),
                KarmaName::new(&real_name),
                &channel_id,
                &message_ts,
                &text,
            )
            .await?
        }
    };

    add_reacji_vote(
        &txn,
        Utc::now(),
        event.user_id.clone(),
        KarmaName::new(&ud),
        KarmaName::new(&rn),
        action,
        message_id,
        karma,
    )
    .await
    .map_err(|x| anyhow!("Failed to insert reacji vote: {x:?}"))?;

    // Dropping txn without commit on any of the `?` above rolls back
    txn.commit().await?;
    Ok(())
}

async fn query_reacji_message<C: GenericClient>(
    client: &C,
    channel_id: &str,
    message_ts: &str,
) -> AResult<Option<i64>> {
    if let Some(row) = client
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
    txn: &Transaction<'_>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: &str,
    message_ts: &str,
    message: &str,
) -> AResult<i64> {
    let (nick_id, channel_id) = future::try_join(
        add_nick(txn, user_id, username.clone(), real_name),
        add_channel(txn, channel_id.to_owned()),
    )
    .await?;

    upsert_returning_id(
        txn,
        "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES ($1, $2, $3, $4)
        ON CONFLICT (ts, chan_id) DO NOTHING
        RETURNING id",
        &[&message_ts, &channel_id, &nick_id, &message],
        "SELECT id FROM reacji_message WHERE ts = $1 AND chan_id = $2",
        &[&message_ts, &channel_id],
    )
    .await
}

#[expect(clippy::too_many_arguments)]
async fn add_reacji_vote(
    txn: &Transaction<'_>,
    timestamp: DateTime<Utc>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    action: ReacjiAction,
    message_id: i64,
    amount: Karma,
) -> AResult<Option<i64>> {
    let nick_id: i64 = add_nick(txn, user_id, username.clone(), real_name).await?;

    // Insert the reacji into the database
    Ok(Some(
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
    ))
}
