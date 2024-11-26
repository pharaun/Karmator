use chrono::prelude::{Utc, DateTime};

use tokio_postgres::Client;
use std::sync::Arc;

use crate::bot::parser::karma::Karma;
use crate::bot::parser::reacji_to_karma;

use crate::bot::query::{KarmaName, ReacjiAction};
use crate::bot::user_event::Event;


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
                |e| Err(e),
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
                                ).await
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
) -> Result<Option<i64>, String> {
    let channel_id: Result<i64, String> = {
        let row = client.query_one(
            "SELECT id FROM chan_metadata WHERE channel = $1",
            &[&channel_id]).await.map_err(|x| x.to_string())?;
        Ok(row.get(0))
    };

    match channel_id {
        Ok(channel_id) => {
            // We good let's proceed, otherwise abort since if channel id isn't here its
            // not going to be in reacji_message either
            let row = client.query_one(
                "SELECT id FROM reacji_message WHERE ts = ? AND chan_id = ?"
                , &[&message_ts, &channel_id]).await.map_err(|x| x.to_string());

            match row {
                Ok(r) => Ok(r.get(0)),
                Err(x) => Err(x),
            }
        },
        Err(x) => Err(x),
    }
}


async fn add_reacji_message(
    client: Arc<Client>,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
    channel_id: String,
    message_ts: String,
    message: String,
) -> Result<Option<i64>, String> {
    let nick_id: i64 = {
        let rows = client.query_opt(
            "SELECT id FROM nick_metadata WHERE username = ?"
            , &[&username]).await.map_err(|x| x.to_string());

        if let Ok(Some(row)) = rows {
            row.get(0)
        } else {
            let row = client.query_one(
                "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?) RETURNING id"
                , &[&username, &real_name, &user_id, &"SlackServer"]).await.map_err(|x| x.to_string())?;
            row.get(0)
        }
    };

    let channel_id: i64 = {
        let rows = client.query_opt(
            "SELECT id FROM chan_metadata WHERE channel = ?"
            , &[&channel_id]).await.map_err(|x| x.to_string());

        if let Ok(Some(row)) = rows {
            row.get(0)
        } else {
            let row = client.query_one(
                "INSERT INTO chan_metadata (channel) VALUES (?) RETURNING id"
                , &[&channel_id]).await.map_err(|x| x.to_string())?;
            row.get(0)
        }
    };

    // Insert the reacji_message content now
    let reacji_message_id: i64 = {
        let rows = client.query_opt(
            "SELECT id, nick_id, message FROM reacji_message WHERE ts = ? AND chan_id = ?"
            , &[&message_ts, &channel_id]).await.map_err(|x| x.to_string());

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
            id
        } else {
            let row = client.query_one(
                "INSERT INTO reacji_message (ts, chan_id, nick_id, message) VALUES (?, ?, ?, ?) RETURNING id"
                , &[&message_ts, &channel_id, &nick_id, &message]).await.map_err(|x| x.to_string())?;
            row.get(0)
        }
    };

    Ok(Some(reacji_message_id))
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
) -> Result<Option<i64>, String> {
    let nick_id: i64 = {
        let rows = client.query_opt(
            "SELECT id FROM nick_metadata WHERE username = ?"
            , &[&username]).await.map_err(|x| x.to_string());

        if let Ok(Some(row)) = rows {
            row.get(0)
        } else {
            let row = client.query_one(
                "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES (?, ?, ?, ?) RETURNING id"
                , &[&username, &real_name, &user_id, &"SlackServer"]).await.map_err(|x| x.to_string())?;
            row.get(0)
        }
    };

    let ts = timestamp.to_rfc3339();
    let ts = ts.trim_end_matches("+00:00");

    // Insert the reacji into the database
    let row = client.query_one(
        "INSERT INTO reacji_votes
            (voted_at, by_whom_name, action, reacji_message_id, amount, nick_id)
        VALUES
            (?, ?, ?, ?, ?, ?)
        RETURNING id"
        , &[&ts, &username, &action, &message_id, &amount, &nick_id]).await.map_err(|x| x.to_string())?;
    Ok(Some(row.get(0)))
}
