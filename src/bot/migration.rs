use std::collections::HashMap;

use rusqlite as rs;
use serde::Deserialize;
use serde_json::json;

use tokio::sync::mpsc;

use crate::core::database::DbResult;
use crate::core::database::Query;
use crate::core::database::send_query;


#[derive(Clone)]
pub struct Migration {
    modern_bot_token: String,
    legacy_app_token: String,
    slack_client: reqwest::Client,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ChannelsWrap {
    ok: bool,
    channels: Vec<Channel>,
    response_metadata: Option<ResponseMetadata>,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ChannelWrap {
    ok: bool,
    channel: Channel,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ResponseMetadata {
    // If "" then is done.
    next_cursor: Option<String>
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub struct Channel {
    pub id: String,
    pub name: String,

    // Flags
    pub is_member: Option<bool>,
    pub is_private: bool,
    pub is_channel: bool,
    pub is_archived: bool,
}

#[derive(Debug)]
pub struct ChannelsResult {
    pub channels: Vec<Channel>,
    pub next_cursor: Option<String>
}

impl Migration {
    pub fn new(modern_bot_token: &str, legacy_app_token: &str) -> Migration {
        Migration {
            modern_bot_token: modern_bot_token.to_string(),
            legacy_app_token: legacy_app_token.to_string(),
            slack_client: reqwest::Client::new(),
        }
    }

    // Query legacy bot token
    pub async fn get_channels(
        &self,
        limit: u16,
        // public_channel, private_channel, mpim, im
        types: Vec<&str>,
        // Pagnation
        cursor: Option<String>,
        legacy: bool,
        users: bool
    ) -> Result<ChannelsResult, String> {
        let url = if users {
            get_slack_url_for_method("users.conversations")
        } else {
            get_slack_url_for_method("conversations.list")
        };

        let mut query = vec![
            ("limit", limit.to_string()),
            ("exclude_archived", "true".to_string()),
            ("types", types.join(","))
        ];
        match cursor {
            Some(c) => query.push(("cursor", c)),
            None => (),
        }

        let token = if legacy { &self.legacy_app_token } else { &self.modern_bot_token };
        let res = self.slack_client.get(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", token))
            .query(&query)
            .send()
            .await.map_err(
                |x| format!("err1: {:?}", x)
            )?
            .text()
            .await.map_err(
                |x| format!("err2: {:?}", x)
            )?;

        let channels_wrap = serde_json::from_str::<ChannelsWrap>(
            &res
        ).map_err(
            |x| format!("err3: {:?}", x)
        )?;

        match channels_wrap.response_metadata {
            Some(ResponseMetadata {
                next_cursor: Some(nc)
            }) if !nc.is_empty() => Ok(ChannelsResult {
                channels: channels_wrap.channels,
                next_cursor: Some(nc)
            }),
            _ => Ok(ChannelsResult {
                channels: channels_wrap.channels,
                next_cursor: None
            })
        }
    }

    // Query Modern bot token
    pub async fn get_channel_info(
        &self,
        channel: &str,
        legacy: bool
    ) -> Result<Channel, String> {
        let url = get_slack_url_for_method("conversations.info");
        let token = if legacy { &self.legacy_app_token } else { &self.modern_bot_token };
        let res = self.slack_client.get(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", token))
            .query(&vec![("channel", channel)])
            .send()
            .await.map_err(
                |x| format!("err1: {:?}", x)
            )?
            .text()
            .await.map_err(
                |x| format!("err2: {:?}", x)
            )?;

        let channel_wrap = serde_json::from_str::<ChannelWrap>(
            &res
        ).map_err(
            |x| format!("err3: {:?}", x)
        )?;

        Ok(channel_wrap.channel)
    }

    // Query Modern bot token
    pub async fn join_channel(
        &self,
        channel: &str,
        legacy: bool
    ) -> Result<Channel, String> {
        let url = get_slack_url_for_method("conversations.join");
        let token = if legacy { &self.legacy_app_token } else { &self.modern_bot_token };

        let mut map = HashMap::new();
        map.insert("channel", channel);

        let res = self.slack_client.post(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", token))
            .json(&map)
            .send()
            .await.map_err(
                |x| format!("err1: {:?}", x)
            )?
            .text()
            .await.map_err(
                |x| format!("err2: {:?}", x)
            )?;

        let channel_wrap = serde_json::from_str::<ChannelWrap>(
            &res
        ).map_err(
            |x| format!("err3: {:?}", x)
        )?;

        Ok(channel_wrap.channel)
    }

    // Query legacy bot token
    pub async fn post_message(
        &self,
        channel: &str,
        text: &str,
        legacy: bool
    ) -> Result<(), String> {
        let url = get_slack_url_for_method("chat.postMessage");
        let token = if legacy { &self.legacy_app_token } else { &self.modern_bot_token };

        let body = json!({
            "channel": channel,
            "as_user": true,
            "blocks": [{
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": text
                }
            }]
        });

        let res = self.slack_client.post(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", token))
            .body(body.to_string())
            .send()
            .await.map_err(
                |x| format!("err1: {:?}", x)
            )?
            .text()
            .await.map_err(
                |x| format!("err2: {:?}", x)
            )?;
        Ok(())
    }
}

//	/* Outcome
//	 * 0 = To-do
//	 * 1 = Done/confirmed
//	 * 2?? = Failed
//	 * 	00 = Generic Failure
//	 */
//	outcome INTEGER NOT NULL,
pub async fn update_channel(
    sql_tx: &mut mpsc::Sender<Query>,
    channel_id: String,
    outcome: u16,
) {
    let _ = send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let mut stmt = conn.prepare("UPDATE chan_status SET outcome = ? WHERE channel_id = ?")?;
            stmt.execute(rs::params![outcome, channel_id])?;
            Ok(())
        })
    ).await;
}

pub async fn upsert_channel(
    sql_tx: &mut mpsc::Sender<Query>,
    channel_id: String,
    channel_name: String,
) {
    let _ = send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let mut stmt = conn.prepare("SELECT id FROM chan_status WHERE channel_id = ?")?;
            let mut rows = stmt.query(rs::params![channel_id])?;

            if let Ok(None) = rows.next() {
                let mut stmt = conn.prepare(
                    "INSERT INTO chan_status (channel_id, channel_name, outcome) VALUES (?, ?, ?)"
                )?;
                stmt.insert(rs::params![channel_id, channel_name, 0])?;
            }
            Ok(())
        })
    ).await;
}

pub async fn get_all_channel_ids(
    sql_tx: &mut mpsc::Sender<Query>,
) -> DbResult<Vec<(String, u16)>> {
    send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let mut stmt = conn.prepare("SELECT channel_id, outcome FROM chan_status")?;
            let mut rows = stmt.query([])?;

            let mut ret: Vec<(String, u16)> = vec![];
            while let Ok(Some(row)) = rows.next() {
                let id: String = row.get(0)?;
                let outcome: u16 = row.get(1)?;

                ret.push((id, outcome));
            }
            Ok(ret)
        })
    ).await
}

pub async fn delete_channel(
    sql_tx: &mut mpsc::Sender<Query>,
    channel_id: String,
) {
    let _ = send_query(
        sql_tx,
        Box::new(move |conn: &mut rs::Connection| {
            let mut stmt = conn.prepare("DELETE FROM chan_status WHERE channel_id = ?")?;
            stmt.execute(rs::params![channel_id])?;
            Ok(())
        })
    ).await;
}

fn get_slack_url_for_method(method: &str) -> String {
    format!("https://slack.com/api/{}", method)
}
