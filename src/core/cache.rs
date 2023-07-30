use serde::Deserialize;

use std::sync::Arc;

// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;
use std::clone::Clone;

use crate::core::event::Message;


// TODO: add settings, and have it or something peroidically query the database to load latest
// settings into the cache, so that things can grab the settings they need from the settings cache
#[derive(Clone)]
pub struct Cache {
    user_cache: Arc<DashMap<String, User>>,
    slack_token: String,
    slack_client: reqwest::Client,
}

#[derive(Clone)]
struct User {
    display_name: String,
    real_name: String,
    is_bot: bool,
    timezone: Timezone,
}

#[derive(Clone, Debug)]
pub struct Timezone {
    pub label: String,
    pub tz: String,
    pub offset: i64,
}

#[derive(Deserialize, Debug)]
struct Conn {
    #[allow(dead_code)]
    ok: bool,
    url: String
}

#[derive(Deserialize, Debug)]
struct UserWrap {
    #[allow(dead_code)]
    ok: bool,
    user: JUser,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct JUser {
    name: String,
    real_name: String,
    is_bot: bool,
    tz_label: String,
    tz: String,
    tz_offset: f32,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ConversationHistoryResult {
    ok: bool,
    messages: Vec<ConversationHistoryMessage>,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ConversationHistoryMessage {
    Message {
        text: String,
        #[serde(rename = "user")]
        user_id: Option<String>,
    },
}


impl Cache {
    pub fn new(token: &str) -> Cache {
        Cache {
            user_cache: Arc::new(DashMap::new()),
            slack_token: token.to_string(),
            slack_client: reqwest::Client::new(),
        }
    }

    pub async fn socket_connect(&self, debug: bool) -> Result<String, String> {
        let url = get_slack_url_for_method("apps.connections.open");
        let res = self.slack_client.post(url)
            .header("Content-type", "application/x-www-form-urlencoded")
            .header("Authorization", format!("Bearer {}", self.slack_token))
            .send()
            .await.map_err(
                |x| format!("{:?}", x)
            )?.text()
            .await.map_err(
                |x| format!("{:?}", x)
            )?;

        let conn = serde_json::from_str::<Conn>(
            &res
        ).map_err(
            |x| format!("{:?}", x)
        )?;

        println!("Socket Url: {:?}", conn);

        Ok(if debug {
            format!("{}&debug_reconnects=true", conn.url)
        } else {
            conn.url
        })
    }

    pub async fn post_message(&self, message: Message) -> Result<reqwest::Response, String> {
        let url = get_slack_url_for_method("chat.postMessage");
        self.slack_client.post(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", self.slack_token))
            // TODO: remove the unwrap here
            .body(serde_json::to_string(&message).unwrap())
            .send()
            .await.map_err(
                |x| format!("{:?}", x)
            )
    }

    pub async fn is_user_bot(&self, user_id: &str) -> Option<bool> {
        let user = self.get_user(user_id).await;
        user.map(|u| u.is_bot)
    }

    pub async fn get_user_real_name(&self, user_id: &str) -> Option<String> {
        let user = self.get_user(user_id).await;
        user.map(|u| u.real_name.clone())
    }

    pub async fn get_username(&self, user_id: &str) -> Option<String> {
        let user = self.get_user(user_id).await;
        user.map(|u| u.display_name.clone())
    }

    pub async fn get_user_tz(&self, user_id: &str) -> Option<Timezone> {
        let user = self.get_user(user_id).await;
        user.map(|u| u.timezone.clone())
    }

    async fn get_user(&self, user_id: &str) -> Option<User> {
        let ud = self.user_cache.get(user_id);
        match ud {
            Some(ud) => Some(ud.clone()),
            None     => {
                let url = get_slack_url_for_method("users.info");
                let res = self.slack_client.get(url)
                    .header("Content-type", "application/json")
                    .header("Authorization", format!("Bearer {}", self.slack_token))
                    .query(&vec![("user", &user_id)])
                    .send()
                    .await.map_err(
                        |x| format!("{:?}", x)
                    ).ok()?
                    .text()
                    .await.map_err(
                        |x| format!("{:?}", x)
                    ).ok()?;

                let user = serde_json::from_str::<UserWrap>(
                    &res
                ).map(
                    |uw| uw.user
                ).map_err(
                    |x| format!("{:?}", x)
                ).ok()?;

                let user = User {
                    display_name: user.name,
                    real_name: user.real_name,
                    is_bot: user.is_bot,
                    timezone: Timezone {
                        label: user.tz_label,
                        tz: user.tz,
                        offset: user.tz_offset as i64,
                    },
                };

                self.user_cache.insert(user_id.to_string(), user.clone());
                Some(user)
            },
        }
    }

    // TODO: find a better location for this, this is an abuse of the cache
    pub async fn get_message(&self, channel_id: &str, message_ts: &str) -> Option<ConversationHistoryMessage> {
        let url = get_slack_url_for_method("conversations.history");
        let params = vec![
            Some(("channel", channel_id.to_string())),
            Some(("latest", message_ts.to_string())),
            Some(("inclusive", "true".to_string())),
            Some(("limit", "1".to_string())),
        ];
        let res = self.slack_client.get(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", self.slack_token))
            .query(&params)
            .send()
            .await.map_err(
                |x| format!("{:?}", x)
            ).ok()?
            .text()
            .await.map_err(
                |x| format!("{:?}", x)
            ).ok()?;

        let mess = serde_json::from_str::<ConversationHistoryResult>(
            &res
        ).map(
            |mw| mw.messages
        ).map_err(
            |x| format!("{:?}", x)
        ).ok()?;

        let res = if mess.len() != 1 {
            Err(format!("Malformed messages: {:?}", mess))
        } else {
            mess.first().ok_or("Shouldn't happen".to_string())
        };

        match res {
            Ok(msg) => Some(msg.clone()),
            Err(x) => {
                eprintln!("ERROR [Cache]: {}", x);
                None
            },
        }
    }
}


fn get_slack_url_for_method(method: &str) -> String {
    format!("https://slack.com/api/{}", method)
}
