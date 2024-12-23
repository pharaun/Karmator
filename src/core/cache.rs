use serde::Deserialize;

use std::future::Future;
use std::clone::Clone;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Result as AResult;

// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;

use crate::core::event::Message;


pub trait HttpSender {
    fn send(&self, request: reqwest::RequestBuilder) -> impl Future<Output = AResult<reqwest::Response>> + Send;
}

#[derive(Clone)]
pub struct ReqwestSender;
impl HttpSender for ReqwestSender {
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        request.send().await.map_err(|e| e.into())
    }
}

#[derive(Clone)]
pub struct Cache<S: HttpSender=ReqwestSender> {
    url: String,
    user_cache: Arc<DashMap<String, User>>,
    app_token: String,
    bot_token: String,
    client: reqwest::Client,
    sender: S,
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

impl Cache<ReqwestSender> {
    pub fn new(url: &str, app_token: &str, bot_token: &str) -> Cache {
        Cache {
            url: url.to_string(),
            user_cache: Arc::new(DashMap::new()),
            app_token: app_token.to_string(),
            bot_token: bot_token.to_string(),
            client: reqwest::Client::new(),
            sender: ReqwestSender,
        }
    }
}

impl<S: HttpSender> Cache<S> {
    pub fn with_sender(sender: S, url: &str, app_token: &str, bot_token: &str) -> Cache<S> {
        Cache {
            url: url.to_string(),
            user_cache: Arc::new(DashMap::new()),
            app_token: app_token.to_string(),
            bot_token: bot_token.to_string(),
            client: reqwest::Client::new(),
            sender: sender,
        }
    }

    fn get_slack_url_for_method(&self, method: &str) -> String {
        format!("{}/{}", self.url, method)
    }

    pub async fn socket_connect(&self, debug: bool) -> AResult<String> {
        let url = self.get_slack_url_for_method("apps.connections.open");
        let conn = self.sender.send(
            self.client.post(url)
                .header("Content-type", "application/x-www-form-urlencoded")
                .header("Authorization", format!("Bearer {}", self.app_token))
            ).await?.json::<Conn>().await?;

        println!("Socket Url: {:?}", conn);

        Ok(if debug {
            format!("{}&debug_reconnects=true", conn.url)
        } else {
            conn.url
        })
    }

    pub async fn post_message(&self, message: Message) -> AResult<reqwest::Response> {
        let url = self.get_slack_url_for_method("chat.postMessage");
        Ok(self.sender.send(
            self.client.post(url)
                .header("Content-type", "application/json")
                .header("Authorization", format!("Bearer {}", self.bot_token))
                .body(serde_json::to_string(&message).unwrap())
            ).await?)
    }

    pub async fn is_user_bot(&self, user_id: &str) -> Option<bool> {
        let user = self.get_user(user_id).await;
        user.unwrap().map(|u| u.is_bot)
    }

    pub async fn get_user_real_name(&self, user_id: &str) -> Option<String> {
        let user = self.get_user(user_id).await;
        user.unwrap().map(|u| u.real_name.clone())
    }

    pub async fn get_username(&self, user_id: &str) -> Option<String> {
        let user = self.get_user(user_id).await;
        user.unwrap().map(|u| u.display_name.clone())
    }

    pub async fn get_user_tz(&self, user_id: &str) -> Option<Timezone> {
        let user = self.get_user(user_id).await;
        user.unwrap().map(|u| u.timezone.clone())
    }

    async fn get_user(&self, user_id: &str) -> AResult<Option<User>> {
        let ud = self.user_cache.get(user_id);
        match ud {
            Some(ud) => Ok(Some(ud.clone())),
            None     => {
                let url = self.get_slack_url_for_method("users.info");
                let user = self.sender.send(
                    self.client.get(url)
                        .header("Content-type", "application/json")
                        .header("Authorization", format!("Bearer {}", self.bot_token))
                        .query(&vec![("user", &user_id)])
                    ).await?.json::<UserWrap>().await.map(|uw| uw.user)?;

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
                Ok(Some(user))
            },
        }
    }

    // TODO: find a better location for this, this is an abuse of the cache
    pub async fn get_message(
        &self,
        channel_id: &str,
        message_ts: &str,
    ) -> AResult<Option<ConversationHistoryMessage>> {
        let url = self.get_slack_url_for_method("conversations.history");
        let params = vec![
            Some(("channel", channel_id.to_string())),
            Some(("latest", message_ts.to_string())),
            Some(("inclusive", "true".to_string())),
            Some(("limit", "1".to_string())),
        ];
        let mess = self.sender.send(
            self.client.get(url)
                .header("Content-type", "application/json")
                .header("Authorization", format!("Bearer {}", self.bot_token))
                .query(&params)
            ).await?.json::<ConversationHistoryResult>().await.map(|mw| mw.messages)?;

        if mess.len() != 1 {
            Err(anyhow!("Malformed messages: {:?}", mess).into())
        } else {
            mess.first().ok_or(anyhow!("Shouldn't happen")).map(|m| Some(m.clone()))
        }
    }
}

#[cfg(test)]
mod test_cache {
    use serde_json::json;

    use http::response;
    use tokio::sync::RwLock;
    use super::*;

    struct FakeSender(u16, &'static str);
    impl HttpSender for FakeSender {
        async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
            let mut builder = response::Builder::new().status(self.0);
            let response = builder.body(self.1)?;
            let response = response.into();
            Ok(response)
        }
    }

    fn cache_response(status: u16, body: &'static str) -> Cache<FakeSender> {
        let sender = FakeSender(status, body);
        Cache::with_sender(sender, "http://localhost/api", "app_token", "bot_token")
    }

    #[tokio::test]
    async fn test_socket_connect() {
        let cache = cache_response(200, r#"{
            "ok": true,
            "url": "http://localhost/websocket"
        }"#);
        let result = cache.socket_connect(false).await;

        assert_eq!(result.unwrap(), "http://localhost/websocket");
    }
}
