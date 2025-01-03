use serde::Deserialize;

use std::future::Future;
use std::clone::Clone;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Result as AResult;

use quick_cache::sync::Cache;

use crate::event::Message;


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
pub struct Client<S: HttpSender=ReqwestSender> {
    url: String,
    user_cache: Arc<Cache<String, User>>,
    app_token: String,
    bot_token: String,
    client: reqwest::Client,
    sender: S,
}

#[derive(Clone, Debug, PartialEq)]
struct User {
    display_name: String,
    real_name: String,
    is_bot: bool,
    timezone: Timezone,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Deserialize, Clone, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ConversationHistoryMessage {
    Message {
        text: String,
        #[serde(rename = "user")]
        user_id: Option<String>,
    },
}

impl Client<ReqwestSender> {
    pub fn new(url: &str, app_token: &str, bot_token: &str, capacity: usize) -> Client {
        Client {
            url: url.to_string(),
            user_cache: Arc::new(Cache::new(capacity)),
            app_token: app_token.to_string(),
            bot_token: bot_token.to_string(),
            client: reqwest::Client::new(),
            sender: ReqwestSender,
        }
    }
}

impl<S: HttpSender> Client<S> {
    pub fn with_sender(sender: S, url: &str, app_token: &str, bot_token: &str, capacity: usize) -> Client<S> {
        Client {
            url: url.to_string(),
            user_cache: Arc::new(Cache::new(capacity)),
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

        Ok(if debug {
            format!("{}&debug_reconnects=true", conn.url)
        } else {
            conn.url
        })
    }

    // TODO: slack will return a {ok: false} on failure versus {ok: true} on success, add
    // error handling for this
    pub async fn post_message(&self, message: Message) -> AResult<reqwest::Response> {
        let url = self.get_slack_url_for_method("chat.postMessage");
        self.sender.send(
            self.client.post(url)
                .header("Content-type", "application/json")
                .header("Authorization", format!("Bearer {}", self.bot_token))
                .body(serde_json::to_string(&message)?)
            ).await
    }

    // TODO: improve the error handling of these, since the api call can fail
    // Improve the retry strat instead of straight up failing.
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
        self.user_cache.get_or_insert_async(
            user_id,
            async {
                // TODO: handle the case when a user does not exist:
                // {"ok": false, "error": "user_not_found"}
                // Map this to a None result
                let url = self.get_slack_url_for_method("users.info");
                let user = self.sender.send(
                    self.client.get(url)
                        .header("Content-type", "application/json")
                        .header("Authorization", format!("Bearer {}", self.bot_token))
                        .query(&vec![("user", &user_id)])
                    ).await?.json::<UserWrap>().await.map(|uw| uw.user)?;

                Ok(User {
                    display_name: user.name,
                    real_name: user.real_name,
                    is_bot: user.is_bot,
                    timezone: Timezone {
                        label: user.tz_label,
                        tz: user.tz,
                        offset: user.tz_offset as i64,
                    },
                })
            }
        ).await.map(|u| Some(u))
    }

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
mod test_slack_client {
    use http::response;
    use super::*;

    struct FakeSender(u16, &'static str);
    impl HttpSender for FakeSender {
        async fn send(&self, _request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
            Ok(response::Builder::new().status(self.0).body(self.1)?.into())
        }
    }

    fn client_response(status: u16, body: &'static str) -> Client<FakeSender> {
        let sender = FakeSender(status, body);
        Client::with_sender(sender, "http://localhost/api", "app_token", "bot_token", 10)
    }

    struct PanicSender;
    impl HttpSender for PanicSender {
        async fn send(&self, _request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
            panic!("Shouldn't hit http");
        }
    }

    fn panic_response() -> Client<PanicSender> {
        Client::with_sender(PanicSender, "http://localhost/api", "app_token", "bot_token", 10)
    }

    #[tokio::test]
    async fn test_socket_connect() {
        let client = client_response(200, r#"{
            "ok": true,
            "url": "http://localhost/websocket"
        }"#);
        let result = client.socket_connect(false).await;

        assert_eq!(result.unwrap(), "http://localhost/websocket");
    }

    #[tokio::test]
    async fn test_one_message() {
        let client = client_response(200, r#"{
            "ok": true,
            "messages": [{
                "type": "message",
                "text": "Asdf",
                "user": "userId"
            }]
        }"#);
        let result = client.get_message("whatever", "whenever").await;

        assert_eq!(
            result.unwrap(),
            Some(ConversationHistoryMessage::Message { text: "Asdf".into(), user_id: Some("userId".into()) })
        );
    }

    #[tokio::test]
    async fn test_two_message() {
        let client = client_response(200, r#"{
            "ok": true,
            "messages": [{
                "type": "message",
                "text": "Asdf",
                "user": "userId"
            }, {
                "type": "message",
                "text": "Two"
            }]
        }"#);
        let result = client.get_message("whatever", "whenever").await;

        assert!(result.is_err());
    }

    #[tokio::test]
    #[should_panic]
    async fn test_get_user_verify_panic() {
        let client = panic_response();
        let _ = client.get_user("whoever").await;
    }

    #[tokio::test]
    async fn test_get_user_in_cache() {
        let client = panic_response();

        // Insert user into cache
        let user = User {
            display_name: "dn".into(),
            real_name: "rn".into(),
            is_bot: false,
            timezone: Timezone { label: "Den".into(), tz: "MST".into(), offset: 1234 }
        };
        client.user_cache.insert("whoever".into(), user.clone());

        let result = client.get_user("whoever").await;

        assert_eq!(
            result.unwrap(),
            Some(user)
        );
    }

    #[tokio::test]
    async fn test_get_user_not_in_cache() {
        let client = client_response(200, r#"{
            "ok": true,
            "user": {
                "name": "dn",
                "real_name": "rn",
                "is_bot": false,
                "tz_label": "Den",
                "tz": "MST",
                "tz_offset": 1234
            }
        }"#);
        let result = client.get_user("whoever").await;

        let user = User {
            display_name: "dn".into(),
            real_name: "rn".into(),
            is_bot: false,
            timezone: Timezone { label: "Den".into(), tz: "MST".into(), offset: 1234 }
        };

        assert_eq!(result.unwrap(), Some(user.clone()));

        let ud = client.user_cache.get("whoever");
        match ud {
            Some(ud) => assert_eq!(ud.clone(), user),
            None => panic!("Should have been in the cache"),
        };
    }
}
