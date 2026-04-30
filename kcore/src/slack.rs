use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;

use std::clone::Clone;
use std::future::Future;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Result as AResult;

use log::warn;

use quick_cache::sync::Cache;

pub trait SlackSender: HttpSender + Clone + Send + Sync + Sized {}
impl SlackSender for ReqwestSender {}

pub trait HttpSender {
    fn send(
        &self,
        request: reqwest::RequestBuilder,
    ) -> impl Future<Output = AResult<reqwest::Response>> + Send;
}

#[derive(Clone)]
pub struct ReqwestSender;
impl HttpSender for ReqwestSender {
    // TODO: Better support rate limiting - https://api.slack.com/apis/rate-limits
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        request.send().await.map_err(Into::into)
    }
}

#[derive(Clone)]
pub struct Client<S: HttpSender = ReqwestSender> {
    url: String,
    // Handles user_not_found via mapping to None
    user_cache: Arc<Cache<String, Option<User>>>,
    app_token: String,
    bot_token: String,
    http: reqwest::Client,
    sender: S,
}

#[derive(Deserialize, Debug)]
struct Envelope {
    ok: bool,
    warning: Option<String>,
    error: Option<String>,

    #[serde(flatten, default)]
    data: serde_json::Value,
}

fn parse_response<T: DeserializeOwned>(key: &str, res: &str) -> AResult<T> {
    match parse_response_envelope(key, res) {
        Ok(data) => Ok(data),
        Err(Some(err)) => Err(anyhow!("Slack error: {err}")),
        Err(None) => Err(anyhow!("Bad Slack API - got {{ok: false}} with no error information")),
    }
}

fn parse_response_envelope<T: DeserializeOwned>(key: &str, res: &str) -> Result<T, Option<String>> {
    let env: Envelope = serde_json::from_str(res).map_err(|e| Some(e.to_string()))?;

    if env.ok {
        // There may sometime be warning with a ok response, log it
        if let Some(warning) = env.warning {
            warn!("Slack api response warning: {warning}");
        }
        Ok(serde_json::from_value(env.data[key].clone()).map_err(|e| Some(e.to_string()))?)
    } else {
        Err(env.error)
    }
}

#[derive(Deserialize, Clone, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct User {
    #[serde(rename = "name")]
    pub username: String,
    pub real_name: String,
    pub is_bot: bool,

    #[serde(flatten)]
    pub timezone: Timezone,
}

#[derive(Deserialize, Clone, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct Timezone {
    #[serde(rename = "tz_label")]
    pub label: String,
    pub tz: String,
    #[serde(rename = "tz_offset")]
    pub offset: i64,
}

#[derive(Debug, Deserialize, Clone, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum ConversationHistoryMessage {
    Message {
        text: String,
        #[serde(rename = "user")]
        user_id: Option<String>,
        bot_id: Option<String>,
    },
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub struct Message {
    pub(crate) channel: String,
    pub(crate) text: String,
    pub(crate) thread_ts: Option<String>,
}

impl Client<ReqwestSender> {
    pub fn new(url: &str, app_token: &str, bot_token: &str, capacity: usize) -> Self {
        Self {
            url: url.to_owned(),
            user_cache: Arc::new(Cache::new(capacity)),
            app_token: app_token.to_owned(),
            bot_token: bot_token.to_owned(),
            http: reqwest::Client::new(),
            sender: ReqwestSender,
        }
    }
}

impl<S: HttpSender> Client<S> {
    #[expect(clippy::use_self)]
    pub fn with_sender(
        sender: S,
        url: &str,
        app_token: &str,
        bot_token: &str,
        capacity: usize,
    ) -> Client<S> {
        Self {
            url: url.to_owned(),
            user_cache: Arc::new(Cache::new(capacity)),
            app_token: app_token.to_owned(),
            bot_token: bot_token.to_owned(),
            http: reqwest::Client::new(),
            sender,
        }
    }

    fn get_slack_url_for_method(&self, method: &str) -> String {
        format!("{}/{}", self.url, method)
    }

    pub async fn socket_connect(&self, debug: bool) -> AResult<String> {
        let url = self.get_slack_url_for_method("apps.connections.open");
        let conn: String = parse_response(
            "url",
            &self
                .sender
                .send(
                    self.http
                        .post(url)
                        .header("Content-type", "application/x-www-form-urlencoded")
                        .header("Authorization", format!("Bearer {}", self.app_token)),
                )
                .await?
                .text()
                .await?,
        )?;

        Ok(if debug {
            format!("{conn}&debug_reconnects=true")
        } else {
            conn
        })
    }

    // TODO: maybe retry if the post_message fails
    pub async fn post_message(&self, message: Message) -> AResult<String> {
        let url = self.get_slack_url_for_method("chat.postMessage");
        parse_response(
            "ts",
            &self
                .sender
                .send(
                    self.http
                        .post(url)
                        .header("Content-type", "application/json")
                        .header("Authorization", format!("Bearer {}", self.bot_token))
                        .body(serde_json::to_string(&message)?),
                )
                .await?
                .text()
                .await?,
        )
    }

    pub async fn get_user(&self, user_id: &str) -> AResult<Option<User>> {
        self.user_cache
            .get_or_insert_async(user_id, async {
                // TODO: Improve the retry strat instead of straight up failing.
                let url = self.get_slack_url_for_method("users.info");
                match parse_response_envelope::<User>(
                    "user",
                    &self
                        .sender
                        .send(
                            self.http
                                .get(url)
                                .header("Content-type", "application/json")
                                .header("Authorization", format!("Bearer {}", self.bot_token))
                                .query(&vec![("user", &user_id)]),
                        )
                        .await?
                        .text()
                        .await?,
                ) {
                    Ok(user) => Ok(Some(user)),
                    Err(Some(err)) => {
                        if err == "user_not_found" {
                            Ok(None)
                        } else {
                            Err(anyhow!("Slack error: {err}"))
                        }
                    },
                    Err(None) => Err(anyhow!("Bad Slack API - got {{ok: false}} with no error information"))?,
                }
            })
            .await
    }

    pub async fn get_message(
        &self,
        channel_id: &str,
        message_ts: &str,
    ) -> AResult<Option<ConversationHistoryMessage>> {
        let url = self.get_slack_url_for_method("conversations.history");
        let params = vec![
            Some(("channel", channel_id.to_owned())),
            Some(("latest", message_ts.to_owned())),
            Some(("inclusive", "true".to_owned())),
            Some(("limit", "1".to_owned())),
        ];
        let mess: Vec<ConversationHistoryMessage> = parse_response(
            "messages",
            &self
                .sender
                .send(
                    self.http
                        .get(url)
                        .header("Content-type", "application/json")
                        .header("Authorization", format!("Bearer {}", self.bot_token))
                        .query(&params),
                )
                .await?
                .text()
                .await?,
        )?;

        match mess.len() {
            0 => Ok(None),
            1 => Ok(mess.into_iter().next()),
            _ => Err(anyhow!("Malformed messages: {mess:?}")),
        }
    }
}

#[cfg(test)]
mod test_slack_client {
    use super::*;
    use http::response;

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
        Client::with_sender(
            PanicSender,
            "http://localhost/api",
            "app_token",
            "bot_token",
            10,
        )
    }

    #[tokio::test]
    async fn test_socket_connect() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "url": "http://localhost/websocket"
        }"#,
        );
        let result = client.socket_connect(false).await;

        assert_eq!(result.unwrap(), "http://localhost/websocket");
    }

    #[tokio::test]
    async fn test_fail_socket_connect() {
        let client = client_response(
            200,
            r#"{
            "ok": false,
            "error": "gone_bye"
        }"#,
        );
        let result = client.socket_connect(false).await;

        assert_eq!(format!("{}", result.unwrap_err()), "Slack error: gone_bye");
    }

    #[tokio::test]
    async fn test_one_message() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "messages": [{
                "type": "message",
                "text": "Asdf",
                "user": "userId"
            }]
        }"#,
        );
        let result = client.get_message("whatever", "whenever").await;

        assert_eq!(
            result.unwrap(),
            Some(ConversationHistoryMessage::Message {
                text: "Asdf".into(),
                user_id: Some("userId".into()),
                bot_id: None
            })
        );
    }

    #[tokio::test]
    async fn test_two_message() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "messages": [{
                "type": "message",
                "text": "Asdf",
                "user": "userId"
            }, {
                "type": "message",
                "text": "Two"
            }]
        }"#,
        );
        let result = client.get_message("whatever", "whenever").await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_bot_message() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "messages": [{
                "type": "message",
                "text": "Asdf",
                "bot_id": "userId"
            }]
        }"#,
        );
        let result = client.get_message("whatever", "whenever").await;

        assert_eq!(
            result.unwrap(),
            Some(ConversationHistoryMessage::Message {
                text: "Asdf".into(),
                user_id: None,
                bot_id: Some("userId".into())
            })
        );
    }

    #[tokio::test]
    async fn test_bad_message() {
        let client = client_response(
            200,
            r#"{
            "ok": false,
            "error": "bad_user"
        }"#,
        );
        let result = client.get_message("whatever", "whenever").await;

        assert_eq!(format!("{}", result.unwrap_err()), "Slack error: bad_user");
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
            username: "dn".into(),
            real_name: "rn".into(),
            is_bot: false,
            timezone: Timezone {
                label: "Den".into(),
                tz: "MST".into(),
                offset: 1234,
            },
        };
        client.user_cache.insert("whoever".into(), Some(user.clone()));

        let result = client.get_user("whoever").await;

        assert_eq!(result.unwrap(), Some(user));
    }

    #[tokio::test]
    async fn test_get_user_not_in_cache() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "user": {
                "name": "dn",
                "real_name": "rn",
                "is_bot": false,
                "tz_label": "Den",
                "tz": "MST",
                "tz_offset": 1234
            }
        }"#,
        );
        let result = client.get_user("whoever").await;

        let user = User {
            username: "dn".into(),
            real_name: "rn".into(),
            is_bot: false,
            timezone: Timezone {
                label: "Den".into(),
                tz: "MST".into(),
                offset: 1234,
            },
        };

        assert_eq!(result.unwrap(), Some(user.clone()));

        let ud = client.user_cache.get("whoever");
        match ud {
            Some(ud) => assert_eq!(ud.clone(), Some(user)),
            None => panic!("Should have been in the cache"),
        };
    }

    #[tokio::test]
    async fn test_get_user_not_in_cache_error() {
        let client = client_response(
            200,
            r#"{
            "ok": false,
            "error": "user_gone"
        }"#,
        );
        let result = client.get_user("whoever").await;

        assert_eq!(format!("{}", result.unwrap_err()), "Slack error: user_gone");
    }

    #[tokio::test]
    async fn test_get_user_user_not_found_cached() {
        let client = client_response(
            200,
            r#"{
            "ok": false,
            "error": "user_not_found"
        }"#,
        );
        let result = client.get_user("whoever").await;

        // Should return None for a user that is not found
        assert_eq!(result.unwrap(), None);

        // Validate that its stored in the cache as None
        let ud = client.user_cache.get("whoever");
        match ud {
            Some(None) => (),
            Some(Some(_)) => panic!("Should have not stored a user here"),
            None => panic!("Should have stored a None in the cache"),
        };
    }

    #[tokio::test]
    async fn test_post_message() {
        let client = client_response(
            200,
            r#"{
            "ok": true,
            "ts": "whenever"
        }"#,
        );
        let result = client
            .post_message(Message {
                channel: "a".into(),
                text: "b".into(),
                thread_ts: None,
            })
            .await;

        assert_eq!(result.unwrap(), "whenever");
    }

    #[tokio::test]
    async fn test_post_message_error() {
        let client = client_response(
            200,
            r#"{
            "ok": false,
            "error": "try_again"
        }"#,
        );
        let result = client
            .post_message(Message {
                channel: "a".into(),
                text: "b".into(),
                thread_ts: None,
            })
            .await;

        assert_eq!(format!("{}", result.unwrap_err()), "Slack error: try_again");
    }

    #[tokio::test]
    async fn test_malformed_envelope() {
        let client = client_response(
            200,
            r#"{
            "ok": false
        }"#,
        );
        let result = client
            .post_message(Message {
                channel: "a".into(),
                text: "b".into(),
                thread_ts: None,
            })
            .await;

        assert_eq!(
            format!("{}", result.unwrap_err()),
            "Bad Slack API - got {ok: false} with no error information"
        );
    }

    #[tokio::test]
    async fn test_malformed_json_envelope() {
        let client = client_response(
            200,
            r#"{
            "ok": "gibbish"
        }"#,
        );
        let result = client
            .post_message(Message {
                channel: "a".into(),
                text: "b".into(),
                thread_ts: None,
            })
            .await;
        assert!(result.is_err());
    }
}
