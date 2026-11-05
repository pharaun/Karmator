use std::sync::Arc;

use anyhow::Result as AResult;
use anyhow::{Context as _, bail};

use http::response;
use log::{error, info};
use serde_json::json;

use kcore::SlackSender;

// TODO: replace hardcoded response with a builder so that we can setup custom replies upon api
// calls to help test edge cases in the api logic (also return the callee info for validation of
// the test too)
const POST_MESSAGE: &str = r#"{"ok": true, "ts": "123.123"}"#;

const USERS_INFO: &str = r#"{
    "ok": true,
    "user": {
        "name": "dn",
        "real_name": "rn",
        "is_bot": false,
        "tz_label": "Den",
        "tz": "MST",
        "tz_offset": 1234
    }
}"#;

const CONVERSATIONS_HISTORY: &str = r#"{
    "ok": true,
    "messages": [{
        "type": "message",
        "text": "Asdf",
        "user": "userId"
    }]
}"#;

#[derive(Clone)]
pub struct FakeSlackApi {
    // Handed out by `apps.connections.open`, points at the fake websocket server
    ws_url: Arc<str>,
}

impl FakeSlackApi {
    pub fn new(ws_url: &str) -> Self {
        Self {
            ws_url: ws_url.into(),
        }
    }
}

impl SlackSender for FakeSlackApi {
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        let request = request.build().context("Bad request")?;
        info!("FakeSlackApi Request: {:?}", request.url().as_str());

        let body = match request.url().path() {
            "/apps.connections.open" => json!({"ok": true, "url": &*self.ws_url}).to_string(),
            "/chat.postMessage" => {
                info!(
                    "postMessage body: {:?}",
                    request
                        .body()
                        .and_then(|b| b.as_bytes())
                        .map(String::from_utf8_lossy)
                );
                POST_MESSAGE.to_owned()
            }
            "/users.info" => USERS_INFO.to_owned(),
            "/conversations.history" => CONVERSATIONS_HISTORY.to_owned(),
            path => {
                error!("Unsupported FakeSlackApi url: {path:?}");
                bail!("Unsupported FakeSlackApi url: {path:?}");
            }
        };
        Ok(response::Builder::new().status(200).body(body)?.into())
    }
}
