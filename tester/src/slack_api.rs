use std::sync::Arc;

use anyhow::Result as AResult;
use anyhow::{Context as _, bail};

use http::response;
use log::{debug, error, warn};
use serde_json::{Value, json};

use kcore::SlackSender;

use crate::monitor::{Activity, Monitor};

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
    monitor: Monitor,
}

impl FakeSlackApi {
    pub fn new(ws_url: &str, monitor: Monitor) -> Self {
        Self {
            ws_url: ws_url.into(),
            monitor,
        }
    }

    fn post_message(&self, body: Option<&[u8]>) {
        let msg = body.and_then(|b| serde_json::from_slice::<Value>(b).ok());
        let Some(msg) = msg else {
            warn!(
                "Unparsable postMessage body: {:?}",
                body.map(String::from_utf8_lossy)
            );
            return;
        };
        let field = |k: &str| msg.get(k).and_then(Value::as_str).map(str::to_owned);
        self.monitor.report(Activity::BotReply {
            channel: field("channel").unwrap_or_default(),
            text: field("text").unwrap_or_default(),
            thread_ts: field("thread_ts"),
        });
    }
}

impl SlackSender for FakeSlackApi {
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        let request = request.build().context("Bad request")?;
        debug!("FakeSlackApi Request: {:?}", request.url().as_str());
        let path = request.url().path();
        self.monitor.report(Activity::ApiCall {
            method: path.trim_start_matches('/').to_owned(),
        });

        let body = match path {
            "/apps.connections.open" => json!({"ok": true, "url": &*self.ws_url}).to_string(),
            "/chat.postMessage" => {
                self.post_message(request.body().and_then(|b| b.as_bytes()));
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
