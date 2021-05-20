use slack_api as slack;
use slack_api::requests::SlackWebRequestSender;

use serde::Deserialize;

use std::sync::Arc;

// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;
use std::clone::Clone;


// TODO: add settings, and have it or something peroidically query the database to load latest
// settings into the cache, so that things can grab the settings they need from the settings cache
#[derive(Clone)]
pub struct Cache<R: SlackWebRequestSender + Clone> {
    user_cache: Arc<DashMap<String, User>>,
    slack_token: String,
    slack_client: R,
}

#[derive(Clone)]
struct User {
    display_name: String,
    real_name: String,
    is_bot: bool,
}


impl <R: SlackWebRequestSender + Clone> Cache<R> {
    pub fn new(token: &str, client: R) -> Cache<R> {
        Cache {
            user_cache: Arc::new(DashMap::new()),
            slack_token: token.to_string(),
            slack_client: client,
        }
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

    async fn get_user(&self, user_id: &str) -> Option<User> {
        let ud = self.user_cache.get(user_id);
        match ud {
            Some(ud) => Some(ud.clone()),
            None     => {
                let resp = slack::users::info(
                    &self.slack_client,
                    &self.slack_token,
                    &slack::users::InfoRequest { user: &user_id }
                ).await;

                let ud = resp.ok().map(
                    |ui| ui.user
                ).flatten().map(
                    |ui| match (ui.name, ui.real_name, ui.is_bot) {
                        (Some(dn), Some(rn), Some(ib)) => Some(User {display_name: dn, real_name: rn, is_bot: ib}),
                        _ => None,
                    }
                ).flatten();

                match ud {
                    Some(ud) => {
                        self.user_cache.insert(user_id.to_string(), ud.clone());
                        Some(ud)
                    },
                    _ => None,
                }
            },
        }
    }

    // TODO: find a better location for this, this is an abuse of the cache
    pub async fn get_message(&self, channel_id: &str, message_ts: &str) -> Option<ConversationHistoryMessage> {
        let url = get_slack_url_for_method("conversations.history");

        let params = vec![
            Some(("token", self.slack_token.clone())),
            Some(("channel", channel_id.to_string())),
            Some(("latest", message_ts.to_string())),
            Some(("inclusive", "true".to_string())),
            Some(("limit", "1".to_string())),
        ];
        let params = params.into_iter().filter_map(|x| x).collect::<Vec<_>>();

        let res = &self.slack_client.send(
            &url,
            &params[..]
        ).await.map_err(
            |x| format!("{:?}", x)
        ).and_then(|result| {
            serde_json::from_str::<ConversationHistoryResult>(
                &result
            ).or_else(
                |x| Err(format!("{:?}", x))
            )
        });

        let res = res.as_ref().map(|x| {
            let messages = &x.messages;
            if messages.len() != 1 {
                Err(format!("Malformed messages: {:?}", messages))
            } else {
                messages.first().ok_or("Shouldn't happen".to_string())
            }
        });

        match res {
            Ok(Ok(msg)) => Some(msg.clone()),
            Ok(Err(x))  => {
                eprintln!("ERROR [Cache]: {}", x);
                None
            },
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

// This is for the cache message fetcher
// TODO: upstream/find a better location for this
#[derive(Debug, Deserialize)]
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
