use slack_api as slack;

use std::sync::Arc;

// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;
use slack_api::requests::SlackWebRequestSender;
use std::clone::Clone;


#[derive(Clone)]
pub struct Cache<R: SlackWebRequestSender + Clone> {
    user_cache: Arc<DashMap<String, String>>,
    slack_token: String,
    slack_client: R,
}


impl <R: SlackWebRequestSender + Clone> Cache<R> {
    pub fn new(token: &str, client: R) -> Cache<R> {
        Cache {
            user_cache: Arc::new(DashMap::new()),
            slack_token: token.to_string(),
            slack_client: client,
        }
    }

    pub async fn get_user_display(&self, user_id: &str) -> Option<String> {
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
                    |ui| ui.name
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
}
