use slack_api as slack;

use std::sync::Arc;

// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;
use slack_api::requests::SlackWebRequestSender;
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

    pub async fn get_user_name(&self, user_id: &str) -> Option<String> {
        let user = self.get_user(user_id).await;
        user.map(|u| u.real_name.clone())
    }

    pub async fn get_user_display(&self, user_id: &str) -> Option<String> {
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
}
