use serde::Deserialize;


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

#[derive(Clone)]
pub struct Migration {
    legacy_app_token: String,
    slack_client: reqwest::Client,
}

impl Migration {
    pub fn new(legacy_app_token: &str) -> Migration {
        Migration {
            legacy_app_token: legacy_app_token.to_string(),
            slack_client: reqwest::Client::new(),
        }
    }

//    pub async fn get_user(&self, user_id: &str) -> Option<User> {
//        let ud = self.user_cache.get(user_id);
//        match ud {
//            Some(ud) => Some(ud.clone()),
//            None     => {
//                let url = get_slack_url_for_method("users.info");
//                let res = self.slack_client.get(url)
//                    .header("Content-type", "application/json")
//                    .header("Authorization", format!("Bearer {}", self.bot_token))
//                    .query(&vec![("user", &user_id)])
//                    .send()
//                    .await.map_err(
//                        |x| format!("{:?}", x)
//                    ).ok()?
//                    .text()
//                    .await.map_err(
//                        |x| format!("{:?}", x)
//                    ).ok()?;
//
//                let user = serde_json::from_str::<UserWrap>(
//                    &res
//                ).map(
//                    |uw| uw.user
//                ).map_err(
//                    |x| format!("{:?}", x)
//                ).ok()?;
//
//                let user = User {
//                    display_name: user.name,
//                    real_name: user.real_name,
//                    is_bot: user.is_bot,
//                    timezone: Timezone {
//                        label: user.tz_label,
//                        tz: user.tz,
//                        offset: user.tz_offset as i64,
//                    },
//                };
//
//                self.user_cache.insert(user_id.to_string(), user.clone());
//                Some(user)
//            },
//        }
//    }
}


fn get_slack_url_for_method(method: &str) -> String {
    format!("https://slack.com/api/{}", method)
}
