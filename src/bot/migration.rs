use serde::Deserialize;

#[derive(Clone)]
pub struct Migration {
    legacy_app_token: String,
    slack_client: reqwest::Client,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct ChannelsWrap {
    ok: bool,
    channels: Vec<Channel>,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub struct Channel {
    id: String,
    name: String,

    // Flags
    is_member: bool,
    is_private: bool,
    is_channel: bool,
    is_archived: bool,
}

impl Migration {
    pub fn new(legacy_app_token: &str) -> Migration {
        Migration {
            legacy_app_token: legacy_app_token.to_string(),
            slack_client: reqwest::Client::new(),
        }
    }

    pub async fn get_channels(
        &self,
        limit: u16,
        // public_channel, private_channel, mpim, im
        types: Vec<String>,
        // Pagnation
        cursor: Option<String>
    ) -> Option<Vec<Channel>> {
        let url = get_slack_url_for_method("conversations.list");
        let res = self.slack_client.get(url)
            .header("Content-type", "application/json")
            .header("Authorization", format!("Bearer {}", self.legacy_app_token))
            .query(&vec![("limit", "10"), ("exclude_archived", "true")])
            .send()
            .await.map_err(
                |x| format!("{:?}", x)
            ).ok()?
            .text()
            .await.map_err(
                |x| format!("{:?}", x)
            ).ok()?;

        println!("DEBUG: {:?}", res);

        let channels = serde_json::from_str::<ChannelsWrap>(
            &res
        ).map(
            |cw| cw.channels
        ).map_err(
            |x| format!("{:?}", x)
        ).ok()?;

        Some(channels)
    }
}


fn get_slack_url_for_method(method: &str) -> String {
    format!("https://slack.com/api/{}", method)
}
