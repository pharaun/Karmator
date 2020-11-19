use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::sync::mpsc;

use serde::Deserialize;
use serde_json::json;

use atomic_counter::AtomicCounter;
use atomic_counter::RelaxedCounter;

use std::sync::Arc;
use std::default::Default;
use std::env;
use std::result::Result;

use chrono::prelude::{Utc, DateTime};
use std::time::Duration;
use humantime::format_duration;

// SQlite worker thread
use std::thread;
use futures::executor::block_on_stream;

use rusqlite as rs;
use std::path::Path;
use tokio::sync::oneshot;

// User id -> display name cache
// TODO: look at some sort of lru or persist this to sqlite instead?
use dashmap::DashMap;


pub type UserCache = Arc<DashMap<String, String>>;


pub async fn get_user_display<R>(
    token: &str,
    client: R,
    user_cache: &UserCache,
    user_id: &String
) -> Option<String>
where
    R: slack::requests::SlackWebRequestSender
{
    let ud = user_cache.get(user_id);
    match ud {
        Some(ud) => Some(ud.clone()),
        None     => {
            let resp = slack_api::users::info(
                &client,
                &token,
                &slack_api::users::InfoRequest { user: &user_id }
            ).await;

            let ud = resp.ok().map(
                |ui| ui.user
            ).flatten().map(
                |ui| ui.name
            ).flatten();

            match ud {
                Some(ud) => {
                    user_cache.insert(user_id.clone(), ud.clone());
                    Some(ud)
                },
                _ => None,
            }
        },
    }
}
