use serde_json::{Value, json};

pub const USER: &str = "testname";
pub const CHANNEL: &str = "testchan";

// TODO: talk to the http mock so that it can have the correct
// message for the reacji query to pull from
const REACJI_CHANNEL: &str = "chanid";
const REACJI_TS: &str = "tststs";

pub fn message(text: &str) -> Value {
    json!({
        "type": "message",
        "channel": CHANNEL,
        "user": USER,
        "text": text,
        "ts": "N/A",
    })
}

pub fn reaction_added(reaction: &str) -> Value {
    reaction_event("reaction_added", reaction)
}

pub fn reaction_removed(reaction: &str) -> Value {
    reaction_event("reaction_removed", reaction)
}

fn reaction_event(kind: &str, reaction: &str) -> Value {
    json!({
        "type": kind,
        "user": USER,
        "reaction": reaction,
        "item_user": "N/A",
        "item": {
            "type": "message",
            "channel": REACJI_CHANNEL,
            "ts": REACJI_TS,
        },
        "event_ts": "N/A",
    })
}
