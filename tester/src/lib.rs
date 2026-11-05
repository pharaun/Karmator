// This is the test harness for running a "fake" slack server which covers:
// - slack_api (http server)
// - ws_server (websocket server)
// - db (scratch test database for the bot itself)
//
// The rest is supporting infra for those main 3 drivers of the harness
pub mod command;
pub mod db;
pub mod events;
pub mod harness;
pub mod slack_api;
pub mod ws_server;

pub use harness::{Harness, HarnessHandle};
pub use ws_server::{ServerConfig, ServerHandle};
