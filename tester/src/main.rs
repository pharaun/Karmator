use std::env;
use std::io::Write;

use log::{debug, error, info, warn};

use anyhow::anyhow;
use anyhow::Result as AResult;

use deadpool_postgres::{Manager, ManagerConfig, Pool, RecyclingMethod};
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio::sync::watch;
use tokio::time;
use tokio_postgres::NoTls;
use tokio_tungstenite::tungstenite;

use rustyline_async::Readline;
use rustyline_async::ReadlineEvent;

use futures_util::FutureExt;
use futures_util::SinkExt;
use futures_util::StreamExt;

use serde_json::json;

use http::response;

use kcore::default_event_loop;
use kcore::Command;
use kcore::Signal;
use kcore::SlackClient;
use kcore::SlackSender;
use kcore::Watcher;

use karmator::bot::user_event;

#[derive(Clone)]
pub struct Reconnect {
    reconnect: (watch::Sender<bool>, watch::Receiver<bool>),
}

impl Reconnect {
    fn new() -> Self {
        Self {
            reconnect: watch::channel(false),
        }
    }

    pub async fn reconnect(&mut self) {
        if let Err(e) = self.reconnect.1.wait_for(|v| *v == true).await {
            error!("Signal reconnect error: {e:?}");
        }
    }

    pub fn reconnect_now(&mut self) {
        if let Err(e) = self.reconnect.0.send(true) {
            error!("Signal reconnect error: {e:?}");
        }
    }

    pub fn reset_now(&mut self) {
        if let Err(e) = self.reconnect.0.send(false) {
            error!("Signal reconnect error: {e:?}");
        }
    }
}


// Fake HTTP server for dealing with slack http api calls
#[derive(Clone)]
struct FakeSender;
impl SlackSender for FakeSender {
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        if let Ok(request) = request.build() {
            info!("FakeSender Request: {:?}", request.url().as_str());
            match request.url().path() {
                "/apps.connections.open" => {
                    let response = r#"{"ok": true, "url": "ws://127.0.0.1:8080"}"#;
                    Ok(response::Builder::new().status(200).body(response)?.into())
                }
                "/chat.postMessage" => {
                    info!(
                        "postMessage body: {:?}",
                        request
                            .body()
                            .map(|x| x.as_bytes())
                            .flatten()
                            .map(|x| std::str::from_utf8(x))
                    );

                    let response = r#"{"ok": true, "ts": "123.123"}"#;
                    Ok(response::Builder::new().status(200).body(response)?.into())
                }
                "/users.info" => {
                    let response = r#"{
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
                    Ok(response::Builder::new().status(200).body(response)?.into())
                }
                "/conversations.history" => {
                    let response = r#"{
                        "ok": true,
                        "messages": [{
                            "type": "message",
                            "text": "Asdf",
                            "user": "userId"
                        }]
                    }"#;
                    Ok(response::Builder::new().status(200).body(response)?.into())
                }
                x => {
                    error!("Unsupported FakeSender url: {:?}", x);
                    Err(anyhow!("Unsupported FakeSender url: {:?}", x))
                }
            }
        } else {
            Err(anyhow!("Bad request"))
        }
    }
}

#[derive(Debug, PartialEq)]
enum ServerState {
    Hello,
    Event,
    Disconnect,
    Exit,
}

// Fake Websocket server for sending the bot "messages" from slack
async fn websocket_server(
    readyness: oneshot::Sender<()>,
    mut watcher: Watcher,
    mut websocket: mpsc::Receiver<serde_json::Value>,
    mut reconnect: Reconnect,
) -> AResult<()> {
    // Socket + event loop for listening to connection attempts
    let socket = TcpListener::bind("127.0.0.1:8080").await.unwrap();
    let _ = readyness.send(());

    // Interval timers for routine disconnect
    let mut disbeat = time::interval(time::Duration::from_secs(240));
    disbeat.set_missed_tick_behavior(time::MissedTickBehavior::Delay);
    disbeat.tick().await;

    let mut state;
    while let Ok((stream, _)) = socket.accept().await {
        let ws_stream = tokio_tungstenite::accept_async(stream).await.unwrap();
        let (mut ws_write, mut ws_read) = ws_stream.split();

        state = ServerState::Hello;
        info!("Slack Websocket - server established");

        while state != ServerState::Exit && !watcher.should_shutdown() {
            debug!("Current Server State: {:?}", state);
            match state {
                ServerState::Hello => {
                    let response = r#"{"type": "hello", "num_connections": 1}"#;
                    let _ = ws_write.send(tungstenite::Message::from(response)).await;

                    state = ServerState::Event;
                }
                ServerState::Event => {
                    tokio::select! {
                        _ = watcher.shutdown() => {
                            info!("Shutdown signal received");
                            state = ServerState::Exit;
                        },

                        ws_msg = ws_read.next() => {
                            match ws_msg {
                                Some(Ok(tungstenite::Message::Text(msg))) => {
                                    // Expect an acknowledgement for an event sent
                                    //
                                    // Expect an acknowledgement (if not, send again after a delay)
                                    // {"envelope_id": <$unique_identifier_string>}
                                    //
                                    // If one is not gotten after sufficient time, go back to resend recent event
                                    //
                                    // Either transition into Event or Disconnect, for now Disconnect
                                    info!("Client message: {:?}", msg);
                                },
                                Some(Ok(tungstenite::Message::Ping(x))) => {
                                    debug!("Client Ping: {:?}", x);
                                    let _ = ws_write.send(tungstenite::Message::Pong(x)).await;
                                },
                                Some(Ok(tungstenite::Message::Pong(x))) => {
                                    debug!("Client Pong: {:?}", x);
                                },
                                Some(Ok(tungstenite::Message::Close(reason))) => {
                                    info!("Client close reason: {:?}", reason);
                                    state = ServerState::Exit;
                                },
                                Some(Ok(ws_msg)) => {
                                    info!("Server Unknown Event: {:?}", ws_msg);
                                },
                                Some(Err(e)) => {
                                    error!("Server Event - Error: {:?}", e);
                                    state = ServerState::Exit;
                                }
                                None => {
                                    warn!("Server Event - Disconnection");
                                    state = ServerState::Exit;
                                },
                            }
                        },

                        // Send the string as it was received from the repl
                        // TODO: generate a unique envelope_id and validate acknowledgements
                        Some(msg) = websocket.recv() => {
                            info!("Sending to bot: {:?}", msg);
                            let _ = ws_write.send(tungstenite::Message::from(
                                json!({
                                    "type": "events_api",
                                    "envelope_id": "asdf",
                                    "payload": {
                                        "type": "event_callback",
                                        "event": msg
                                    },
                                    "accepts_response_payload": false,
                                }).to_string()
                            )).await;
                        },

                        _ = reconnect.reconnect() => {
                            // Reconnect triggered (there's disbeat too but this is a forced one)
                            info!("Forced Reconnect tester");
                            state = ServerState::Disconnect;
                            reconnect.reset_now();
                        },

                        // This is woken up peroidically to force a reconnection
                        _ = disbeat.tick() => {
                            info!("Perodic Reconnect tester");
                            state = ServerState::Disconnect;
                        }
                    }
                }
                ServerState::Disconnect => {
                    let response = r#"{"type": "disconnect", "reason": "refresh_requested"}"#;
                    let _ = ws_write.send(tungstenite::Message::from(response)).await;

                    // Sleep for a second then exit
                    time::sleep(time::Duration::from_secs(1)).await;
                    state = ServerState::Exit;
                }

                // Do nothing, we are done, exit event loop
                ServerState::Exit => (),
            }
        }
    }
    Ok(())
}

async fn terminal_readline(
    mut rl: rustyline_async::Readline,
    mut stdout: rustyline_async::SharedWriter,
    mut watcher: Watcher,
    websocket: mpsc::Sender<serde_json::Value>,
    mut reconnect: Reconnect,
    pool: &Pool,
) -> AResult<()> {
    let client = pool.get().await?;
    while !watcher.should_shutdown() {
        let _ = rl.flush();
        tokio::select! {
            _ = watcher.shutdown() => info!("Shutdown signal received"),

            command = rl.readline().fuse() => match command {
                Ok(ReadlineEvent::Line(line)) => {
                    let mut line = line.trim().to_string();
                    rl.add_history_entry(line.clone());
                    line.insert_str(0, "!");

                    match kcore::parse_command(&line) {
                        // Send string as it is to the bot via websocket
                        Ok(Command("send", arg)) => {
                            let _ = websocket.send(
                                json!({
                                    "type": "message",
                                    "channel": "testchan",
                                    "user": "testname",
                                    "text": arg.join(" "),
                                    "ts": "N/A",
                                })
                            ).await;
                        },
                        // Truncate all tables to empty to reset the state
                        Ok(Command("truncate", _)) => {
                            let _ = writeln!(stdout, "Truncating all tables");
                            let res = client.execute(
                                "TRUNCATE
                                    votes,
                                    reacji_votes,
                                    reacji_message,
                                    nick_metadata,
                                    chan_metadata,
                                    karma_received_count,
                                    karma_given_count;",
                                &[]
                            ).await;
                            let _ = writeln!(stdout, "Result: {:?}", res);
                        },
                        // Send an reacji event to the bot via websocket
                        // TODO: talk to the http mock so that it can have the correct
                        // message for the reacji query to pull from
                        Ok(Command("addReacji", arg)) => {
                            let _ = websocket.send(
                                json!({
                                    "type": "reaction_added",
                                    "user": "testname",
                                    "reaction": arg.join(" "),
                                    "item_user": "N/A",
                                    "item": {
                                        "type": "message",
                                        "channel": "chanid",
                                        "ts": "tststs",
                                    },
                                    "event_ts": "N/A",
                                })
                            ).await;
                        },
                        Ok(Command("delReacji", arg)) => {
                            let _ = websocket.send(
                                json!({
                                    "type": "reaction_removed",
                                    "user": "testname",
                                    "reaction": arg.join(" "),
                                    "item_user": "N/A",
                                    "item": {
                                        "type": "message",
                                        "channel": "chanid",
                                        "ts": "tststs",
                                    },
                                    "event_ts": "N/A",
                                })
                            ).await;
                        },
                        Ok(Command("reconnect", _)) => {
                            let _ = writeln!(stdout, "Triggering websocket reconnect");
                            reconnect.reconnect_now();
                        },
                        Ok(Command("help", _)) => {
                            let _ = writeln!(stdout, "Commands available: help, send [arg], truncate, addReacji [arg], delReacji [arg], reconnect");
                        },
                        _ => {
                            let _ = writeln!(stdout, "Command not found: \"{}\"", line);
                        },
                    }
                },
                Ok(ReadlineEvent::Eof) => {
                    let _ = watcher.shutdown_now();
                    let _ = writeln!(stdout, "Exiting...");
                },
                Ok(ReadlineEvent::Interrupted) => {
                    let _ = watcher.shutdown_now();
                    let _ = writeln!(stdout, "^C");
                },
                Err(err) => {
                    let _ = watcher.shutdown_now();
                    let _ = writeln!(stdout, "Received err: {:?}", err);
                    let _ = writeln!(stdout, "Exiting...");
                },
            }
        }
    }
    Ok(())
}

// TODO: Figure out how to flesh out the bits we need for running tests in a useful way and then
// provide an adapter to wire those bits into the terminal + main program for actually running
// things, but otherwise setting things up to be ran in tests.
#[tokio::main]
async fn main() -> AResult<()> {
    let postgres_url =
        env::var("POSTGRES_URL").map_err(|_| anyhow!("POSTGRES_URL env var must be set"))?;

    // Initial terminal setup
    let (rl, stdout) = Readline::new("> ".to_string()).unwrap();

    // Logging
    simplelog::WriteLogger::init(
        log::LevelFilter::Debug,
        simplelog::Config::default(),
        stdout.clone(),
    )
    .unwrap();

    // Block till server is ready
    let (tx, rx) = oneshot::channel();

    // Force websocket server to drop connection
    let reconnect = Reconnect::new();

    //*******************
    // Signals bits
    //*******************
    let signal = Signal::new()?;

    //*******************
    // Postgres bits
    //*******************
    let pg_config = postgres_url.parse::<tokio_postgres::Config>()?;

    // Deadpool configuration
    let mgr_config = ManagerConfig {
        recycling_method: RecyclingMethod::Verified,
    };
    let mgr = Manager::from_config(pg_config, NoTls, mgr_config);
    let pool = Pool::builder(mgr).max_size(8).build()?;

    //*******************
    // Terminal bits
    //*******************
    let (ws_tx, ws_rx) = mpsc::channel(10);
    {
        let watcher = signal.get_shutdown_watcher();
        let pool = pool.clone();
        let reconnect = reconnect.clone();
        tokio::spawn(async move {
            // If this exits, something went wrong
            if let Err(e) = terminal_readline(rl, stdout, watcher, ws_tx, reconnect, &pool).await {
                error!("Websocket Error: {:?}", e);
            }
        });
    }

    //*******************
    // Websocket bits
    //*******************
    {
        let watcher = signal.get_shutdown_watcher();
        let reconnect = reconnect.clone();
        tokio::spawn(async move {
            // If this exits something went wrong
            if let Err(e) = websocket_server(tx, watcher, ws_rx, reconnect).await {
                error!("Websocket Error: {:?}", e);
            }
        });
    }

    //*******************
    // Bot bits
    //*******************
    // Wait till the server and all of the other daemons are ready
    // TODO: have it be a more sophsicated readyness check that makes sure sql, terminal,
    // websockets are all ready to go?
    let _ = rx.await;
    let pool = pool.clone();
    default_event_loop(
        SlackClient::with_sender(FakeSender, "http://localhost", "app-token", "bot-token", 10),
        signal,
        |event, slack| {
            let pool = pool.clone();
            tokio::spawn(async move {
                if let Err(e) = user_event::process_user_message(event, slack, &pool).await {
                    error!("user_event::process_user_message error: {:?}", e);
                };
            });
        },
    )
    .await?;

    Ok(())
}
