use std::env;
use std::sync::Arc;
use std::io::Write;

use log::{debug, info, warn, error};

use anyhow::anyhow;
use anyhow::Result as AResult;

use tokio::time;
use tokio::net::TcpListener;
use tokio_postgres::NoTls;
use tokio::sync::oneshot;
use tokio::sync::watch;
use tokio_tungstenite::tungstenite;

use rustyline_async::Readline;
use rustyline_async::ReadlineEvent;

use futures_util::StreamExt;
use futures_util::SinkExt;
use futures_util::FutureExt;

use http::response;

use kcore::slack;
use kcore::slack::HttpSender;
use kcore::signal;
use kcore::bot;
use kcore::command::Command;
use karmator::bot::user_event;


// Fake HTTP server for dealing with slack http api calls
#[derive(Clone)]
struct FakeSender;

impl HttpSender for FakeSender {
    async fn send(&self, request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        if let Ok(request) = request.build() {
            info!("FakeSender Request: {:?}", request.url().as_str());
            match request.url().path() {
                "/apps.connections.open" => {
                    let response = r#"{"ok": true, "url": "ws://127.0.0.1:8080"}"#;
                    Ok(response::Builder::new().status(200).body(response)?.into())
                },
                "/chat.postMessage" => {
                    info!("postMessage body: {:?}", request.body());

                    let response = r#"{"ok": true}"#;
                    Ok(response::Builder::new().status(200).body(response)?.into())
                },
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
                },
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
                },
                x => {
                    error!("Unsupported FakeSender url: {:?}", x);
                    Err(anyhow!("Unsupported FakeSender url: {:?}", x))
                },
            }
        } else {
            Err(anyhow!("Bad request"))
        }
    }
}

#[derive(Debug, PartialEq)]
enum ServerState {
    Hello, Event, Disconnect, Exit
}

// Fake Websocket server for sending the bot "messages" from slack
async fn websocket_server(
    readyness: oneshot::Sender<()>,
    mut signal: signal::Signal,
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

        while state != ServerState::Exit && !signal.should_shutdown() {
            debug!("Current Server State: {:?}", state);
            match state {
                ServerState::Hello => {
                    let response = r#"{"type": "hello", "num_connections": 1}"#;
                    let _ = ws_write.send(tungstenite::Message::from(response)).await;

                    state = ServerState::Event;
                }
                ServerState::Event => {
                    // For events from the prompt to the bot:
                    // {"payload": <event_payload>, "envelope_id": <unique_identifier_string>,
                    //  "type": <event_type_enum>, "accepts_response_payload": <accepts_response_payload_bool>}
                    // Where type = EventsApi and payload = EventCallback (with some json value that is handled
                    // in the bot)
                    //
                    // TODO: talk to the repl to get stuff to then send to the bot
                    tokio::select! {
                        _ = signal.shutdown() => {
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

                        // This is woken up peroidically to force a reconnection
                        _ = disbeat.tick() => {
                            info!("Perodic Reconnect tester");
                            state = ServerState::Disconnect;
                        }
                    }
                },
                ServerState::Disconnect => {
                    let response = r#"{"type": "disconnect", "reason": "refresh_requested"}"#;
                    let _ = ws_write.send(tungstenite::Message::from(response)).await;

                    // Sleep for a second then exit
                    time::sleep(time::Duration::from_secs(1)).await;
                    state = ServerState::Exit;
                },

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
    mut signal: signal::Signal,
    shutdown: watch::Sender<bool>,
) -> AResult<()> {
    while !signal.should_shutdown() {
	    let _ = rl.flush();
        tokio::select! {
            _ = signal.shutdown() => info!("Shutdown signal received"),

			command = rl.readline().fuse() => match command {
				Ok(ReadlineEvent::Line(line)) => {
					let mut line = line.trim().to_string();
                    line.insert_str(0, "!");
					rl.add_history_entry(line.clone());

					match kcore::command::parse(&line) {
                        Ok(Command("send", arg)) => {
                            let _ = writeln!(stdout, "Send: {:?}", arg);
                        },
						_ => {
                            let _ = writeln!(stdout, "Command not found: \"{}\"", line);
                        },
                    }
                },
				Ok(ReadlineEvent::Eof) => {
                    let _ = shutdown.send(true);
                    let _ = writeln!(stdout, "Exiting...");
                },
				Ok(ReadlineEvent::Interrupted) => {
                    let _ = shutdown.send(true);
                    let _ = writeln!(stdout, "^C");
                },
				Err(err) => {
                    let _ = shutdown.send(true);
					let _ = writeln!(stdout, "Received err: {:?}", err);
					let _ = writeln!(stdout, "Exiting...");
				},
            }
        }
    }
    Ok(())

}

#[tokio::main]
async fn main() -> AResult<()> {
    //let postgres_url = env::var("POSTGRES_URL").map_err(|_| anyhow!("POSTGRES_URL env var must be set"))?;

    // Initial terminal setup
    let (rl, stdout) = Readline::new("> ".to_string()).unwrap();

    // Logging
    simplelog::WriteLogger::init(
		log::LevelFilter::Debug,
		simplelog::Config::default(),
		stdout.clone(),
	).unwrap();

    // Block till server is ready
    let (tx, rx) = oneshot::channel();

    //*******************
    // Signals bits
    //*******************
    let (sql_shutdown_tx, signal) = signal::Signal::new();

    let mut signal2 = signal.clone();
    tokio::spawn(async move {
        // If this exits, then shutdown got invoked and this is no longer needed
        signal2.shutdown_daemon().await;
        info!("Shutdown listener exited, shutdown is invoked");
    });

    //*******************
    // Terminal bits
    //*******************
    let signal4 = signal.clone();
    let sql_shutdown_tx3 = sql_shutdown_tx.clone();
    tokio::spawn(async move {
        // If this exits, something went wrong
        if let Err(e) = terminal_readline(rl, stdout, signal4, sql_shutdown_tx3.clone()).await {
            error!("Websocket Error: {:?}", e);
        }
        if let Err(e) = sql_shutdown_tx3.send(true) {error!("Shutdown Signal Error: {:?}", e);}
    });

    //*******************
    // Websocket bits
    //*******************
    // Launch the websocket server
    let signal3 = signal.clone();
    let sql_shutdown_tx2 = sql_shutdown_tx.clone();
    tokio::spawn(async move {
        // If this exits something went wrong
        if let Err(e) = websocket_server(tx, signal3).await {error!("Websocket Error: {:?}", e);}
        if let Err(e) = sql_shutdown_tx2.send(true) {error!("Shutdown Signal Error: {:?}", e);}
    });

    //*******************
    // Slack Client bits
    //*******************
    let slack_fake = FakeSender;
    let slack = slack::Client::with_sender(slack_fake, "http://localhost", "app-token", "bot-token", 10);

    //*******************
    // Postgres bits
    //*******************
    //let (client, connection) = tokio_postgres::connect(&postgres_url, NoTls).await?;
    //let client = Arc::new(client);
    //tokio::spawn(async move {
    //    if let Err(e) = connection.await {error!("Database Error: {:?}", e);}
    //    if let Err(e) = sql_shutdown_tx.send(true) {error!("Shutdown Signal Error: {:?}", e);}
    //});

    //*******************
    // Bot bits
    //*******************
    // Wait till the server and all of the other daemons are ready
    // TODO: have it be a more sophsicated readyness check that makes sure sql, terminal,
    // websockets are all ready to go?
    let _ = rx.await;

    bot::default_event_loop(
        slack.clone(),
        signal,
        |event, tx| {
            //let client2 = client.clone();
            //let slack2 = slack.clone();

            tokio::spawn(async move {
                info!("Event: {:?}", event);
                //if let Err(e) = user_event::process_user_message(
                //    event,
                //    tx,
                //    client2,
                //    slack2,
                //).await {
                //    error!("user_event::process_user_message error: {:?}", e);
                //};
            });
        },
    ).await?;

    Ok(())
}
