use std::env;
use std::sync::Arc;

use log::{info, warn, error};

use anyhow::anyhow;
use anyhow::Result as AResult;

use tokio::net::TcpListener;
use tokio_postgres::NoTls;
use tokio::sync::oneshot;

use futures_util::StreamExt;

use http::response;

use kcore::slack;
use kcore::slack::HttpSender;
use kcore::signal;
use kcore::bot;
use karmator::bot::user_event;


// Fake HTTP server for dealing with slack http api calls
#[derive(Clone)]
struct FakeSender;

impl HttpSender for FakeSender {
    // TODO: build a set of data and/or hook up to a prompt input
    async fn send(&self, _request: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
        let response = r#"{
            "ok": true,
            "url": "ws://127.0.0.1:8080"
        }"#;
        Ok(response::Builder::new().status(200).body(response)?.into())
    }
}


// Fake Websocket server for sending the bot "messages" from slack
async fn websocket_server(tx: oneshot::Sender<()>) -> AResult<()> {
    // Socket + event loop for listening to connection attempts
    let socket = TcpListener::bind("127.0.0.1:8080").await.unwrap();
    let _ = tx.send(());

    // For now only support one connection
    while let Ok((stream, addr)) = socket.accept().await {
        let ws_stream = tokio_tungstenite::accept_async(stream).await.unwrap();
        let (mut ws_write, mut ws_read) = ws_stream.split();

        info!("Slack Websocket - server established");

        // TODO: talk to the repl to get stuff to then send to the bot
        // TODO: deal with shutdowning
        // TODO: do some sort of heartbeat to keep the client happy
        // TODO: do some sort of routine reconnection to keep the reconnect code tested
        tokio::select! {
            ws_msg = ws_read.next() => {
                match ws_msg {
                    Some(Ok(ws_msg)) => {
                        info!("Server Event: {:?}", ws_msg);
                    },
                    Some(Err(e)) => error!("Server Event - Error: {:?}", e),
                    None => warn!("Server Event - Disconnection"),
                }
            },
        }
    }
    Ok(())
}


#[tokio::main]
async fn main() -> AResult<()> {
    // TODO: integrate logger into REPL and remove env_logger
    env_logger::init();

    // Do postgres bits later
    //let postgres_url = env::var("POSTGRES_URL").map_err(|_| anyhow!("POSTGRES_URL env var must be set"))?;

    // Shutdown Signal
    let (sql_shutdown_tx, signal) = signal::Signal::new();

    // Block till server is ready
    let (tx, rx) = oneshot::channel();

    // Launch the websocket
    tokio::spawn(async move {
        // If this exits something went wrong
        // TODO: wire it up to repl
        if let Err(e) = websocket_server(tx).await {error!("Websocket Error: {:?}", e);}
    });
    let _ = rx.await;

    // System slack client manager
    let slack_fake = FakeSender;
    let slack = slack::Client::with_sender(slack_fake, "http://localhost", "app-token", "bot-token", 10);

    //let (client, connection) = tokio_postgres::connect(&postgres_url, NoTls).await?;
    //let client = Arc::new(client);
    //tokio::spawn(async move {
    //    if let Err(e) = connection.await {error!("Database Error: {:?}", e);}
    //    if let Err(e) = sql_shutdown_tx.send(true) {error!("Shutdown Signal Error: {:?}", e);}
    //});

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
