use slack_api as slack;
use tokio_tungstenite as tungstenite;

use futures_util::{SinkExt, StreamExt};

use tokio::runtime::Runtime;
use tokio::sync::mpsc;

// Alternative if need tokio 0.3
// use tokio_compat_02::FutureExt;

use std::sync::{Arc, Mutex};
use std::default::Default;
use std::env;

// Type alias for msg_id
type MsgId = Arc<Mutex<u64>>;

fn main() {
    Runtime::new()
        .expect("Failed to create Tokio runtime")
        .block_on(async_main());
}

async fn async_main() -> Result<(), Box<dyn std::error::Error>> {
    // Shared integer counter for message ids
    let msg_id = Arc::new(Mutex::new(0u64));

    let token = env::var("SLACK_API_TOKEN").map_err(|_| "SLACK_API_TOKEN env var must be set")?;
    let client = slack::default_client().map_err(|e| format!("Could not get default_client, {:?}", e))?;

    {
        // Post a message
        let msg = slack::chat::PostMessageRequest {
            channel: "karmator-devs",
            text: "Pogger, i'm starting up!",
            as_user: Some(true),
            ..Default::default()
        };
        slack::chat::post_message(&client, &token, &msg).await;


        // We use .compat() from FutureExt specifically because the api client is on reqwest which
        // uses tokio 0.2 and we are going to be using tokio 0.3 (for tungstein for websockets)
        //
        // One possibility could be to:
        // You can provide your own client by implementing the async or sync versions of SlackWebRequestSender.
        //
        // Which would let me get off requwest onto something that is on tokio 0.3 (this may have
        // merit)
        //let request = slack::rtm::StartRequest::default();
        //let response = slack::rtm::start(&client, &token, &request).compat().await;
        let response = slack::rtm::connect(&client, &token).await;

        if let Ok(response) = response {
            println!("Got a response lets start the websocket");

            if let Some(ws_url) = response.url {
                println!("Got WS url: {:?}", ws_url);

                let ws_response = tungstenite::connect_async(ws_url).await;

                if let Ok((mut ws_stream, ws_response)) = ws_response {
                    println!("Got a connection established");

                    // Setup the mpsc channel
                    let (mut tx, mut rx) = mpsc::channel(32);

                    // Split the stream
                    let (mut ws_write, mut ws_read) = ws_stream.split();

                    // Spawn the inbound ws stream processor
                    let inbound = tokio::spawn(async move {
                        loop {
                            if let Some(Ok(ws_msg)) = ws_read.next().await {
                                let msg_id = msg_id.clone();
                                let tx2 = tx.clone();

                                tokio::spawn(async move {
                                    process_inbound_message(msg_id, ws_msg, tx2).await;
                                });

                            }
                        }
                    });

                    // Spawn outbound ws processor
                    // TODO: need to make sure to wait on sending till we recieve the hello event
                    let outbound = tokio::spawn(async move {
                        loop {
                            while let Some(message) = rx.recv().await {
                                println!("{:?}", message);

                                ws_write.send(message).await;
                            }
                        }
                    });

                    // Wait till either exits (error) then begin recovery
                    let res = tokio::try_join!(inbound, outbound);

                    match res {
                        Ok((first, second)) => println!("Both exited fine"),
                        Err(err) => println!("Something failed: {:?}", err),
                    }
                } else {
                    println!("{:?}", ws_response.err());
                }
            } else {
                println!("No WS url....");
            }
        } else {
            println!("{:?}", response);
        }
    }
    Ok(())
}


async fn process_inbound_message(
    msg_id: MsgId,
    msg: tungstenite::tungstenite::Message,
    mut tx: mpsc::Sender<tungstenite::tungstenite::Message>
) -> Result<(), Box<dyn std::error::Error>>
{
    let id = {
        // TODO: for now just forever inc the msg id, later only inc if want to send a reply
        let mut data = msg_id.lock().unwrap();
        *data += 1;
        *data
    };

    println!("id = {:?}", id);
    println!("{:?}", msg);

    if id % 4 == 0 {
        // Trial send something
        let ws_msg = tungstenite::tungstenite::Message::from(
            "{\"id\":1,\"type\":\"message\",\"channel\":\"CAF6S4TRT\",\"text\":\"mod4 kappa\"}".to_string()
        );

        tx.send(ws_msg).await;

        println!("Kappa was sent");
    }

    Ok(())
}
