use std::fmt::Debug;

use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite::Bytes;
use tokio_tungstenite::tungstenite::Error as TError;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::tungstenite::handshake::client::Response;
use tokio_tungstenite::tungstenite::protocol::frame::CloseFrame;
use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;

use futures_util::SinkExt as _;
use futures_util::StreamExt as _;
use futures_util::sink::Sink;
use futures_util::stream::Stream;

use tokio::sync::broadcast::Sender;
use tokio::time;
use tokio::time::MissedTickBehavior;
use tokio_util::future::FutureExt as _;
use tokio_util::sync::CancellationToken;

use serde_json::json;

use log::{debug, error, info, warn};

use tokio::time::Duration;

use anyhow::Result as AResult;
use anyhow::anyhow;

use crate::SlackClient;
use crate::SlackSender;
use crate::connection_state::ConnectionState;
use crate::event::Envelope;
use crate::event::process_control_message;

use tokio::net::TcpStream;
use tokio_tungstenite::MaybeTlsStream;
use tokio_tungstenite::WebSocketStream;

pub trait WsClient {
    type WsStream: Stream<Item = Result<Message, TError>>
        + Sink<Message, Error: Debug>
        + Unpin
        + Send;

    fn connect(
        &self,
        url: &str,
    ) -> impl Future<Output = Result<(Self::WsStream, Response), TError>> + Send;
    fn close(
        &self,
        stream: &mut Self::WsStream,
        frame: Option<CloseFrame>,
    ) -> impl Future<Output = Result<(), TError>> + Send;
}

pub struct TungsteniteClient;
impl WsClient for TungsteniteClient {
    type WsStream = WebSocketStream<MaybeTlsStream<TcpStream>>;

    fn connect(
        &self,
        url: &str,
    ) -> impl Future<Output = Result<(Self::WsStream, Response), TError>> + Send {
        connect_async(url)
    }

    fn close(
        &self,
        stream: &mut Self::WsStream,
        frame: Option<CloseFrame>,
    ) -> impl Future<Output = Result<(), TError>> + Send {
        stream.close(frame)
    }
}

pub async fn slack_websocket_handler<S: SlackSender, W: WsClient>(
    slack: SlackClient<S>,
    client: W,
    watcher: CancellationToken,
    user_event: Sender<Envelope>,
) {
    let conn_state = ConnectionState::init();

    // Interval timers for heartbeat
    let mut heartbeat = time::interval(Duration::from_secs(1));
    heartbeat.set_missed_tick_behavior(MissedTickBehavior::Delay);

    // Main server loop, exit if the database dies
    while !watcher.is_cancelled() {
        match ws_connect(&slack, &client, &watcher).await {
            Err(e) => error!("Slack Websocket - Connection failed: {e:?}"),
            Ok(mut websocket) => {
                // Reset the connection to waiting for ack from Slack
                conn_state.pending();

                while !conn_state.should_reconnect() && !watcher.is_cancelled() {
                    tokio::select! {
                        // Listens for shutdown signals from the system or the database
                        () = watcher.cancelled() => info!("Shutdown signal received"),

                        // Process inbound messages
                        ws_msg = websocket.next() => process_message(&mut websocket, &conn_state, &user_event, ws_msg).await,

                        // This is woken up peroidically to force a heartbeat check
                        _ = heartbeat.tick() => heartbeat_tick(&mut websocket, &conn_state).await,
                    }
                }

                // We are exiting or reconnecting, close the tungstein stream
                // TODO: skip sending close if the underlaying stream is in a bad state
                //  - check the status/error from attempting to transmit
                if let Err(e) = client
                    .close(
                        &mut websocket,
                        Some(CloseFrame {
                            code: CloseCode::Normal,
                            reason: "Reconnecting".into(),
                        }),
                    )
                    .timeout(Duration::from_secs(5))
                    .await
                {
                    warn!("Failed to close the websocket {e:?}");
                }
            }
        }

        if !watcher.is_cancelled() {
            // Backoff timer is implemented in conn_state, its reset on entering reconnect state
            // - When it returns None we have ran out of attempts and we should give up now
            // - This is also exceeded in a crash loop cuz we will not have an alive-connection and
            // max backoff timer expires
            if let Some(durt) = conn_state.fetch_next_backoff() {
                info!("Slack Websocket - Reconnecting, sleeping for: {durt:?}");
                time::sleep(durt).with_cancellation_token(&watcher).await;
            } else {
                error!("Slack Websocket - Exceeded with_max_elapsed_time, shutting down");
                watcher.cancel();
                break;
            }
        }
    }
    info!("Slack Websocket - Exiting");
}

async fn ws_connect<S: SlackSender, W: WsClient>(
    slack: &SlackClient<S>,
    client: &W,
    watcher: &CancellationToken,
) -> AResult<W::WsStream> {
    let ws_url = slack
        .socket_connect()
        .with_cancellation_token(watcher)
        .await
        .ok_or(anyhow!("Shutdown triggered"))??;
    let (ws_stream, _) = client
        .connect(&ws_url)
        .timeout(Duration::from_secs(5))
        .with_cancellation_token(watcher)
        .await
        .ok_or(anyhow!("Shutdown triggered"))???;
    info!("Slack Websocket - connection established");
    Ok(ws_stream)
}

async fn process_message<W>(
    websocket: &mut W,
    connection_state: &ConnectionState,
    user_event: &Sender<Envelope>,
    ws_msg: Option<Result<Message, TError>>,
) where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    if connection_state.should_reconnect() {
        return;
    }
    match ws_msg {
        Some(Ok(ws_msg)) => {
            match process_control_message::<W>(websocket, connection_state, ws_msg).await {
                Ok(Some(event)) => {
                    if let Err(e) = user_event.send(event) {
                        // A send fail doesn't mean it won't be picked up again.
                        error!("user_event.send error: {e:?}");
                    }
                }
                Err(e) => error!("process_control_message error: {e:?}"),
                _ => (),
            }
        }
        Some(Err(e)) => {
            error!("Slack Websocket - Process - Connection error: {e:?}");
            connection_state.reconnect();
        }
        None => {
            // Stream is done/closed
            warn!("Slack Websocket - Process - Stream closed, reconnecting");
            connection_state.reconnect();
        }
    }
}

async fn heartbeat_tick<W>(websocket: &mut W, connection_state: &ConnectionState)
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    if connection_state.should_reconnect() {
        return;
    }
    // Check the delta, If either or both are None, that means
    // their timer are *ahead* of the 'now' which is fine, stop checking.
    if let (Some(last_received), Some(last_ping)) = connection_state.timer_delta() {
        // Check if more than 30s has past since the last slack message received
        if last_received.as_secs() > 30 {
            // Check if more than 2m has past since the last slack message received
            if last_received.as_secs() > 120 {
                // Reconnect
                info!(
                    "Slack Websocket Heartbeat - Last message received: {:?}s, reconnecting",
                    last_received.as_secs()
                );
                connection_state.reconnect();

            // Check if more than 30s has past since the send of the last ping
            } else if last_ping.as_secs() > 30 {
                // Send Ping
                debug!(
                    "Slack Websocket Heartbeat - Last message received: {:?}s, Last ping sent: {:?}s",
                    last_received.as_secs(),
                    last_ping.as_secs(),
                );
                if connection_state.should_send() {
                    // TODO: Generate a new value each time so we can track the ping-pong for
                    // connection aliveness
                    if let Err(e) = send_ping::<W>(connection_state, websocket, Bytes::new()).await
                    {
                        error!("send_ping error: {e:?}");
                    }
                }
            }
        }
    }
}

pub(crate) async fn send_acknowledge<W>(
    connection_state: &ConnectionState,
    websocket: &mut W,
    envelope_id: &str,
) -> AResult<()>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    send_websocket(
        connection_state,
        websocket,
        Message::from(
            json!({
                "envelope_id": envelope_id
            })
            .to_string(),
        ),
    )
    .await
}

async fn send_ping<W>(
    connection_state: &ConnectionState,
    websocket: &mut W,
    content: Bytes,
) -> AResult<()>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    match send_websocket(connection_state, websocket, Message::Ping(content)).await {
        Ok(()) => {
            connection_state.ping_sent();
            Ok(())
        }
        e => e,
    }
}

pub(crate) async fn send_pong<W>(
    connection_state: &ConnectionState,
    websocket: &mut W,
    content: Bytes,
) -> AResult<()>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    send_websocket(connection_state, websocket, Message::Pong(content)).await
}

async fn send_websocket<W>(
    connection_state: &ConnectionState,
    websocket: &mut W,
    message: Message,
) -> AResult<()>
where
    W: Sink<Message> + Unpin,
    <W as Sink<Message>>::Error: Debug,
{
    if connection_state.should_send() {
        if let Err(e) = websocket.send(message).await {
            connection_state.reconnect();
            Err(anyhow!("Slack Websocket - Connection error: {e:?}"))
        } else {
            Ok(())
        }
    } else {
        Err(anyhow!(
            "Bad state! Can't send a control message yet received slack message"
        ))
    }
}
