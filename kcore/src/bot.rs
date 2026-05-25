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

#[cfg(test)]
mod test_heartbeat {
    use super::*;

    #[tokio::test(start_paused = true)]
    async fn test_heartbeat_reconnect_gate() {
        let state = ConnectionState::init();
        state.reconnect();

        let mut ws_sink = vec![];
        heartbeat_tick(
            &mut ws_sink,
            &state,
        ).await;

        // Assert that the connection state is same as before
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        // We should have not been able to send ping cuz we are reconnect
        assert!(ws_sink.is_empty());

        // Timing check
        let (lmr, lps) = state.timer_delta();
        assert_eq!(lmr, Some(Duration::from_secs(0)));
        assert_eq!(lps, Some(Duration::from_secs(0)));
    }

    #[tokio::test(start_paused = true)]
    async fn test_heartbeat_pending_send_ping() {
        let state = ConnectionState::init();
        state.pending();

        // Message was received + ping sent
        state.message_received();
        state.ping_sent();

        time::advance(Duration::from_mins(1)).await;

        let mut ws_sink = vec![];
        heartbeat_tick(
            &mut ws_sink,
            &state,
        ).await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(!state.should_send());

        // It has been 1 minute since last message + ping it should have sent 1 ping, however we
        // are still pending so it would have not be permitted to.
        assert!(ws_sink.is_empty());

        // Timing check
        let (lmr, lps) = state.timer_delta();
        assert_eq!(lmr, Some(Duration::from_mins(1)));
        assert_eq!(lps, Some(Duration::from_mins(1)));
    }

    #[tokio::test(start_paused = true)]
    async fn test_heartbeat_too_early_send_ping() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        // Message was received + ping sent
        state.message_received();
        state.ping_sent();

        time::advance(Duration::from_secs(15)).await;

        let mut ws_sink = vec![];
        heartbeat_tick(
            &mut ws_sink,
            &state,
        ).await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        // It has been 15s, its not yet time to send a ping.
        assert!(ws_sink.is_empty());

        // Timing check
        let (lmr, lps) = state.timer_delta();
        assert_eq!(lmr, Some(Duration::from_secs(15)));
        assert_eq!(lps, Some(Duration::from_secs(15)));
    }

    #[tokio::test(start_paused = true)]
    async fn test_heartbeat_send_ping() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        // Message was received + ping sent
        state.message_received();
        state.ping_sent();

        time::advance(Duration::from_mins(1)).await;

        let mut ws_sink = vec![];
        heartbeat_tick(
            &mut ws_sink,
            &state,
        ).await;

        // Assert that the connection state is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        // It has been 1 minute since last message + ping it should have sent 1 ping.
        assert_eq!(ws_sink, vec![Message::Ping(Bytes::new())]);

        // Timing check
        let (lmr, lps) = state.timer_delta();
        assert_eq!(lmr, Some(Duration::from_mins(1)));
        assert_eq!(lps, Some(Duration::from_secs(0)));
    }

    #[tokio::test(start_paused = true)]
    async fn test_heartbeat_reconnect_send_ping() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        // Message was received + ping sent
        state.message_received();

        time::advance(Duration::from_mins(2)).await;

        state.ping_sent();

        time::advance(Duration::from_mins(1)).await;

        let mut ws_sink = vec![];
        heartbeat_tick(
            &mut ws_sink,
            &state,
        ).await;

        // Assert we are now reconnecting
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        // It has been 3 minutes since last message, 1 minute since last ping (shouldn't matter).
        // We should have not sent a ping to slack since we are now in reconnect mode.
        assert!(ws_sink.is_empty());

        // Timing check
        let (lmr, lps) = state.timer_delta();
        assert_eq!(lmr, Some(Duration::from_mins(3)));
        assert_eq!(lps, Some(Duration::from_mins(1)));
    }
}

#[cfg(test)]
mod test_process_message {
    use super::*;
    use tokio::sync::broadcast::channel;

    #[tokio::test(start_paused = true)]
    async fn test_process_reconnect_gap() {
        let data = r#"{
            "type": "hello",
            "num_connections": 1
        }"#;

        let state = ConnectionState::init();
        state.reconnect();

        let mut ws_sink = vec![];
        let (sue, rue) = channel(32);

        process_message(
            &mut ws_sink,
            &state,
            &sue,
            Some(Ok(Message::from(data))),
        ).await;

        // Assert that the connection state is same as before
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        // We should have not been able process anything
        assert!(ws_sink.is_empty());
        assert!(rue.is_empty());
    }

    #[tokio::test(start_paused = true)]
    async fn test_process_hello() {
        let data = r#"{
            "type": "hello",
            "num_connections": 1
        }"#;

        let state = ConnectionState::init();
        state.pending();

        let mut ws_sink = vec![];
        let (sue, rue) = channel(32);

        process_message(
            &mut ws_sink,
            &state,
            &sue,
            Some(Ok(Message::from(data))),
        ).await;

        // Assert that the connection is now send.
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        // We should have not gotten anything, its a control message
        assert!(ws_sink.is_empty());
        assert!(rue.is_empty());
    }

    #[tokio::test(start_paused = true)]
    async fn test_process_actual_data() {
        let data = json!({
            "envelope_id": "id",
            "type": "typ",
            "payload": {
                "test": "asdf"
            }
        })
        .to_string();

        let envelope = Envelope {
            id: "id".to_owned(),
            typ: "typ".to_owned(),
            payload: json!({"test": "asdf"}),
        };

        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let (sue, mut rue) = channel(32);

        process_message(
            &mut ws_sink,
            &state,
            &sue,
            Some(Ok(Message::from(data))),
        ).await;

        // Assert that the connection is same as before
        assert!(!state.should_reconnect());
        assert!(state.should_send());

        // We should have gotten an ack + the event itself
        assert_eq!(ws_sink, vec![Message::from(json!({"envelope_id": "id"}).to_string())]);
        assert_eq!(rue.recv().await.unwrap(), envelope);
        assert!(rue.is_empty());
    }

    #[tokio::test(start_paused = true)]
    async fn test_process_closed() {
        let state = ConnectionState::init();
        state.pending();
        state.connected();

        let mut ws_sink = vec![];
        let (sue, rue) = channel(32);

        process_message(
            &mut ws_sink,
            &state,
            &sue,
            None,
        ).await;

        // Assert that we are reconnecting now
        assert!(state.should_reconnect());
        assert!(!state.should_send());

        // We should have gotten nothing
        assert!(ws_sink.is_empty());
        assert!(rue.is_empty());
    }
}

#[cfg(test)]
mod test_lifecycle_slack_handler {
    use super::*;
    use std::pin::Pin;
    use std::task::Context;
    use std::task::Poll;
    use std::io::Error as IError;
    use std::io::ErrorKind;
    use std::collections::VecDeque;
    use tokio::sync::mpsc;
    use tokio::sync::broadcast;
    use parking_lot::Mutex;

    //***************************
    // Fake WebSocket
    //***************************
    struct FakeWebSocket {
        inbound: mpsc::UnboundedReceiver<Result<Message, TError>>,
        outbound: mpsc::UnboundedSender<Message>,
    }

    impl Stream for FakeWebSocket {
        type Item = Result<Message, TError>;

        fn poll_next(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            self.inbound.poll_recv(ctx)
        }
    }

    impl Sink<Message> for FakeWebSocket {
        type Error = TError;

        fn poll_ready(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }

        fn start_send(self: Pin<&mut Self>, item: Message) -> Result<(), Self::Error> {
            self.outbound.send(item).map_err(|e| TError::Io(IError::new(ErrorKind::Other, e)))
        }

        fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }

        fn poll_close(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }
    }

    //***************************
    // Fake WebSocket Client
    //***************************
    struct FakeWebSocketClient {
        // This allows simulating several connection
        session: Mutex<VecDeque<FakeWebSocket>>
    }

    impl FakeWebSocketClient {
        fn new(session: Vec<(mpsc::UnboundedReceiver<Result<Message, TError>>, mpsc::UnboundedSender<Message>)>) -> Self {
            Self {
                session: Mutex::new(
                    VecDeque::from(session
                        .into_iter()
                        .map(|(inbound, outbound)| FakeWebSocket { inbound, outbound })
                        .collect::<Vec<_>>()
                    ),
                ),
            }
        }
    }

    impl WsClient for FakeWebSocketClient {
        type WsStream = FakeWebSocket;

        async fn connect(&self, _: &str) -> Result<(Self::WsStream, Response), TError> {
            let websocket = self.session.lock().pop_front().expect("No more connection");
            let resp = Response::default();
            Ok((websocket, resp))
        }

        async fn close(&self, _: &mut Self::WsStream, _: Option<CloseFrame>) -> Result<(), TError> {
            Ok(())
        }
    }

    //***************************
    // Fake Slack Client
    //***************************
    #[derive(Clone)]
    struct FakeSender;

    impl SlackSender for FakeSender {
        async fn send(&self, _: reqwest::RequestBuilder) -> AResult<reqwest::Response> {
            let body = r#"{"ok": true, "url": "wss://whatev"}"#;
            Ok(http::response::Builder::new().status(200).body(body)?.into())
        }
    }

    //***************************
    // Actual tests below
    //***************************
    #[tokio::test(start_paused = true)]
    async fn test_initial_hello_and_envelope() {
        let watcher = CancellationToken::new();
        let slack = SlackClient::with_sender(FakeSender, "http://localhost/api", "app", "bot", 10);

        let (input, mut output, ws_client) = {
            let (in_tx, in_rx) = mpsc::unbounded_channel();
            let (out_tx, out_rx) = mpsc::unbounded_channel();
            let ws_client = FakeWebSocketClient::new(vec![(in_rx, out_tx)]);

            (in_tx, out_rx, ws_client)
        };

        let (event_tx, mut event_rx) = broadcast::channel::<Envelope>(32);

        // TODO: spawn the slack_websocket_handler
        //  - Connect, receive Hello, receive an Envelope, verify ack frame is sent and
        //  event is broadcast.
    }

    //  - Connect, receive Hello, receive an Envelope, verify ack frame is sent and
    //  event is broadcast.
    //  - Connect, receive nothing for 31s (virtual), verify Ping is emitted.
    //  - Connect, receive nothing for 121s (virtual), verify should_reconnect() is
    //  set.
    //  - Connect, peer sends Close(Normal), verify our inner loop exits and close()
    //  is sent back.
    //  - Connect, peer sends Disconnect{reason: "refresh_requested"}, verify
    //  reconnect path runs.
}
