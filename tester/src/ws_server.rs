use std::future;
use std::net::SocketAddr;
use std::time::Duration;

use anyhow::Result as AResult;
use anyhow::{Context as _, anyhow};

use futures_util::SinkExt as _;
use futures_util::StreamExt as _;
use futures_util::stream::SplitSink;

use log::{debug, error, info, warn};

use random_string::charsets;
use random_string::generate;

use serde_json::{Value, json};

use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc;
use tokio::time::{self, Interval, MissedTickBehavior};
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_util::future::FutureExt as _;
use tokio_util::sync::CancellationToken;

type WsWrite = SplitSink<WebSocketStream<TcpStream>, Message>;

#[derive(Debug, Clone)]
pub struct ServerConfig {
    // Periodically ask the bot to reconnect
    pub refresh_interval: Option<Duration>,
    // Send every event twice to force test the dedup event loop
    pub duplicate_events: bool,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            refresh_interval: Some(Duration::from_secs(240)),
            duplicate_events: true,
        }
    }
}

#[derive(Debug)]
enum Control {
    Event(Value),
    NetDisconnect(Duration),
    CrashLoop,
}

#[derive(Clone)]
pub struct ServerHandle {
    control: mpsc::Sender<Control>,
    url: String,
}

impl ServerHandle {
    pub fn url(&self) -> &str {
        &self.url
    }

    pub async fn send_event(&self, event: Value) -> AResult<()> {
        self.send(Control::Event(event)).await
    }

    pub async fn net_disconnect(&self, duration: Duration) -> AResult<()> {
        self.send(Control::NetDisconnect(duration)).await
    }

    pub async fn crash_loop(&self) -> AResult<()> {
        self.send(Control::CrashLoop).await
    }

    async fn send(&self, control: Control) -> AResult<()> {
        self.control
            .send(control)
            .await
            .map_err(|_| anyhow!("Fake slack websocket server is not running"))
    }
}

pub struct FakeSlackServer {
    listener: TcpListener,
    addr: SocketAddr,
    session: Session,
}

struct Session {
    control: mpsc::Receiver<Control>,
    refresh: Option<Interval>,
    duplicate_events: bool,
    crash_loop: bool,
}

enum Outcome {
    Shutdown,
    Disconnected,
    NetDown(Duration),
}

impl FakeSlackServer {
    // Bind is done up front so the server is ready to accept as soon as this returns.
    // Use port 0 (ie `127.0.0.1:0`) to pick a free port, then check `ServerHandle::url`.
    pub async fn bind(addr: &str, config: ServerConfig) -> AResult<(Self, ServerHandle)> {
        let listener = TcpListener::bind(addr)
            .await
            .with_context(|| format!("Failed to bind fake slack websocket server to {addr}"))?;
        let addr = listener.local_addr()?;

        let refresh = config.refresh_interval.map(|period| {
            let mut interval = time::interval_at(time::Instant::now() + period, period);
            interval.set_missed_tick_behavior(MissedTickBehavior::Delay);
            interval
        });

        let (tx, rx) = mpsc::channel(10);
        let server = Self {
            listener,
            addr,
            session: Session {
                control: rx,
                refresh,
                duplicate_events: config.duplicate_events,
                crash_loop: false,
            },
        };
        let handle = ServerHandle {
            control: tx,
            url: format!("ws://{addr}"),
        };
        Ok((server, handle))
    }

    pub async fn run(self, watcher: CancellationToken) {
        let Self {
            mut listener,
            addr,
            mut session,
        } = self;
        info!("Slack Websocket - server listening on ws://{addr}");

        loop {
            let stream = tokio::select! {
                () = watcher.cancelled() => break,
                res = listener.accept() => match res {
                    Ok((stream, _)) => stream,
                    Err(e) => {
                        warn!("Slack Websocket - accept failed: {e:?}");
                        continue;
                    }
                },
            };
            let ws = match tokio_tungstenite::accept_async(stream).await {
                Ok(ws) => ws,
                Err(e) => {
                    warn!("Slack Websocket - handshake failed: {e:?}");
                    continue;
                }
            };
            info!("Slack Websocket - server established");

            match session.serve(ws, &watcher).await {
                Outcome::Shutdown => break,
                Outcome::Disconnected => (),
                Outcome::NetDown(durt) => {
                    info!("Slack Websocket - server disconnecting for: {durt:?}");
                    // Stop listening entirely so reconnect attempts get refused
                    drop(listener);
                    if time::sleep(durt)
                        .with_cancellation_token(&watcher)
                        .await
                        .is_none()
                    {
                        break;
                    }
                    listener = match TcpListener::bind(addr).await {
                        Ok(listener) => listener,
                        Err(e) => {
                            error!("Slack Websocket - failed to rebind {addr}: {e:?}");
                            break;
                        }
                    };
                }
            }
        }
        info!("Slack Websocket - server exiting");
    }
}

impl Session {
    async fn serve(
        &mut self,
        ws: WebSocketStream<TcpStream>,
        watcher: &CancellationToken,
    ) -> Outcome {
        let (mut write, mut read) = ws.split();
        send(&mut write, json!({"type": "hello", "num_connections": 1})).await;

        if self.crash_loop {
            return refresh(&mut write, watcher).await;
        }

        loop {
            tokio::select! {
                () = watcher.cancelled() => return Outcome::Shutdown,

                ws_msg = read.next() => match ws_msg {
                    // TODO: validate acknowledgements ({"envelope_id": ...}) and resend
                    // events that weren't acked after a delay
                    Some(Ok(Message::Text(msg))) => info!("Client message: {msg:?}"),
                    Some(Ok(Message::Ping(x))) => {
                        debug!("Client Ping: {x:?}");
                        send(&mut write, Message::Pong(x)).await;
                    }
                    Some(Ok(Message::Pong(x))) => debug!("Client Pong: {x:?}"),
                    Some(Ok(Message::Close(reason))) => {
                        info!("Client close reason: {reason:?}");
                        return Outcome::Disconnected;
                    }
                    Some(Ok(ws_msg)) => info!("Server Unknown Event: {ws_msg:?}"),
                    Some(Err(e)) => {
                        error!("Server Event - Error: {e:?}");
                        return Outcome::Disconnected;
                    }
                    None => {
                        warn!("Server Event - Disconnection");
                        return Outcome::Disconnected;
                    }
                },

                Some(control) = self.control.recv() => match control {
                    Control::Event(event) => {
                        let envelope = envelope(event);
                        info!("Sending to bot: {envelope}");
                        send(&mut write, &envelope).await;
                        if self.duplicate_events {
                            send(&mut write, &envelope).await;
                        }
                    }
                    Control::NetDisconnect(durt) => return Outcome::NetDown(durt),
                    Control::CrashLoop => {
                        info!("Forced Crash tester");
                        self.crash_loop = true;
                        return refresh(&mut write, watcher).await;
                    }
                },

                () = tick(&mut self.refresh) => {
                    info!("Periodic Reconnect tester");
                    return refresh(&mut write, watcher).await;
                }
            }
        }
    }
}

async fn refresh(write: &mut WsWrite, watcher: &CancellationToken) -> Outcome {
    send(
        write,
        json!({"type": "disconnect", "reason": "refresh_requested"}),
    )
    .await;
    match time::sleep(Duration::from_secs(1))
        .with_cancellation_token(watcher)
        .await
    {
        Some(()) => Outcome::Disconnected,
        None => Outcome::Shutdown,
    }
}

fn envelope(event: Value) -> Value {
    json!({
        "type": "events_api",
        "envelope_id": generate(12, charsets::ALPHANUMERIC),
        "payload": {
            "type": "event_callback",
            "event_id": generate(12, charsets::ALPHANUMERIC),
            "event": event,
        },
        "accepts_response_payload": false,
    })
}

async fn send(write: &mut WsWrite, msg: impl Into<WsMessage>) {
    if let Err(e) = write.send(msg.into().0).await {
        warn!("Slack Websocket - send failed: {e:?}");
    }
}

struct WsMessage(Message);

impl From<Message> for WsMessage {
    fn from(msg: Message) -> Self {
        Self(msg)
    }
}

impl From<Value> for WsMessage {
    fn from(value: Value) -> Self {
        Self(Message::from(value.to_string()))
    }
}

impl From<&Value> for WsMessage {
    fn from(value: &Value) -> Self {
        Self(Message::from(value.to_string()))
    }
}

async fn tick(interval: &mut Option<Interval>) {
    match interval {
        Some(interval) => {
            interval.tick().await;
        }
        None => future::pending().await,
    }
}
