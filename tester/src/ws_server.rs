use std::collections::VecDeque;
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
    control: mpsc::UnboundedSender<Control>,
    url: String,
}

impl ServerHandle {
    pub fn url(&self) -> &str {
        &self.url
    }

    pub fn send_event(&self, event: Value) -> AResult<()> {
        self.send(Control::Event(event))
    }

    pub fn net_disconnect(&self, duration: Duration) -> AResult<()> {
        self.send(Control::NetDisconnect(duration))
    }

    pub fn crash_loop(&self) -> AResult<()> {
        self.send(Control::CrashLoop)
    }

    fn send(&self, control: Control) -> AResult<()> {
        self.control
            .send(control)
            .map_err(|_| anyhow!("Fake slack websocket server is not running"))
    }
}

const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(5);
const ACCEPT_BACKOFF: Duration = Duration::from_millis(100);
const REBIND_ATTEMPTS: u32 = 20;
const REBIND_BACKOFF: Duration = Duration::from_millis(250);

pub struct FakeSlackServer {
    listener: TcpListener,
    addr: SocketAddr,
    session: Session,
}

struct Session {
    control: mpsc::UnboundedReceiver<Control>,
    // Events waiting for a connected bot
    pending: VecDeque<Value>,
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
            let mut interval = time::interval(period);
            interval.set_missed_tick_behavior(MissedTickBehavior::Delay);
            interval
        });

        let (tx, rx) = mpsc::unbounded_channel();
        let server = Self {
            listener,
            addr,
            session: Session {
                control: rx,
                pending: VecDeque::new(),
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
            let outcome = match session.accept(&listener, &watcher).await {
                Ok(ws) => {
                    info!("Slack Websocket - server established");
                    session.serve(ws, &watcher).await
                }
                Err(outcome) => outcome,
            };

            match outcome {
                Outcome::Shutdown => break,
                Outcome::Disconnected => (),
                Outcome::NetDown(durt) => {
                    info!("Slack Websocket - server disconnecting for: {durt:?}");
                    // Stop listening entirely so reconnect attempts get refused
                    drop(listener);
                    if !session.idle(durt, &watcher).await {
                        break;
                    }
                    let Some(new) = rebind(addr, &watcher).await else {
                        break;
                    };
                    listener = new;
                }
            }
        }
        info!("Slack Websocket - server exiting");
    }
}

impl Session {
    async fn accept(
        &mut self,
        listener: &TcpListener,
        watcher: &CancellationToken,
    ) -> Result<WebSocketStream<TcpStream>, Outcome> {
        loop {
            let stream = tokio::select! {
                () = watcher.cancelled() => return Err(Outcome::Shutdown),
                Some(control) = self.control.recv() => {
                    if let Some(outcome) = self.idle_control(control) {
                        return Err(outcome);
                    }
                    continue;
                }
                res = listener.accept() => match res {
                    Ok((stream, _)) => stream,
                    Err(e) => {
                        warn!("Slack Websocket - accept failed: {e:?}");
                        time::sleep(ACCEPT_BACKOFF).await;
                        continue;
                    }
                },
            };
            match tokio_tungstenite::accept_async(stream)
                .timeout(HANDSHAKE_TIMEOUT)
                .with_cancellation_token(watcher)
                .await
            {
                None => return Err(Outcome::Shutdown),
                Some(Ok(Ok(ws))) => return Ok(ws),
                Some(Ok(Err(e))) => warn!("Slack Websocket - handshake failed: {e:?}"),
                Some(Err(_)) => warn!("Slack Websocket - handshake timed out"),
            }
        }
    }

    async fn idle(&mut self, durt: Duration, watcher: &CancellationToken) -> bool {
        let sleep = time::sleep(durt);
        tokio::pin!(sleep);
        loop {
            tokio::select! {
                () = watcher.cancelled() => return false,
                () = &mut sleep => return true,
                Some(control) = self.control.recv() => {
                    if let Some(Outcome::NetDown(_)) = self.idle_control(control) {
                        info!("Slack Websocket - already disconnected, ignoring");
                    }
                }
            }
        }
    }

    fn idle_control(&mut self, control: Control) -> Option<Outcome> {
        match control {
            Control::Event(event) => {
                info!(
                    "Bot not connected, queueing event ({} pending)",
                    self.pending.len() + 1
                );
                self.pending.push_back(event);
                None
            }
            Control::NetDisconnect(durt) => Some(Outcome::NetDown(durt)),
            Control::CrashLoop => {
                info!("Forced Crash tester");
                self.crash_loop = true;
                None
            }
        }
    }

    async fn serve(
        &mut self,
        ws: WebSocketStream<TcpStream>,
        watcher: &CancellationToken,
    ) -> Outcome {
        let (mut write, mut read) = ws.split();
        if send(&mut write, json!({"type": "hello", "num_connections": 1}))
            .await
            .is_err()
        {
            return Outcome::Disconnected;
        }

        if self.crash_loop {
            return refresh(&mut write, watcher).await;
        }

        if let Some(refresh) = &mut self.refresh {
            refresh.reset();
        }

        while let Some(event) = self.pending.pop_front() {
            if let Err(event) = self.deliver(&mut write, event).await {
                self.pending.push_front(event);
                return Outcome::Disconnected;
            }
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
                        if send(&mut write, Message::Pong(x)).await.is_err() {
                            return Outcome::Disconnected;
                        }
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
                        if let Err(event) = self.deliver(&mut write, event).await {
                            self.pending.push_back(event);
                            return Outcome::Disconnected;
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

    async fn deliver(&self, write: &mut WsWrite, event: Value) -> Result<(), Value> {
        let envelope = envelope(&event);
        info!("Sending to bot: {envelope}");
        send(write, &envelope).await.map_err(|()| event.clone())?;
        if self.duplicate_events {
            // The original was delivered, so don't requeue on a failed duplicate
            let _ = send(write, &envelope).await;
        }
        Ok(())
    }
}

async fn rebind(addr: SocketAddr, watcher: &CancellationToken) -> Option<TcpListener> {
    for attempt in 1..=REBIND_ATTEMPTS {
        match TcpListener::bind(addr).await {
            Ok(listener) => return Some(listener),
            Err(e) => warn!("Slack Websocket - rebind {addr} attempt {attempt} failed: {e:?}"),
        }
        time::sleep(REBIND_BACKOFF)
            .with_cancellation_token(watcher)
            .await?;
    }
    error!("Slack Websocket - giving up on rebinding {addr}");
    None
}

async fn refresh(write: &mut WsWrite, watcher: &CancellationToken) -> Outcome {
    if send(
        write,
        json!({"type": "disconnect", "reason": "refresh_requested"}),
    )
    .await
    .is_err()
    {
        return Outcome::Disconnected;
    }
    match time::sleep(Duration::from_secs(1))
        .with_cancellation_token(watcher)
        .await
    {
        Some(()) => Outcome::Disconnected,
        None => Outcome::Shutdown,
    }
}

fn envelope(event: &Value) -> Value {
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

async fn send(write: &mut WsWrite, msg: impl Into<WsMessage>) -> Result<(), ()> {
    write.send(msg.into().0).await.map_err(|e| {
        warn!("Slack Websocket - send failed: {e:?}");
    })
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
