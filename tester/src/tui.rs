use std::collections::VecDeque;
use std::time::Duration;

use anyhow::Result as AResult;
use chrono::{DateTime, Local};
use crossterm::event::{Event, EventStream, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use futures_util::StreamExt as _;
use log::Level;
use ratatui::DefaultTerminal;
use ratatui::Frame;
use ratatui::layout::{Constraint, Layout, Rect};
use ratatui::style::{Color, Style, Stylize as _};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};
use tokio::sync::{broadcast, mpsc, oneshot};
use tokio::time;
use tokio_util::sync::CancellationToken;
use tui_input::Input;
use tui_input::backend::crossterm::EventHandler as _;

use karmator_tester::command::{self, Command, Context};
use karmator_tester::logging::{LogControl, LogLine};
use karmator_tester::{Activity, Connection, HarnessHandle, Status};

const HISTORY: usize = 1000;
const TICK: Duration = Duration::from_secs(1);

struct Pane<T> {
    lines: VecDeque<T>,
}

impl<T> Pane<T> {
    const fn new() -> Self {
        Self {
            lines: VecDeque::new(),
        }
    }

    fn push(&mut self, line: T) {
        if self.lines.len() == HISTORY {
            self.lines.pop_front();
        }
        self.lines.push_back(line);
    }
}

enum Chat {
    You(String),
    Reacji { added: bool, name: String },
    Bot(String),
    Sys(String),
}

struct Stamped<T> {
    time: DateTime<Local>,
    item: T,
}

impl<T> Stamped<T> {
    fn now(item: T) -> Self {
        Self {
            time: Local::now(),
            item,
        }
    }
}

struct App {
    logs: Pane<LogLine>,
    wire: Pane<Stamped<Activity>>,
    chat: Pane<Stamped<Chat>>,
    input: Input,
    history: Vec<String>,
    history_pos: Option<usize>,
    show_wire: bool,
    status: Option<Status>,
    log: LogControl,
}

impl App {
    fn new(log: LogControl) -> Self {
        Self {
            logs: Pane::new(),
            wire: Pane::new(),
            chat: Pane::new(),
            input: Input::default(),
            history: Vec::new(),
            history_pos: None,
            show_wire: false,
            status: None,
            log,
        }
    }

    fn sys(&mut self, text: impl Into<String>) {
        self.chat.push(Stamped::now(Chat::Sys(text.into())));
    }

    fn activity(&mut self, activity: Activity) {
        match &activity {
            Activity::EventSent { event, .. } => {
                if let Some(chat) = chat_for_event(event) {
                    self.chat.push(Stamped::now(chat));
                }
            }
            Activity::BotReply { text, .. } => self.chat.push(Stamped::now(Chat::Bot(text.clone()))),
            _ => (),
        }
        self.wire.push(Stamped::now(activity));
    }

    fn history_step(&mut self, back: bool) {
        if self.history.is_empty() {
            return;
        }
        let last = self.history.len() - 1;
        self.history_pos = match (self.history_pos, back) {
            (None, true) => Some(last),
            (None, false) => None,
            (Some(i), true) => Some(i.saturating_sub(1)),
            (Some(i), false) if i < last => Some(i + 1),
            (Some(_), false) => None,
        };
        let value = self
            .history_pos
            .map_or_else(String::new, |i| self.history[i].clone());
        self.input = Input::new(value);
    }

    fn tab(&mut self) {
        if let Some(done) = command::complete(self.input.value()) {
            self.input = Input::new(done);
        }
    }

    fn submit(&mut self) -> Option<String> {
        let line = self.input.value_and_reset();
        self.history_pos = None;
        if line.trim().is_empty() {
            return None;
        }
        if self.history.last() != Some(&line) {
            self.history.push(line.clone());
        }
        Some(line)
    }
}

fn chat_for_event(event: &serde_json::Value) -> Option<Chat> {
    let field = |k: &str| event[k].as_str().unwrap_or_default().to_owned();
    match event["type"].as_str()? {
        "message" => Some(Chat::You(field("text"))),
        "reaction_added" => Some(Chat::Reacji {
            added: true,
            name: field("reaction"),
        }),
        "reaction_removed" => Some(Chat::Reacji {
            added: false,
            name: field("reaction"),
        }),
        _ => None,
    }
}

pub async fn run(
    log: LogControl,
    mut logs: mpsc::UnboundedReceiver<LogLine>,
    watcher: CancellationToken,
    mut ready: oneshot::Receiver<HarnessHandle>,
    mut done: oneshot::Receiver<()>,
) -> AResult<()> {
    let mut terminal = ratatui::init();
    let res = event_loop(&mut terminal, log, &mut logs, &watcher, &mut ready, &mut done).await;
    ratatui::restore();
    res
}

async fn event_loop(
    terminal: &mut DefaultTerminal,
    log: LogControl,
    logs: &mut mpsc::UnboundedReceiver<LogLine>,
    watcher: &CancellationToken,
    ready: &mut oneshot::Receiver<HarnessHandle>,
    done: &mut oneshot::Receiver<()>,
) -> AResult<()> {
    let mut app = App::new(log);
    let mut keys = EventStream::new();
    let mut tick = time::interval(TICK);

    let (results_tx, mut results) = mpsc::unbounded_channel::<String>();
    let mut harness: Option<HarnessHandle> = None;
    let mut activity: Option<broadcast::Receiver<Activity>> = None;
    let mut status = None;

    loop {
        terminal.draw(|f| draw(f, &app))?;

        tokio::select! {
            _ = &mut *done => break,
            Some(line) = logs.recv() => app.logs.push(line),
            Some(text) = results.recv() => app.sys(text),
            _ = tick.tick() => (),

            Ok(handle) = &mut *ready, if harness.is_none() => {
                activity = Some(handle.monitor.subscribe());
                status = Some(handle.monitor.watch_status());
                app.status = Some(handle.monitor.status());
                app.sys(format!("ready, type {}help for commands", command::SIGIL));
                harness = Some(handle);
            }

            res = recv_activity(&mut activity) => match res {
                Ok(a) => app.activity(a),
                Err(broadcast::error::RecvError::Lagged(n)) => app.sys(format!("skipped {n} wire entries")),
                Err(broadcast::error::RecvError::Closed) => activity = None,
            },

            res = changed_status(&mut status) => match res {
                Some(s) => app.status = Some(s),
                None => status = None,
            },

            Some(event) = keys.next() => {
                let Ok(event) = event else { continue };
                let Event::Key(key) = event else { continue };
                if key.kind != KeyEventKind::Press {
                    continue;
                }
                match key {
                    KeyEvent { code: KeyCode::Char('c' | 'd'), modifiers: KeyModifiers::CONTROL, .. } => {
                        watcher.cancel();
                        app.sys("shutting down…");
                    }
                    KeyEvent { code: KeyCode::F(2), .. } => app.show_wire = !app.show_wire,
                    KeyEvent { code: KeyCode::Tab, .. } => app.tab(),
                    KeyEvent { code: KeyCode::Up, .. } => app.history_step(true),
                    KeyEvent { code: KeyCode::Down, .. } => app.history_step(false),
                    KeyEvent { code: KeyCode::Enter, .. } => {
                        if let Some(line) = app.submit() {
                            run_line(&mut app, harness.as_ref(), line, &results_tx);
                        }
                    }
                    _ => {
                        app.input.handle_event(&Event::Key(key));
                    }
                }
            }
        }
    }
    Ok(())
}

fn run_line(
    app: &mut App,
    harness: Option<&HarnessHandle>,
    line: String,
    results: &mpsc::UnboundedSender<String>,
) {
    let cmd = match line.parse::<Command>() {
        Ok(cmd) => cmd,
        Err(e) => return app.sys(format!("{e:#}")),
    };
    let Some(harness) = harness else {
        return app.sys("harness isn't ready yet");
    };
    if !matches!(cmd, Command::Message(_)) {
        app.sys(line);
    }

    let harness = harness.clone();
    let log = app.log.clone();
    let results = results.clone();
    tokio::spawn(async move {
        let ctx = Context {
            harness: &harness,
            log: Some(&log),
        };
        let out = match cmd.execute(&ctx).await {
            Ok(out) => out,
            Err(e) => format!("error: {e:#}"),
        };
        if !out.is_empty() {
            let _ = results.send(out);
        }
    });
}

async fn recv_activity(
    rx: &mut Option<broadcast::Receiver<Activity>>,
) -> Result<Activity, broadcast::error::RecvError> {
    match rx {
        Some(rx) => rx.recv().await,
        None => std::future::pending().await,
    }
}

async fn changed_status(rx: &mut Option<tokio::sync::watch::Receiver<Status>>) -> Option<Status> {
    match rx {
        Some(rx) => match rx.changed().await {
            Ok(()) => Some(rx.borrow_and_update().clone()),
            Err(_) => None,
        },
        None => std::future::pending().await,
    }
}

fn draw(f: &mut Frame<'_>, app: &App) {
    let [top, chat, input, status] = Layout::vertical([
        Constraint::Fill(2),
        Constraint::Fill(1),
        Constraint::Length(3),
        Constraint::Length(1),
    ])
    .areas(f.area());

    if app.show_wire {
        let [logs, wire] =
            Layout::horizontal([Constraint::Fill(1), Constraint::Fill(1)]).areas(top);
        draw_logs(f, app, logs, Borders::BOTTOM);
        draw_wire(f, app, wire);
    } else {
        draw_logs(f, app, top, Borders::BOTTOM);
    }
    draw_chat(f, app, chat);
    draw_input(f, app, input);
    draw_status(f, app, status);
}

fn rule(borders: Borders) -> Block<'static> {
    Block::new()
        .borders(borders)
        .border_style(Style::new().dark_gray())
}

fn tail<'a>(mut lines: Vec<Line<'a>>, block: Block<'a>, area: Rect) -> Paragraph<'a> {
    let inner = block.inner(area);
    let height = usize::from(inner.height);
    let used = Paragraph::new(lines.clone())
        .wrap(Wrap { trim: false })
        .line_count(inner.width);
    if used < height {
        lines.splice(0..0, std::iter::repeat_n(Line::default(), height - used));
    }
    let overflow = used.saturating_sub(height);
    Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(block)
        .scroll((u16::try_from(overflow).unwrap_or(u16::MAX), 0))
}

fn time_span(time: &DateTime<Local>) -> Span<'static> {
    Span::raw(time.format("%H:%M:%S ").to_string()).dark_gray()
}

fn draw_logs(f: &mut Frame<'_>, app: &App, area: Rect, borders: Borders) {
    let lines = app
        .logs
        .lines
        .iter()
        .map(|l| {
            let level = match l.level {
                Level::Error => Span::raw("ERROR").red(),
                Level::Warn => Span::raw("WARN ").yellow(),
                Level::Info => Span::raw("INFO ").green(),
                Level::Debug => Span::raw("DEBUG").blue(),
                Level::Trace => Span::raw("TRACE").magenta(),
            };
            Line::from(vec![
                time_span(&DateTime::<Local>::from(l.time)),
                level,
                Span::raw(format!(" {}: ", l.target)).dark_gray(),
                Span::raw(l.message.clone()),
            ])
        })
        .collect();
    let title = format!(" logs [{}] ", app.log.summary());
    let block = rule(borders).title_bottom(Line::raw(title).dark_gray());
    f.render_widget(tail(lines, block, area), area);
}

fn draw_wire(f: &mut Frame<'_>, app: &App, area: Rect) {
    let lines = app
        .wire
        .lines
        .iter()
        .map(|w| {
            let style = match &w.item {
                Activity::Connection(_) | Activity::RefreshRequested => Style::new().magenta(),
                Activity::Queued { .. } => Style::new().yellow(),
                Activity::EventSent { .. } => Style::new().cyan(),
                Activity::BotReply { .. } => Style::new().green(),
                Activity::Acked { .. } | Activity::ApiCall { .. } => Style::new().dark_gray(),
            };
            Line::from(vec![time_span(&w.time), Span::styled(w.item.to_string(), style)])
        })
        .collect();
    let block = rule(Borders::BOTTOM | Borders::LEFT)
        .border_set(ratatui::symbols::border::Set {
            bottom_left: "┴",
            ..ratatui::symbols::border::PLAIN
        })
        .title_bottom(Line::raw(" wire ").dark_gray());
    f.render_widget(tail(lines, block, area), area);
}

fn draw_chat(f: &mut Frame<'_>, app: &App, area: Rect) {
    let mut lines = Vec::new();
    for c in &app.chat.lines {
        let (who, text, style) = match &c.item {
            Chat::You(t) => (Span::raw("you  ").bold(), t.clone(), Style::new()),
            Chat::Reacji { added, name } => {
                let verb = if *added { "reacted" } else { "unreacted" };
                (
                    Span::raw("you  ").bold(),
                    format!(":{name}: {verb}"),
                    Style::new().dark_gray(),
                )
            }
            Chat::Bot(t) => (Span::raw("bot  ").green().bold(), t.clone(), Style::new().green()),
            Chat::Sys(t) => (Span::raw("sys  ").dark_gray(), t.clone(), Style::new().dark_gray()),
        };
        let indent = " ".repeat(time_span(&c.time).width() + who.width());
        let mut rows = text.lines();
        let first = rows.next().unwrap_or_default().to_owned();
        lines.push(Line::from(vec![time_span(&c.time), who, Span::styled(first, style)]));
        for row in rows {
            lines.push(Line::from(vec![
                Span::raw(indent.clone()),
                Span::styled(row.to_owned(), style),
            ]));
        }
    }
    f.render_widget(tail(lines, Block::new(), area), area);
}

fn draw_input(f: &mut Frame<'_>, app: &App, area: Rect) {
    let block = rule(Borders::TOP | Borders::BOTTOM);
    let inner = block.inner(area);
    let prompt = "> ";
    let width = usize::from(inner.width).saturating_sub(prompt.len()).max(1);
    let scroll = app.input.visual_scroll(width);

    let value: String = app.input.value().chars().skip(scroll).collect();
    let mut spans = vec![Span::raw(prompt).bold(), Span::raw(value)];
    if let Some(hint) = command::hint(app.input.value()) {
        spans.push(Span::raw(hint).dark_gray());
    }
    f.render_widget(Paragraph::new(Line::from(spans)).block(block), area);

    let cursor = app.input.visual_cursor().saturating_sub(scroll);
    let x = inner.x + u16::try_from(prompt.len() + cursor).unwrap_or(inner.width);
    f.set_cursor_position((x.min(inner.right().saturating_sub(1)), inner.y));
}

fn draw_status(f: &mut Frame<'_>, app: &App, area: Rect) {
    let mut spans = match &app.status {
        None => vec![Span::raw("● starting…").yellow()],
        Some(s) => {
            let (color, label) = match s.connection {
                Connection::Connected => (Color::Green, "connected"),
                Connection::Waiting => (Color::Yellow, "waiting"),
                Connection::NetDown => (Color::Red, "netdown"),
                Connection::CrashLoop => (Color::Red, "crashloop"),
                Connection::Stopped => (Color::Red, "stopped"),
            };
            vec![
                Span::styled(format!("● {label} ({})", s.connections), Style::new().fg(color)),
                Span::raw(format!(
                    " │ sent {}  acked {}  unacked {}  queued {} │ replies {} │ up {}",
                    s.events_sent,
                    s.events_acked,
                    s.unacked,
                    s.pending,
                    s.bot_replies,
                    uptime(s.started.elapsed()),
                ))
                .dark_gray(),
            ]
        }
    };
    spans.push(Span::raw(" │ F2 wire · /help · ^C quit").dark_gray());
    f.render_widget(Paragraph::new(Line::from(spans)), area);
}

fn uptime(d: Duration) -> String {
    let s = d.as_secs();
    match s {
        0..60 => format!("{s}s"),
        60..3600 => format!("{}m{:02}s", s / 60, s % 60),
        _ => format!("{}h{:02}m", s / 3600, (s % 3600) / 60),
    }
}
