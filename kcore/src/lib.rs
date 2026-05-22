mod bot;
mod command;
mod connection_state;
mod event;
mod sanitizer;
mod signal;
mod slack;

// TODO: Improve the ergonomics of the Slack Api interface
pub use slack::Client as SlackClient;
pub use slack::ConversationHistoryMessage as SlackReply;
pub use slack::HttpSender as SlackSender;
pub use slack::Message;
pub use slack::ReqwestSender;
pub use slack::Timezone as SlackTimezone;
pub use slack::User as SlackUser;
pub use slack::send_simple_message as send_text_message;

pub use bot::slack_websocket_handler;
pub use signal::wait_for_signal;

pub use command::Command;
pub use command::parse as parse_command;

pub use sanitizer::AtType;
pub use sanitizer::Segment;
pub use sanitizer::parse as sanitize_input;

pub use event::Envelope;
