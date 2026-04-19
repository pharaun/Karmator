mod tz;
pub mod user_event;

#[allow(clippy::all, clippy::pedantic, clippy::restriction, clippy::nursery, unused)]
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}
