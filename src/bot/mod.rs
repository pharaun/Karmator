#![allow(unused)]

mod tz;
pub mod user_event;

#[allow(clippy::all, clippy::pedantic, clippy::restriction, clippy::nursery)]
pub mod build {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}
