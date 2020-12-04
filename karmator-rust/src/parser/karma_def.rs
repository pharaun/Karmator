use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::IResult;

use crate::parser::karma::Karma;

// TODO: Refactor this entire crate to be better and ingest a list of karma items and generate the
// definition here but for now this will do.

pub fn str_to_karma(input: &str) -> Karma {
    match input {
        "++" => Karma::Up,
        "⧺"  => Karma::Up,
        "--" => Karma::Down,
        "—"  => Karma::Down,
        "╌"  => Karma::Down,
        "+-" => Karma::Side,
        "±"  => Karma::Side,
        "∓"  => Karma::Side,
        _    => panic!("Shouldn't arrive here"),
    }
}

pub fn karma_tags(input: &str) -> IResult<&str, &str> {
    alt((
        tag("++"),
        tag("⧺"),
        tag("--"),
        tag("—"),
        tag("╌"),
        tag("+-"),
        tag("±"),
        tag("∓"),
    ))(input)
}

pub const KARMA_LIST: [&str; 8] = ["++", "⧺", "--", "—", "╌", "+-", "±", "∓"];
