use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::IResult;

use crate::parser::karma::Karma;

// TODO: Refactor this entire crate to be better and ingest a list of karma items and generate the
// definition here but for now this will do.

macro_rules! tags {
    ( $input:ident, $( $x:expr ),* ) => {
        {
            alt((
                $(
                    tag($x),
                )*
            ))($input)
        }
    };
}

// Karma definition list (Try to generate the rest from this)
pub const KARMA_LIST: [&str; 8] = ["++", "--", "—", "╌", "+-", "-+", "±", "∓"];

pub fn karma_tags(input: &str) -> IResult<&str, &str> {
    tags!(input, "++", "--", "—", "╌", "+-", "-+", "±", "∓")
}

pub fn str_to_karma(input: &str) -> Karma {
    match input {
        "++" => Karma::Up,
        "--" => Karma::Down,
        "—"  => Karma::Down,
        "╌"  => Karma::Down,
        "+-" => Karma::Side,
        "-+" => Karma::Side,
        "±"  => Karma::Side,
        "∓"  => Karma::Side,
        _    => panic!("Shouldn't arrive here"),
    }
}

pub fn reacji_to_karma(input: &str) -> Option<Karma> {
    match input {
        "++"            => Some(Karma::Up),
        "++1"           => Some(Karma::Up),
        "++vert"        => Some(Karma::Up),
        "plusone"       => Some(Karma::Up),
        "plus1"         => Some(Karma::Up),
        "++-inverted"   => Some(Karma::Up),
        "upvote"        => Some(Karma::Up),
        "--"            => Some(Karma::Down),
        "minus1"        => Some(Karma::Down),
        "downvote"      => Some(Karma::Down),
        "+-"            => Some(Karma::Side),
        _               => None,
    }
}
