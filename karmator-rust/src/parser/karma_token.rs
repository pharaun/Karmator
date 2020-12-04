use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
      take,
  },
  multi::{
      many0,
  },
  combinator::{
      map,
      peek,
  },
  branch::alt,
  error::{
      Error,
      ErrorKind,
  },
};
use std::fmt;

use crate::parser::karma_def::karma_tags;
use crate::parser::karma_def::KARMA_LIST;


// This starts karma tokenizer stream
// TODO: make fancy but for now basic objective
// TODO: we probs want to stress test the round-trip feature (use a fuzzer to help this)
// 1. Convert inbound &str to a token stream and work on that (for structural concerns)
// 2. support 2 types of braces '"' and '['/']' to support single type and open/close
// 3. suppport 3 types of karma, up, down, side
// 4. support single character or multiple character karma token, for now '++', '--', '+-' '±'
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum KarmaToken{
    // Alphanumberic
    Text(String),
    // Whitespace (with a copy in so we can reproduce the string exactly) (only because we care if
    // there is whitespace before the karma or not)
    Space(String),
    // Only the " mark
    // TODO: quotation escaping?
    Quote,
    // Only the [ mark
    OpenBrace,
    // Only the ] mark
    CloseBrace,
    // Karma (only complete chunk makes it in) ('++', '--', '+-', '±' for now)
    // For runs to the left of this, see KText()
    Karma(String),
    // Karma Run text, this contains any karma runs ie. "+++++++++" that is to the left of
    // an valid karma parse (to support c++--), It needs to be its own type becaue it is slightly
    // more restrictive than plain Text in downstream parsers
    KText(String),
}

impl <'a> fmt::Display for KarmaToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaToken::Text(t)    => write!(f, "{}", t),
            KarmaToken::Space(t)   => write!(f, "{}", t),
            KarmaToken::Karma(t)   => write!(f, "{}", t),
            KarmaToken::KText(t)   => write!(f, "{}", t),
            KarmaToken::Quote      => write!(f, "\""),
            KarmaToken::OpenBrace  => write!(f, "["),
            KarmaToken::CloseBrace => write!(f, "]"),
        }
    }
}


pub fn all_token(input: &str) -> IResult<&str, Vec<KarmaToken>> {
    many0(token)(input)
}

// TODO: maybe instead of parsing in only whole karma chunk could be greedy
// and start only if its a whole chunk (for some) but if there's more,
// continue accumulating. "a+++" -> Text("a"), Karma("+++") for eg. Then parse
// the rightmost for valid karma value to use
//
// Look into just capturing all value in a karma, then doing some sort of iterative
// parsing to try to fit things together, and if it comes up with something workable
// use it otherwise reject it.
//
// Rightmost parse with space/eof after, will become a token, the rejects get shuffled
// back into a Text
fn token(input: &str) -> IResult<&str, KarmaToken> {
    alt((
        symbols,
        space,
        karma_run,
        karma,
        text,
    ))(input)
}

fn symbols(input: &str) -> IResult<&str, KarmaToken> {
    alt((
        map(tag("\""),   |_| KarmaToken::Quote),
        map(tag("["),    |_| KarmaToken::OpenBrace),
        map(tag("]"),    |_| KarmaToken::CloseBrace),
    ))(input)
}

fn space(input: &str) -> IResult<&str, KarmaToken> {
    map(take_while1(|c:char| c.is_whitespace()), |s:&str| KarmaToken::Space(s.to_string()))(input)
}

fn karma(input: &str) -> IResult<&str, KarmaToken> {
    map(karma_tags, |k:&str| KarmaToken::Karma(k.to_string()))(input)
}

fn karma_run(input: &str) -> IResult<&str, KarmaToken> {
    let (_, candidate) = take_while1(|c:char| is_karma_symbol(c))(input)?;
    let mut longest = "";

    // Let's try a list of karma
    for k in KARMA_LIST.iter() {
        // Short circuit this check if
        // 1. karma is shorter than longest match, it won't win
        // 2. candidate is shorter than the karma to be checked
        if (longest.len() > k.len()) | (k.len() > candidate.len()) {
            continue;
        }

        // Try a rmatch approach first
        let v: Vec<_> = candidate.rmatch_indices(k).collect();

        // Check if there is any possible match at all
        if !v.is_empty() {
            // There is, let's verify if the index of the match is eol
            let (idx, _) = v.get(0).unwrap();
            let offset = candidate.len() - k.len();

            if idx == &offset {
                if k.len() >= longest.len() {
                    longest = k;
                }
            }
        }
    }

    // If longest is empty, nothing matched
    if longest.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        // Returns the input with the text portion sliced off, also return the karma matched to
        // Memory layout is
        // [input_slice, longest, tmp_input] and we want to return
        // [input_slice, [longest + tmp_input] via slicing the original input
        let input_slice = &candidate[
            0..(candidate.len() - longest.len())
        ];
        let ret_slice = &input[
            input_slice.len()..
        ];

        // If text to be returned is empty, we only got the karma/other parse now
        if input_slice.is_empty() {
            Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))

        } else {
            Ok((&ret_slice, KarmaToken::KText(input_slice.to_string())))
        }
    }
}

fn is_karma_symbol(s: char) -> bool {
    let sym = vec!['+', '-', '±'];
    sym.contains(&s)
}

fn is_symbol(s: char) -> bool {
    let sym = vec!['"', '[', ']'];
    sym.contains(&s)
}

// Steps needed:
// 1. Take any non [whitespace, part of karma, or braces]
// 2. When take_while stops, do a peek parse of [whitespace/karma/braces]
// 3. [if fail] add the character to the string and resume take_while
// 4. [if succ] exit parser with a Text(ret)
fn text(input: &str) -> IResult<&str, KarmaToken> {
    let mut ret = "".to_string();
    let mut cur_input = input;

    // Bail out if empty input
    if input.is_empty() {
        return Err(nom::Err::Error(Error::new(input, ErrorKind::Eof)));
    }

    loop {
        // Force forward progress
        let par: IResult<&str, &str> = take_while1(
            |c:char| !c.is_whitespace() & !is_symbol(c) & !is_karma_symbol(c)
        )(cur_input);
        if let Ok((input, tok)) = par {
            cur_input = input;
            ret.push_str(tok);
        }

        // Check if it is a whitespace or symbol parse
        let par = peek(alt((
            space,
            symbols,
            karma_run,
            karma,
        )))(cur_input);

        match par {
            Ok(_)  => break,
            Err(_) => {
                // Check if it hit eof, if so exit
                if cur_input.is_empty() {
                    break;
                }

                let (input, tok) = take(1usize)(cur_input)?;
                cur_input = input;
                ret.push_str(tok);
            },
        }
    }

    Ok((cur_input, KarmaToken::Text(ret)))
}


#[cfg(test)]
macro_rules! space {
    ($data:expr) => {
        KarmaToken::Space($data.to_string())
    }
}

#[cfg(test)]
macro_rules! text {
    ($data:expr) => {
        KarmaToken::Text($data.to_string())
    }
}

#[cfg(test)]
macro_rules! karma {
    ($data:expr) => {
        KarmaToken::Karma($data.to_string())
    }
}

#[cfg(test)]
macro_rules! ktext {
    ($data:expr) => {
        KarmaToken::KText($data.to_string())
    }
}


#[cfg(test)]
mod test_karma_token {
    use super::*;

    #[test]
    fn test_rightmost_valid() {
        assert_eq!(
            karma_run("--++"),
            Ok(("++", ktext!("--")))
        );
    }

    #[test]
    fn test_rightmost_only_karma() {
        assert_eq!(
            karma_run("++"),
            Err(nom::Err::Error(Error::new("++", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_rightmost_text_before() {
        assert_eq!(
            karma_run("text++"),
            Err(nom::Err::Error(Error::new("text++", ErrorKind::TakeWhile1)))
        );
    }

    #[test]
    fn test_rightmost_text_after() {
        assert_eq!(
            karma_run("++text"),
            Err(nom::Err::Error(Error::new("++text", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_karma_run() {
        assert_eq!(
            all_token("++--++"),
            Ok(("", vec![ktext!("++--"), karma!("++")]))
        );
    }

    #[test]
    fn test_karma_incomplete_run() {
        assert_eq!(
            all_token("+++"),
            Ok(("", vec![ktext!("+"), karma!("++")]))
        );
    }

    // TODO: this test depends on us supporting "+-" but not "-+" find alternatives later
    //#[test]
    //fn test_karma_incomplete_not_valid_run() {
    //    assert_eq!(
    //        all_token("+-+"),
    //        Ok(("", vec![karma!("+-"), text!("+")]))
    //    );
    //}

    #[test]
    fn test_quote() {
        assert_eq!(
            all_token("\"\"\""),
            Ok(("", vec![KarmaToken::Quote; 3]))
        );
    }

    #[test]
    fn test_brace() {
        assert_eq!(
            all_token("][]]"),
            Ok(("", vec![
                KarmaToken::CloseBrace,
                KarmaToken::OpenBrace,
                KarmaToken::CloseBrace,
                KarmaToken::CloseBrace
            ]))
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            all_token(" \t"),
            Ok(("", vec![space!(" \t")]))
        );
    }

    #[test]
    fn test_whitespace_block() {
        assert_eq!(
            all_token(" ]\t[  "),
            Ok(("", vec![
                space!(" "),
                KarmaToken::CloseBrace,
                space!("\t"),
                KarmaToken::OpenBrace,
                space!("  ")
            ]))
        );
    }

    #[test]
    fn test_text_plain() {
        assert_eq!(
            all_token("this藏test-d"),
            Ok(("", vec![text!("this藏test-d")]))
        );
    }

    #[test]
    fn test_text_starting_ending_hypen() {
        assert_eq!(
            all_token("-test-"),
            Ok(("", vec![text!("-test-")]))
        );
    }

    // TODO: this test depends on us supporting "+-" but not "-+" find alternatives later
    //#[test]
    //fn test_text_unsupported_karma() {
    //    assert_eq!(
    //        all_token("text-+"),
    //        Ok(("", vec![text!("text-+")]))
    //    );
    //}

    #[test]
    fn test_text_block() {
        assert_eq!(
            all_token("this\t[\"藏  ++]"),
            Ok(("", vec![
                text!("this"),
                space!("\t"),
                KarmaToken::OpenBrace,
                KarmaToken::Quote,
                text!("藏"),
                space!("  "),
                karma!("++"),
                KarmaToken::CloseBrace,
            ]))
        );
    }

    #[test]
    fn test_roundtrip() {
        let text = "this\t[\"藏  ++]";
        let parse = all_token(text).unwrap().1;

        assert_eq!(parse.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(""), text);
    }

    #[test]
    fn test_fuzz_one() {
        assert_eq!(
            all_token("±+\""),
            Ok(("", vec![karma!("±"), text!("+"), KarmaToken::Quote]))
        );
    }
}
