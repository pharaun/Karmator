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
use unicode_segmentation::UnicodeSegmentation;


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
    // TODO: this is eager, will need to figure out if we want to deal with "+++" at all
    Karma(String)
}

impl <'a> fmt::Display for KarmaToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaToken::Text(t)    => write!(f, "{}", t),
            KarmaToken::Space(t)   => write!(f, "{}", t),
            KarmaToken::Karma(t)   => write!(f, "{}", t),
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
        // TODO: should be able to make this work better if we break up the
        // rightmost_karma into 2 pieces, a karma parse, and a 'remaining text' parse
        // and then have one return karma, one return text, and then make text_or_karma
        // do a karma/symbol/rightmost parse and bail if it hits it. this will result in
        // a bit more text(), text() ... sequence but that should be overall harmless?
        text_or_karma,
    ))(input)
}

fn symbols(input: &str) -> IResult<&str, KarmaToken> {
    alt((
        //map(karma,       |k| KarmaToken::Karma(k.to_string())),
        map(tag("\""),   |_| KarmaToken::Quote),
        map(tag("["),    |_| KarmaToken::OpenBrace),
        map(tag("]"),    |_| KarmaToken::CloseBrace),
    ))(input)
}

fn karma(input: &str) -> IResult<&str, &str> {
    alt((tag("++"), tag("--"), tag("+-"), tag("±")))(input)
}

fn rightmost_karma(input: &str) -> IResult<&str, (&str, &str)> {
    let (_, candidate) = take_while1(|c:char| is_karma_symbol(c))(input)?;

    // Let's try a list of karma
    let klist = ["++", "--", "+-", "±"];
    let mut longest = "";

    // Debug block
    let ktup = klist.iter().map(|k| (k, k.len())).collect::<Vec<_>>();
    println!("ktup {:?}", ktup);

    let g = UnicodeSegmentation::graphemes(input, true).collect::<Vec<&str>>();
    println!("input: {:?}\nunicode: {:?}", input, g);

    for k in klist.iter() {
        // Make sure candidate length is greater or equal to the karma token length
        if candidate.len() >= k.len() {
            // TODO: the issue here is that we are slicing on bytes, but... characters...
            // Find a better way to do the slicing
            let c_slice = &candidate[
                (candidate.len() - k.len())..
            ];

            // Check if the karma candidate match
            if &c_slice == k {
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

        Ok((&ret_slice, (&input_slice, longest)))
    }
}


fn space(input: &str) -> IResult<&str, KarmaToken> {
    map(take_while1(|c:char| c.is_whitespace()), |s:&str| KarmaToken::Space(s.to_string()))(input)
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
fn text_or_karma(input: &str) -> IResult<&str, KarmaToken> {
    let mut ret = "".to_string();
    let mut cur_input = input;

    // Bail out if empty input
    if input.is_empty() {
        return Err(nom::Err::Error(Error::new(input, ErrorKind::Eof)));
    }

    loop {
        println!("========");
        println!("1i: {:?}", cur_input);
        println!("1r: {:?}", ret);

        // Force forward progress
        let par: IResult<&str, &str> = take_while1(
            |c:char| !c.is_whitespace() & !is_symbol(c) & !is_karma_symbol(c)
        )(cur_input);
        println!("1p: {:?}", par);
        if let Ok((input, tok)) = par {
            cur_input = input;
            ret.push_str(tok);
        }

        println!("");
        println!("2i: {:?}", cur_input);
        println!("2r: {:?}", ret);

        // Check if it is a whitespace or symbol parse
        let par = peek(alt((space, symbols)))(cur_input);
        println!("2p: {:?}", par);
        match par {
            Ok(_)  => break,
            Err(_) => {
                // Check if it hit eof, if so exit
                if cur_input.is_empty() {
                    break;
                }

                // Check if its a rightmost karma parse
                let kpar = rightmost_karma(cur_input);
                println!("2k: {:?}", kpar);
                match kpar {
                    Ok((input, (txt, kar))) => {
                        // Check if the returned txt is empty, if so then its a valid karma
                        // and we should bail here to allow the karma tag to parse it?
                        //
                        // TODO: identify if we should return early instead of exit early with a
                        // karma parse
                        println!("********");
                        println!("cur_input: {:?}", cur_input);
                        println!("ret      : {:?}", ret);
                        println!("input    : {:?}", input);
                        println!("karma    : {:?}", kar);
                        println!("kar-text : {:?}", txt);
                        println!("********");

                        // If ret and txt is empty, return a karma parse
                        if txt.is_empty() & ret.is_empty() {
                            // Do a karma parse
                            return map(karma, |k| KarmaToken::Karma(k.to_string()))(cur_input);

                        } else if txt.is_empty() {
                            break;

                        } else {
                            cur_input = input;
                            ret.push_str(txt);
                        }
                    },
                    Err(_) => {
                        // It did not parse, grab the next char and append it and continue
                        let (input, tok) = take(1usize)(cur_input)?;
                        cur_input = input;
                        ret.push_str(tok);
                    },
                }

                println!("");
                println!("3i: {:?}", cur_input);
                println!("3r: {:?}", ret);
            },
        }
    }

    let stuff = Ok((cur_input, KarmaToken::Text(ret)));
    println!("========");
    println!("ret: {:?}", stuff);
    stuff
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
mod test_karma_token {
    use super::*;

    #[test]
    fn test_rightmost_valid() {
        assert_eq!(
            rightmost_karma("--++"),
            Ok(("++", ("--", "++")))
        );
    }

    #[test]
    fn test_rightmost_only_karma() {
        assert_eq!(
            rightmost_karma("++"),
            Ok(("++", ("", "++")))
        );
    }

    #[test]
    fn test_rightmost_text_before() {
        assert_eq!(
            rightmost_karma("text++"),
            Err(nom::Err::Error(Error::new("text++", ErrorKind::TakeWhile1)))
        );
    }

    #[test]
    fn test_rightmost_text_after() {
        assert_eq!(
            rightmost_karma("++text"),
            Ok(("++text", ("", "++")))
        );
    }

    #[test]
    fn test_karma_run() {
        assert_eq!(
            all_token("++--++"),
            Ok(("", vec![text!("++--"), karma!("++")]))
        );
    }

    #[test]
    fn test_karma_incomplete_run() {
        assert_eq!(
            all_token("+++"),
            Ok(("", vec![text!("+"), karma!("++")]))
        );
    }

    // TODO: this test depends on us supporting "+-" but not "-+" find alternatives later
    #[test]
    fn test_karma_incomplete_not_valid_run() {
        assert_eq!(
            all_token("+-+"),
            Ok(("", vec![text!("+-+")]))
        );
    }

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
    fn test_text() {
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
    #[test]
    fn test_text_unsupported_karma() {
        assert_eq!(
            all_token("text-+"),
            Ok(("", vec![text!("text-+")]))
        );
    }

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
            Ok(("", vec![text!("±+\"")]))
        );
    }
}
