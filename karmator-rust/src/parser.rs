use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
      take,
  },
  multi::{
      separated_list0,
      many0,
  },
  combinator::{
      map,
      eof,
      peek,
  },
  branch::alt,
  sequence::{
      delimited,
      pair,
      terminated,
  },
  character::complete::{
      multispace1,
      multispace0,
  },
  error::{
      Error,
      ErrorKind,
  },
};
use std::fmt;
use std::matches;

use nom::InputLength;
use nom::InputTake;
use nom::InputIter;
use nom::Slice;
use nom::UnspecializedInput;

use std::iter::Enumerate;
use std::ops::RangeFull;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::ops::Range;


// TODO: add in specific support for parsing at-here and other special <!user_id> entities
// so that in the message subsystem it can do the right thing
#[derive(Debug, PartialEq)]
pub struct Command<'a> (pub &'a str, pub Vec<&'a str>);


pub fn command(input: &str) -> IResult<&str, Command> {
    let (input, _) = tag("!")(input)?;
    let (input, cmd) = command_string(input)?;
    let (input, _) = multispace0(input)?;
    let (input, arg) = args(input)?;

    Ok((input, Command(cmd, arg)))
}

fn command_string(input: &str) -> IResult<&str, &str> {
    take_while1(|c:char| c.is_alphanumeric())(input)
}

fn not_multispace1(input: &str) -> IResult<&str, &str> {
    alt((
        delimited(
            tag("\""),
            take_while1(|c:char| c != '"'),
            tag("\""),
        ),
        take_while1(|c:char| !c.is_whitespace()),
    ))(input)
}

fn args(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(multispace1, not_multispace1)(input)
}


// This starts karma tokenizer stream
// TODO: make fancy but for now basic objective
// 1. Convert inbound &str to a token stream and work on that (for structural concerns)
// 2. support 2 types of braces '"' and '['/']' to support single type and open/close
// 3. suppport 3 types of karma, up, down, side
// 4. support single character or multiple character karma token, for now '++', '--', '+-' '±'
// 5. support 'emoji' karma token - ':++:'
#[derive(Debug, PartialEq, Clone)]
enum KarmaToken<'a>{
    // Alphanumberic
    Text(&'a str),
    // Whitespace (with a copy in so we can reproduce the string exactly) (only because we care if
    // there is whitespace before the karma or not)
    Space(&'a str),
    // Only the " mark
    // TODO: quotation escaping?
    Quote,
    // Only the [ mark
    OpenBrace,
    // Only the ] mark
    CloseBrace,
    // Karma (only complete chunk makes it in) (':++:', '++', '--', '+-', '±' for now)
    // TODO: this is eager, will need to figure out if we want to deal with "+++" at all
    Karma(&'a str)
}

impl <'a> fmt::Display for KarmaToken<'a> {
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


fn all_token(input: &str) -> IResult<&str, Vec<KarmaToken>> {
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
        map(tag(":++:"), |k| KarmaToken::Karma(k)),
        map(tag("++"),   |k| KarmaToken::Karma(k)),
        map(tag("--"),   |k| KarmaToken::Karma(k)),
        map(tag("+-"),   |k| KarmaToken::Karma(k)),
        map(tag("±"),    |k| KarmaToken::Karma(k)),
        map(tag("\""),   |_| KarmaToken::Quote),
        map(tag("["),    |_| KarmaToken::OpenBrace),
        map(tag("]"),    |_| KarmaToken::CloseBrace),
        space,
        text,
    ))(input)
}

fn space(input: &str) -> IResult<&str, KarmaToken> {
    map(take_while1(|c:char| c.is_whitespace()), |s| KarmaToken::Space(s))(input)
}

fn text(input: &str) -> IResult<&str, KarmaToken> {
    map(take_while1(|c:char| c.is_alphanumeric()), |s| KarmaToken::Text(s))(input)
}


// Engine for allowing us to parse on top of tokens
#[derive(Debug, PartialEq, Clone, Copy)]
struct Tokens<'a> {
    tok: &'a [KarmaToken<'a>],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    fn new(vec: &'a Vec<KarmaToken<'a>>) -> Self {
        Tokens {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a> InputLength for KarmaToken<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a KarmaToken<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, KarmaToken<'a>>>;
    type IterElem = ::std::slice::Iter<'a, KarmaToken<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, KarmaToken<'a>>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, KarmaToken<'a>> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(|b| predicate(b))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

impl UnspecializedInput for Tokens<'_> {}



// Now here begins the actual structural karma parser (Karma Structure Tree (KST))
#[derive(Debug, PartialEq)]
enum KST<'a> {
    Karma(&'a str, &'a str),

    // Experiment for skipping on copy
    VKarma(&'a [&'a str], &'a str),
}


fn structural_karma(input: Tokens) -> IResult<Tokens, KST> {
    Ok((input, KST::Karma("str", "++")))
}


fn ktext(input: Tokens) -> IResult<Tokens, &str> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Text(t)) => Ok((input, t)),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kkarma(input: Tokens) -> IResult<Tokens, &str> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Karma(t)) => Ok((input, t)),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kspace(input: Tokens) -> IResult<Tokens, &str> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Space(t)) => Ok((input, t)),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kquote(input: Tokens) -> IResult<Tokens, &str> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Quote) => Ok((input, "\"")),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}


// TODO: develop invalid cases to test extent of the parser
// TODO: make basic parsers for - quote/(open/close brace)
//enum KarmaToken<'a>{ Text(&'a str), Space(&'a str), Quote, OpenBrace, CloseBrace, Karma(&'a str) }





// Simple Karma:
// 1. a++     -> ("",     Karma("a", "++"))
// 2. a++ b++ -> (" b++", Karma("a", "++"))
// 3. a++ b   -> (" b",   Karma("a", "++"))
// 4. a++b    -> Invalid
//
// Ground rule for this particular parse:
// 1. Text followed by karma
// 2. Karma followed by whitespace/eol
fn simple(input: Tokens) -> IResult<Tokens, KST> {
    map(terminated(
            pair(ktext, kkarma),
            alt((
                peek(kspace),
                map(eof, |_| ""),
            )),
        ),
        |(t, k)| KST::Karma(t, k)
    )(input)
}


// Simple MultiKarma:
// 1. a++     -> ("",   [Karma("a", "++")])
// 2. a++ b++ -> ("",   [Karma("a", "++"), Karma("b", "++")])
// 3. a++ b   -> (" b", [Karma("a", "++")])
//
// Ground rule for this particular parse:
// 1. SimpleKarma separated by whitespace
fn multi_simple(input: Tokens) -> IResult<Tokens, Vec<KST>> {
    separated_list0(kspace, simple)(input)
}


// Quoted Karma
// 1. "a"++       -> ("", Karma("a", "++"))
// 2. "a b"++     -> ("", Karma("a b", "++"))
// 3. " a b "++   -> ("", Karma("a b", "++")) - Trim " a b "
// 4. "a++"++     -> ("", Karma("a++", "++"))
// 5. "++"++      -> ("", Karma("++", "++"))
// 6. "a"++ "b"++ -> (" \"b\"++", Karma("a", "++"))
// 7. ""++        -> Invalid
// 8. " "++       -> Invalid
//
// Ground rule for this particular parse:
// 1. Quote followed by any; Karma/Text/Space
// 2. Closing with another Quote
// 3. At least 1 Text in between Quotes
// 4. Closing Quote followed by karma
// 5. Karma followed by whitespace/eol
fn quoted(input: Tokens) -> IResult<Tokens, KST> {
    Ok((input, KST::Karma("a", "++")))
    //let (input, a) = kenclosed(input)?;
    //Ok((input, KST::Karma(a, a)))
//    terminated(
//        pair(
//            delimited!(
//                kquote,
//                ?, //TODO: its own kparse here (complicated)
//                kquote,
//            ),
//            kkarma
//        ),
//        alt((
//            peek(kspace),
//            map(eof, |_| ""),
//        )),
//    )
}


// TODO: make this take a second parse for "inside" bits (ie take_(these)_till)
// 1. Quote followed by any; Karma/Text/Space
// 2. Closing with another Quote
// 3. At least 1 Text in between Quotes
fn kenclosed(input: Tokens) -> IResult<Tokens, &[&str]> {
    let (input, tkt) = take_while1(|kt:&KarmaToken|
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::Karma(_))
    )(input)?;

    // Collapse the list into string and trim it
    let temp = tkt.tok.iter().skip_while(
        |i| matches!(i, &KarmaToken::Space(_))
    ).collect::<Vec<&KarmaToken>>().drain(..).rev().skip_while(
        |i| matches!(i, &KarmaToken::Space(_))
    ).collect::<Vec<&KarmaToken>>().drain(..).rev().map(|i|
        match i {
            &KarmaToken::Space(s) => s,
            &KarmaToken::Text(s)  => s,
            &KarmaToken::Karma(s) => s,
            _ => ""
        }
    ).collect::<Vec<&str>>();

    if temp.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        Ok((input, &temp))
    }
}



#[cfg(test)]
mod test_quoted {
    use super::*;

    #[test]
    fn test_kenclosed() {
        let token = all_token("   text ++ ").unwrap().1;
        let parse = kenclosed(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, "")));
    }

    #[test]
    fn test_case_one() {
        let token = all_token("\"a\"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a", "++")])));
    }

    #[test]
    fn test_case_two() {
        let token = all_token("\"a b\"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a b", "++")])));
    }

    #[test]
    fn test_case_three() {
        let token = all_token("\" a b \"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a b", "++")])));
    }

    #[test]
    fn test_case_four() {
        let token = all_token("\"a++\"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a++", "++")])));
    }

    #[test]
    fn test_case_five() {
        let token = all_token("\"++\"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("++", "++")])));
    }

    #[test]
    fn test_case_six() {
        let token = all_token("\"a\"++ \"b\"++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![KarmaToken::Quote, KarmaToken::Text("b"), KarmaToken::Quote, KarmaToken::Karma("++")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("++", "++")])));
    }

    #[test]
    fn test_case_seven() {
        let token = all_token("\"\"++").unwrap().1;
        let parse = simple(Tokens::new(&token));

        // TODO: partial parse, do we want this?)
        let empty = vec![KarmaToken::Quote, KarmaToken::Quote, KarmaToken::Karma("++")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Err(nom::Err::Error(Error::new(empty, ErrorKind::Eof))));
    }

    #[test]
    fn test_case_eight() {
        let token = all_token("\" \"++").unwrap().1;
        let parse = simple(Tokens::new(&token));

        // TODO: partial parse, do we want this?)
        let empty = vec![KarmaToken::Quote, KarmaToken::Space(" "), KarmaToken::Quote, KarmaToken::Karma("++")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Err(nom::Err::Error(Error::new(empty, ErrorKind::Eof))));
    }
}


#[cfg(test)]
mod test_multi_simple {
    use super::*;

    #[test]
    fn test_case_one() {
        let token = all_token("a++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a", "++")])));
    }

    #[test]
    fn test_case_two() {
        let token = all_token("a++ b").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![KarmaToken::Space(" "), KarmaToken::Text("b")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a", "++")])));
    }

    #[test]
    fn test_case_three() {
        let token = all_token("a++ b++").unwrap().1;
        let parse = multi_simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, vec![KST::Karma("a", "++"), KST::Karma("b", "++")])));
    }
}


#[cfg(test)]
mod test_simple {
    use super::*;

    #[test]
    fn test_case_one() {
        let token = all_token("a++").unwrap().1;
        let parse = simple(Tokens::new(&token));

        let empty = vec![];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, KST::Karma("a", "++"))));
    }

    #[test]
    fn test_case_two() {
        let token = all_token("a++ b").unwrap().1;
        let parse = simple(Tokens::new(&token));

        let empty = vec![KarmaToken::Space(" "), KarmaToken::Text("b")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, KST::Karma("a", "++"))));
    }

    #[test]
    fn test_case_three() {
        let token = all_token("a++ b++").unwrap().1;
        let parse = simple(Tokens::new(&token));

        let empty = vec![KarmaToken::Space(" "), KarmaToken::Text("b"), KarmaToken::Karma("++")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Ok((empty, KST::Karma("a", "++"))));
    }

    #[test]
    fn test_case_four() {
        let token = all_token("a++b").unwrap().1;
        let parse = simple(Tokens::new(&token));

        // TODO: partial parse, do we want this?)
        let empty = vec![KarmaToken::Text("b")];
        let empty = Tokens::new(&empty);
        assert_eq!(parse, Err(nom::Err::Error(Error::new(empty, ErrorKind::Eof))));
    }
}


#[cfg(test)]
mod test_karma_token {
    use super::*;

    #[test]
    fn test_karma() {
        let karma = vec![":++:", "++", "--", "+-", "±"];

        assert_eq!(
            all_token(&karma.join("")),
            Ok(("", karma.iter().map(|k| KarmaToken::Karma(k)).collect()))
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
            Ok(("", vec![KarmaToken::Space(" \t")]))
        );
    }

    #[test]
    fn test_whitespace_block() {
        assert_eq!(
            all_token(" ]\t[  "),
            Ok(("", vec![
                KarmaToken::Space(" "),
                KarmaToken::CloseBrace,
                KarmaToken::Space("\t"),
                KarmaToken::OpenBrace,
                KarmaToken::Space("  ")
            ]))
        );
    }

    #[test]
    fn test_text() {
        assert_eq!(
            all_token("this藏test"),
            Ok(("", vec![KarmaToken::Text("this藏test")]))
        );
    }

    #[test]
    fn test_text_block() {
        assert_eq!(
            all_token("this\t[\"藏  ++]"),
            Ok(("", vec![
                KarmaToken::Text("this"),
                KarmaToken::Space("\t"),
                KarmaToken::OpenBrace,
                KarmaToken::Quote,
                KarmaToken::Text("藏"),
                KarmaToken::Space("  "),
                KarmaToken::Karma("++"),
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
}


#[cfg(test)]
mod test_command {
    use super::*;

    #[test]
    fn test_command() {
        assert_eq!(
            command("!karma"),
            Ok(("", Command("karma", vec![])))
        );
    }

    #[test]
    fn test_command_one_arg() {
        assert_eq!(
            command("!karma a"),
            Ok(("", Command("karma", vec!["a"])))
        );
    }

    #[test]
    fn test_command_two_arg() {
        assert_eq!(
            command("!karma a bcd"),
            Ok(("", Command("karma", vec!["a", "bcd"])))
        );
    }

    #[test]
    fn test_command_one_quote_arg() {
        assert_eq!(
            command("!karma \"a b\""),
            Ok(("", Command("karma", vec!["a b"])))
        );
    }

    #[test]
    fn test_command_two_quote_arg() {
        assert_eq!(
            command("!karma \"a b\" \"c d\""),
            Ok(("", Command("karma", vec!["a b", "c d"])))
        );
    }

    #[test]
    fn test_command_two_quote_and_unquote_arg() {
        assert_eq!(
            command("!karma \"a b\" cd"),
            Ok(("", Command("karma", vec!["a b", "cd"])))
        );
    }

    #[test]
    fn test_command_fail_close_arg() {
        assert_eq!(
            command("!karma \"a"),
            Ok(("", Command("karma", vec!["\"a"])))
        );
    }

    #[test]
    fn test_command_fail_open_arg() {
        assert_eq!(
            command("!karma a\""),
            Ok(("", Command("karma", vec!["a\""])))
        );
    }

    // TODO: decide how to deal with this case
//    #[test]
//    fn test_command_multi_quote_arg() {
//        assert_eq!(
//            command("!karma \"\"a\"\""),
//            Ok(("", Command("karma", vec!["a"])))
//        );
//    }

    // TODO: decide how to deal with this case
//    #[test]
//    fn test_command_escape_quote_arg() {
//        assert_eq!(
//            command("!karma \"\\\"a\\\"\""),
//            Ok(("", Command("karma", vec!["\"a\""])))
//        );
//    }
}
