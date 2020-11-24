use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
      take_till1,
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
      complete,
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
// TODO: make command parser support [] and "" for querying the karma system
#[derive(Debug, PartialEq)]
pub struct Command<'a> (pub &'a str, pub Vec<&'a str>);


pub fn parse_command(input: &str) -> Result<Command, String> {
    let cmd = complete(command)(input);

    match cmd {
        Err(x)       => Err(format!("{:?}", x)),
        Ok((_, res)) => Ok(res),
    }
}


fn command(input: &str) -> IResult<&str, Command> {
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
// TODO: we probs want to stress test the round-trip feature (use a fuzzer to help this)
// 1. Convert inbound &str to a token stream and work on that (for structural concerns)
// 2. support 2 types of braces '"' and '['/']' to support single type and open/close
// 3. suppport 3 types of karma, up, down, side
// 4. support single character or multiple character karma token, for now '++', '--', '+-' '±'
// 5. support 'emoji' karma token - ':++:'
#[derive(Debug, PartialEq, Clone)]
enum KarmaToken{
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
    // Karma (only complete chunk makes it in) (':++:', '++', '--', '+-', '±' for now)
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
        symbols,
        space,
        text,
    ))(input)
}

fn symbols(input: &str) -> IResult<&str, KarmaToken> {
    alt((
        map(tag(":++:"), |k:&str| KarmaToken::Karma(k.to_string())),
        map(tag("++"),   |k:&str| KarmaToken::Karma(k.to_string())),
        map(tag("--"),   |k:&str| KarmaToken::Karma(k.to_string())),
        map(tag("+-"),   |k:&str| KarmaToken::Karma(k.to_string())),
        map(tag("±"),    |k:&str| KarmaToken::Karma(k.to_string())),
        map(tag("\""),   |_| KarmaToken::Quote),
        map(tag("["),    |_| KarmaToken::OpenBrace),
        map(tag("]"),    |_| KarmaToken::CloseBrace),
    ))(input)
}

fn space(input: &str) -> IResult<&str, KarmaToken> {
    map(take_while1(|c:char| c.is_whitespace()), |s:&str| KarmaToken::Space(s.to_string()))(input)
}

fn is_symbol(s: char) -> bool {
    let sym = vec![':', '+', '-', '"', '[', ']', '±'];
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

    loop {
        let (input, tok) = take_while1(|c:char| !c.is_whitespace() & !is_symbol(c))(cur_input)?;
        cur_input = input;
        ret.push_str(tok);

        // Check if it is a whitespace or symbol parse
        let par = peek(alt((space, symbols)))(cur_input);
        match par {
            Ok(_)  => break,
            Err(_) => {
                // Check if it hit eof, if so exit
                if cur_input.len() == 0 {
                    break;
                }

                // It did not parse, grab the next char and append it and continue
                let (input, tok) = take(1usize)(cur_input)?;
                cur_input = input;
                ret.push_str(tok);
            },
        }
    }

    Ok((cur_input, KarmaToken::Text(ret)))
}


// Engine for allowing us to parse on top of tokens
#[derive(Debug, PartialEq, Clone, Copy)]
struct Tokens<'a> {
    tok: &'a [KarmaToken],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    fn new(vec: &'a Vec<KarmaToken>) -> Self {
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

impl<'a> InputLength for KarmaToken {
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
    type Item = &'a KarmaToken;
    type Iter = Enumerate<::std::slice::Iter<'a, KarmaToken>>;
    type IterElem = ::std::slice::Iter<'a, KarmaToken>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, KarmaToken>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, KarmaToken> {
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
pub struct KST(pub String, pub String);


pub fn parse_karma(input: &str) -> Result<Vec<KST>, String> {
    let tokens = complete(all_token)(input);

    match tokens {
        Err(x)           => Err(format!("{:?}", x)),
        Ok((_, tok)) => {
            let result = complete(multi)(Tokens::new(&tok));

            match result {
                Err(x)       => Err(format!("{:?}", x)),
                Ok((_, res)) => Ok(res),
            }
        },
    }
}


fn kkarma(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Karma(t)) => Ok((input, t.to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kspace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Space(t)) => Ok((input, t.to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kquote(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Quote) => Ok((input, "\"".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kopenbrace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::OpenBrace) => Ok((input, "[".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kclosebrace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::CloseBrace) => Ok((input, "]".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kenclosedquote(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take_while1(|kt:&KarmaToken|
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::Karma(_) | &KarmaToken::Quote)
    )(input)?;

    // Collapse the list into string and trim it
    let temp = tkt.tok.iter().map(
        |k:&KarmaToken| k.to_string()
    ).collect::<Vec<String>>().join("").trim().to_string();

    if temp.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        Ok((input, temp))
    }
}

fn kenclosedbrace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take_while1(|kt:&KarmaToken|
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::Karma(_) | &KarmaToken::OpenBrace | &KarmaToken::CloseBrace)
    )(input)?;

    // Collapse the list into string and trim it
    let temp = tkt.tok.iter().map(
        |k:&KarmaToken| k.to_string()
    ).collect::<Vec<String>>().join("").trim().to_string();

    if temp.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        Ok((input, temp))
    }
}

fn kspacetext(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take_while1(|kt:&KarmaToken|
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_))
    )(input)?;

    // Validate that last entity isn't a space
    match tkt.tok.last() {
        Some(&KarmaToken::Space(_)) => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
        _ => {
            // Collapse the list into string and trim it
            let temp = tkt.tok.iter().map(
                |k:&KarmaToken| k.to_string()
            ).collect::<Vec<String>>().join("").trim_start().to_string();

            Ok((input, temp))
        },
    }
}


// Macro for cleaning up the test cases
#[cfg(test)]
macro_rules! success_test {
    ($name:ident, $parse:ident, $data:expr, $buff:expr, $res:expr) => (
        #[test]
        fn $name() {
            let token = all_token($data).unwrap().1;
            let parse = $parse(Tokens::new(&token));

            let empty = $buff;
            let empty = Tokens::new(&empty);
            assert_eq!(parse, Ok((empty, $res)));
        }
    )
}

#[cfg(test)]
macro_rules! fail_test {
    ($name:ident, $parse:ident, $data:expr, $buff:expr, $kind:expr) => (
        #[test]
        fn $name() {
            let token = all_token($data).unwrap().1;
            let parse = $parse(Tokens::new(&token));

            let empty = $buff;
            let empty = Tokens::new(&empty);
            assert_eq!(parse, Err(nom::Err::Error(Error::new(empty, $kind))));
        }
    )
}

#[cfg(test)]
macro_rules! kst {
    ($data:expr, $karma:expr) => {
        KST($data.to_string(), $karma.to_string())
    }
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


// Simple Karma:
// 1. a++       -> ("",     Karma("a", "++"))
// 2. a b++     -> ("",     Karma("a b", "++"))
// 3. a++ b++   -> (" b++", Karma("a", "++"))
// 4. a b++ c   -> (" c",   Karma("a b", "++"))
// 5.   a++     -> ("",     Karma("a", "++")) - Trim preceeding
// 6. a b++c    -> Invalid
// 7. a++b      -> Invalid
// 8. a b ++    -> Invalid
// 9. [""]++    -> Invalid
//
// Ground rule for this particular parse:
// 1. Any Text/Space followed by karma
// 2. Karma must follow a Text
// 3. karma must be followed by space/eol
fn simple(input: Tokens) -> IResult<Tokens, KST> {
    map(terminated(
            pair(
                kspacetext,
                kkarma
            ),
            alt((
                peek(kspace),
                map(eof, |_| "".to_string()),
            )),
        ),
        |(t, k)| KST(t, k)
    )(input)
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
// 9. "[]"++      -> "", Karma("[]", "++")
//
// Ground rule for this particular parse:
// 1. Quote followed by any; Karma/Text/Space
// 2. Closing with another Quote
// 3. At least 1 Text in between Quotes
// 4. Closing Quote followed by karma
// 5. Karma followed by whitespace/eol
fn quoted(input: Tokens) -> IResult<Tokens, KST> {
    map(terminated(
            pair(
                delimited(
                    kquote,
                    kenclosedbrace,
                    kquote
                ),
                kkarma
            ),
            alt((
                peek(kspace),
                map(eof, |_| "".to_string())
            ))
        ),
        |(t, k)| KST(t, k)
    )(input)
}


// Braced Karma
// Same outcome as quoted case just with [ ] instead of " "
// 1. [a]++
// 2. [a b]++
// 3. [ a b ]++
// 4. [a++]++
// 5. [++]++
// 6. [a]++ [b]++
// 7. []++
// 8. [ ]++
// 9. [""]++
//
// Ground rule for this particular parse:
// 1. OpenBrace followed by any; Karma/Text/Space
// 2. Closing with CloseBrace
// 3. At least 1 Text in between Open/Close Brace
// 4. ClosedBrace followed by karma
// 5. Karma followed by whitespace/eol
fn braced(input: Tokens) -> IResult<Tokens, KST> {
    map(terminated(
            pair(
                delimited(
                    kopenbrace,
                    kenclosedquote,
                    kclosebrace
                ),
                kkarma
            ),
            alt((
                peek(kspace),
                map(eof, |_| "".to_string())
            ))
        ),
        |(t, k)| KST(t, k)
    )(input)
}


// TODO: develop invalid cases to test extent of the parser
// MultiKarma (K(x) == Karma(x, "++"))
// 1. a++ "b"++ [c]++       -> ("", [K("a"), K("b"), K("c")])
// 2. a b++ c d++           -> ("", [K("a b"), K("c d")])
// 3. abc "d"++ def         -> ("", [K("d")])
// 4. [a"b"c]++             -> ("", [K("a\"b\"c")])
// 5. [a b]++ c d++ "e f"++ -> ("", [K("a b"), K("c d"), K("e f")])
// 6. a"b[c]d++             -> ("", [])
// 7. [a]]b["++             -> ("", [])
// 8. <empty string>        -> ("", [])
// 9. a-b++                 -> ("", [K("a-b")])
//
// Ground rule for this particular parse:
// 1. Apply simple combinator as many time as possible
// 2. Apply quote combinator as many time as possible
// 3. Apply brace combinator as many time as possible
// 4. If still more, discard till space/karma, goto 1
fn multi(input: Tokens) -> IResult<Tokens, Vec<KST>> {
    let mut ret = vec![];
    let mut cur_input = input;

    loop {
        // 1. Apply simple combinator as many times as possible
        let (input, res) = many0(simple)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        // 2. Apply quote combinator as many time as possible
        let (input, res) = many0(quoted)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        // 3. Apply brace combinator as many time as possible
        let (input, res) = many0(braced)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        // 4. If still more, discard 1 token and go to 1
        if cur_input.tok.len() != 0 {
            // If space or karma, drop 1, otherwise eat till space/karma
            match cur_input.tok.get(0) {
                Some(KarmaToken::Space(_)) | Some(KarmaToken::Karma(_)) => {
                    let (input, _) = take(1usize)(input)?;
                    cur_input = input;
                },
                _ => {
                    let (input, _) = take_till1(|kt:&KarmaToken|
                        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Karma(_))
                    )(input)?;
                    cur_input = input;
                }
            }
        }

        if cur_input.tok.len() == 0 {
            break;
        }
    }

    Ok((cur_input, ret))
}


#[cfg(test)]
mod test_multi {
    use super::*;

    success_test!(
        test_case_one,
        multi,
        "a++ \"b\"++ [c]++",
        vec![],
        vec![kst!("a", "++"), kst!("b", "++"), kst!("c", "++")]
    );

    success_test!(
        test_case_two,
        multi,
        "a b++ c d++",
        vec![],
        vec![kst!("a b", "++"), kst!("c d", "++")]
    );

    success_test!(
        test_case_three,
        multi,
        "abc \"d\"++ def",
        vec![],
        vec![kst!("d", "++")]
    );

    success_test!(
        test_case_four,
        multi,
        "[a\"b\"c]++",
        vec![],
        vec![kst!("a\"b\"c", "++")]
    );

    success_test!(
        test_case_five,
        multi,
        "[a b]++ c d++ \"e f\"++",
        vec![],
        vec![kst!("a b", "++"), kst!("c d", "++"), kst!("e f", "++")]
    );

    success_test!(
        test_case_six,
        multi,
        "a\"b[c]d++",
        vec![],
        vec![]
    );

    success_test!(
        test_case_seven,
        multi,
        "[a]]b[\"++",
        vec![],
        vec![]
    );

    success_test!(
        test_case_eight,
        multi,
        "",
        vec![],
        vec![]
    );

    success_test!(
        test_case_nine,
        multi,
        "a-b++",
        vec![],
        vec![kst!("a-b", "++")]
    );
}


#[cfg(test)]
mod test_braced {
    use super::*;

    success_test!(test_case_one,   braced, "[a]++",     vec![], kst!("a", "++"));
    success_test!(test_case_two,   braced, "[a b]++",   vec![], kst!("a b", "++"));
    success_test!(test_case_three, braced, "[ a b ]++", vec![], kst!("a b", "++"));
    success_test!(test_case_four,  braced, "[a++]++",   vec![], kst!("a++", "++"));
    success_test!(test_case_five,  braced, "[++]++",    vec![], kst!("++", "++"));

    success_test!(
        test_case_six,
        braced,
        "[a]++ [b]++",
        vec![
            space!(" "),
            KarmaToken::OpenBrace,
            text!("b"),
            KarmaToken::CloseBrace,
            karma!("++")
        ],
        kst!("a", "++")
    );

    fail_test!(
        test_case_seven,
        braced,
        "[]++",
        vec![KarmaToken::CloseBrace, karma!("++")],
        ErrorKind::TakeWhile1
    );

    fail_test!(
        test_case_eight,
        braced,
        "[ ]++",
        vec![KarmaToken::CloseBrace, karma!("++")],
        ErrorKind::Tag
    );

    success_test!(test_case_nine, braced, "[\"\"]++", vec![], kst!("\"\"", "++"));
}


#[cfg(test)]
mod test_quoted {
    use super::*;

    success_test!(test_case_one,   quoted, "\"a\"++",     vec![], kst!("a", "++"));
    success_test!(test_case_two,   quoted, "\"a b\"++",   vec![], kst!("a b", "++"));
    success_test!(test_case_three, quoted, "\" a b \"++", vec![], kst!("a b", "++"));
    success_test!(test_case_four,  quoted, "\"a++\"++",   vec![], kst!("a++", "++"));
    success_test!(test_case_five,  quoted, "\"++\"++",    vec![], kst!("++", "++"));

    success_test!(
        test_case_six,
        quoted,
        "\"a\"++ \"b\"++",
        vec![
            space!(" "),
            KarmaToken::Quote,
            text!("b"),
            KarmaToken::Quote,
            karma!("++")
        ],
        kst!("a", "++")
    );

    fail_test!(
        test_case_seven,
        quoted,
        "\"\"++",
        vec![KarmaToken::Quote, karma!("++")],
        ErrorKind::TakeWhile1
    );

    fail_test!(
        test_case_eight,
        quoted,
        "\" \"++",
        vec![KarmaToken::Quote, karma!("++")],
        ErrorKind::Tag
    );

    success_test!(test_case_nine, quoted, "\"[]\"++", vec![], kst!("[]", "++"));
}


#[cfg(test)]
mod test_simple {
    use super::*;

    success_test!(test_case_one, simple, "a++", vec![], kst!("a", "++"));
    success_test!(test_case_two, simple, "a b++", vec![], kst!("a b", "++"));

    success_test!(
        test_case_three,
        simple,
        "a++ b++",
        vec![
            space!(" "),
            text!("b"),
            karma!("++")
        ],
        kst!("a", "++")
    );

    success_test!(
        test_case_four,
        simple,
        "a b++ c",
        vec![
            space!(" "),
            text!("c"),
        ],
        kst!("a b", "++")
    );

    success_test!(
        test_case_five,
        simple,
        "  a++",
        vec![],
        kst!("a", "++")
    );

    fail_test!(
        test_case_six,
        simple,
        "a b++c",
        vec![text!("c")],
        ErrorKind::Eof
    );

    fail_test!(
        test_case_seven,
        simple,
        "a++b",
        vec![text!("b")],
        ErrorKind::Eof
    );

    fail_test!(
        test_case_eight,
        simple,
        "a b ++",
        vec![karma!("++")],
        ErrorKind::Tag
    );

    fail_test!(
        test_case_nine,
        simple,
        "[\"\"]++",
        vec![
            KarmaToken::OpenBrace,
            KarmaToken::Quote,
            KarmaToken::Quote,
            KarmaToken::CloseBrace,
            karma!("++")
        ],
        ErrorKind::TakeWhile1
    );
}


#[cfg(test)]
mod test_karma_token {
    use super::*;

    #[test]
    fn test_karma() {
        let karma = vec![":++:", "++", "--", "+-", "±"];

        assert_eq!(
            all_token(&karma.join("")),
            Ok(("", karma.iter().map(|k| karma!(k)).collect()))
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
