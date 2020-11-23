use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
      take_till1,
      take_till,
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
enum KST {
    Karma(String, String),
}


fn structural_karma(input: Tokens) -> IResult<Tokens, KST> {
    Ok((input, KST::Karma("str".to_string(), "++".to_string())))
}


fn ktext(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.tok.get(0) {
        Some(KarmaToken::Text(t)) => Ok((input, t.to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
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

macro_rules! kst {
    ($data:expr, $karma:expr) => {
        KST::Karma($data.to_string(), $karma.to_string())
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
        |(t, k)| KST::Karma(t, k)
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
        |(t, k)| KST::Karma(t, k)
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
        |(t, k)| KST::Karma(t, k)
    )(input)
}


// Hail Mary parse
// 1. a"b[c]d++   -> ("", Karma("a\"b[c]d", "++"))
// 2. "[a]]b["++  -> ("", Karma("\"[a]]b[\"", "++"))
// 3. a["++ "]b++ -> (" \"]b++", Karma("a[\"", "++"))
// 4. asdf ++     -> Invalid
// 5. asdf++bas   -> Invalid
// 6. ++          -> Invalid
//
// Ground rule for this particular parse:
// 1. Anything to Karma
// 2. Karma must not be preeced by a space
// 3. Karma must not be preeced by the start of the line
// 4. Karma must be followed by space/eol



// TODO: develop invalid cases to test extent of the parser
// MultiKarma (K(x) == Karma(x, "++"))
// 1. a++ "b"++ [c]++       -> ("", [K("a"), K("b"), K("c")])
// 2. a b++ c d++           -> ("", [K("a b"), K("c d")])
// 3. abc "d"++ def         -> ("", [K("d")])
// 4. [a"b"c]++             -> ("", [K("a\"b\"c")])
// 5. [a b]++ c d++ "e f"++ -> ("", [K("a b"), K("c d"), K("e f")])
// 6. a"b[c]d++             -> Hail mary (Separate parse attempt)
// 7. "[a]]b["++            -> Hail mary (Separate parse attempt)
//
// Ground rule for this particular parse:
// 1. Apply simple combinator as many time as possible
// 2. Apply quote combinator as many time as possible
// 3. Apply brace combinator as many time as possible
// 4. If still more, discard 1 token and go to 1
// 5. If no result, do a hail mary pass? (TODO)
fn multi(input: Tokens) -> IResult<Tokens, Vec<KST>> {
    let mut ret = vec![];
    let mut cur_input = input;

    let mut i = 0;

    loop {
        // Debugging
        i += 1;
        println!("=========");
        println!("iter = {}", i);
        println!("ret  = {:?}", ret);

        // 1. Apply simple combinator as many times as possible
        let (input, res) = many0(simple)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        println!("i1s: {:?}", cur_input);

        // 2. Apply quote combinator as many time as possible
        let (input, res) = many0(quoted)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        println!("i1q: {:?}", cur_input);

        // 3. Apply brace combinator as many time as possible
        let (input, res) = many0(braced)(cur_input)?;
        cur_input = input;
        ret.extend(res);

        println!("i1b: {:?}", cur_input);

        // 4. If still more, discard 1 token and go to 1
        if cur_input.tok.len() != 0 {
            let (input, _) = take(1usize)(input)?;
            cur_input = input;

            println!("i2t: {:?}", cur_input);
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

    // Hail Mary pass
    success_test!(
        test_case_six,
        multi,
        "a\"b[c]d++",
        vec![],
        vec![kst!("a\"b[c]d", "++")]
    );

    success_test!(
        test_case_seven,
        multi,
        "a\"b[c]d++",
        vec![],
        vec![kst!("a\"b[c]d", "++")]
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
            KarmaToken::Space(" "),
            KarmaToken::OpenBrace,
            KarmaToken::Text("b"),
            KarmaToken::CloseBrace,
            KarmaToken::Karma("++")
        ],
        kst!("a", "++")
    );

    fail_test!(
        test_case_seven,
        braced,
        "[]++",
        vec![KarmaToken::CloseBrace, KarmaToken::Karma("++")],
        ErrorKind::TakeWhile1
    );

    fail_test!(
        test_case_eight,
        braced,
        "[ ]++",
        vec![KarmaToken::CloseBrace, KarmaToken::Karma("++")],
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
            KarmaToken::Space(" "),
            KarmaToken::Quote,
            KarmaToken::Text("b"),
            KarmaToken::Quote,
            KarmaToken::Karma("++")
        ],
        kst!("a", "++")
    );

    fail_test!(
        test_case_seven,
        quoted,
        "\"\"++",
        vec![KarmaToken::Quote, KarmaToken::Karma("++")],
        ErrorKind::TakeWhile1
    );

    fail_test!(
        test_case_eight,
        quoted,
        "\" \"++",
        vec![KarmaToken::Quote, KarmaToken::Karma("++")],
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
            KarmaToken::Space(" "),
            KarmaToken::Text("b"),
            KarmaToken::Karma("++")
        ],
        kst!("a", "++")
    );

    success_test!(
        test_case_four,
        simple,
        "a b++ c",
        vec![
            KarmaToken::Space(" "),
            KarmaToken::Text("c"),
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
        vec![KarmaToken::Text("c")],
        ErrorKind::Eof
    );

    fail_test!(
        test_case_seven,
        simple,
        "a++b",
        vec![KarmaToken::Text("b")],
        ErrorKind::Eof
    );

    fail_test!(
        test_case_eight,
        simple,
        "a b ++",
        vec![KarmaToken::Karma("++")],
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
            KarmaToken::Karma("++")
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
