use nom::{
  IResult,
  bytes::complete::{
      take_while1,
      take_till1,
      take,
  },
  multi::{
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
  error::{
      Error,
      ErrorKind,
  },
};
use std::matches;

use crate::bot::parser::karma_token::all_token;
use crate::bot::parser::karma_token::KarmaToken;
use crate::bot::parser::tokenizer::Tokens;
use crate::bot::parser::karma_def::str_to_karma;


// Now here begins the actual structural karma parser (Karma Structure Tree (KST))
#[derive(Debug, PartialEq)]
pub struct KST(pub String, pub Karma);

#[derive(Debug, PartialEq)]
pub enum Karma {
    Up,
    Down,
    Side
}


pub fn parse(input: &str) -> Result<Vec<KST>, String> {
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


fn kkarma(input: Tokens) -> IResult<Tokens, Karma> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.first() {
        Some(KarmaToken::Karma(t)) => Ok((input, str_to_karma(&t))),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kspace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.first() {
        Some(KarmaToken::Space(t)) => Ok((input, t.to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kquote(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.first() {
        Some(KarmaToken::Quote) => Ok((input, "\"".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kopenbrace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.first() {
        Some(KarmaToken::OpenBrace) => Ok((input, "[".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kclosebrace(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take(1usize)(input)?;

    // Extract this out of the Tokens structure
    match tkt.first() {
        Some(KarmaToken::CloseBrace) => Ok((input, "]".to_string())),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn kenclosedquote(input: Tokens) -> IResult<Tokens, String> {
    let (input, tkt) = take_while1(|kt:&KarmaToken|
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::Karma(_) | &KarmaToken::Quote | &KarmaToken::KText(_))
    )(input)?;

    // Collapse the list into string and trim it
    let temp = tkt.iter().map(
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
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::Karma(_) | &KarmaToken::OpenBrace | &KarmaToken::CloseBrace | &KarmaToken::KText(_))
    )(input)?;

    // Collapse the list into string and trim it
    let temp = tkt.iter().map(
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
        matches!(kt, &KarmaToken::Space(_) | &KarmaToken::Text(_) | &KarmaToken::KText(_))
    )(input)?;

    match (tkt.second_to_last(), tkt.last()) {
        // Validate that last entity isn't a space
        // Text|KText|Space, Text, Karma
        (_, Some(KarmaToken::Space(_))) => {
            Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
        },

        // Do additional check, make sure there is not a space before this token
        // Text|KText|Space, Text, KText, Karma
        (Some(KarmaToken::Space(_)), Some(KarmaToken::KText(_))) => {
            Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
        },
        (None, Some(KarmaToken::KText(_))) => {
            Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
        },

        // Collapse the list into string and trim it
        _ => {
            let temp = tkt.iter().map(
                |k:&KarmaToken| k.to_string()
            ).collect::<Vec<String>>().join("").trim_start().to_string();

            Ok((input, temp))
        },
    }
}




// Simple Karma:
//  1. a++       -> ("",     Karma("a", "++"))
//  2. a b++     -> ("",     Karma("a b", "++"))
//  3. a++ b++   -> (" b++", Karma("a", "++"))
//  4. a b++ c   -> (" c",   Karma("a b", "++"))
//  5.   a++     -> ("",     Karma("a", "++")) - Trim preceeding
//  6. a b++c    -> Invalid
//  7. a++b      -> Invalid
//  8. a b ++    -> Invalid
//  9. [""]++    -> Invalid
// 10. a--++     -> ("",     Karma("a--", "++"))
// 11. --++      -> Invalid
// 12. a --++    -> Invalid
//
// Ground rule for this particular parse:
// 1. Any Text/Space/KText followed by karma
// 2. Karma must follow a Text/KText
// 3. karma must be followed by space/eol
// 4. KText must follow a Text
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
//  1. "a"++       -> ("", Karma("a", "++"))
//  2. "a b"++     -> ("", Karma("a b", "++"))
//  3. " a b "++   -> ("", Karma("a b", "++")) - Trim " a b "
//  4. "a++"++     -> ("", Karma("a++", "++"))
//  5. "++"++      -> ("", Karma("++", "++"))
//  6. "a"++ "b"++ -> (" \"b\"++", Karma("a", "++"))
//  7. ""++        -> Invalid
//  8. " "++       -> Invalid
//  9. "[]"++      -> ("", Karma("[]", "++"))
// 10. "--++"++    -> ("", Karma("--++", "++"))
// 11. "++"--++    -> Invalid
//
// Ground rule for this particular parse:
// 1. Quote followed by any; Karma/Text/Space/KText
// 2. Closing with another Quote
// 3. At least 1 Text in between Quotes
// 4. Closing Quote followed by karma
// 5. Karma followed by whitespace/eol
// 6. KText must not follow Quote
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
//  1. [a]++
//  2. [a b]++
//  3. [ a b ]++
//  4. [a++]++
//  5. [++]++
//  6. [a]++ [b]++
//  7. []++
//  8. [ ]++
//  9. [""]++
// 10. [--++]++
// 11. [++]--++
//
// Ground rule for this particular parse:
// 1. OpenBrace followed by any; Karma/Text/Space/KText
// 2. Closing with CloseBrace
// 3. At least 1 Text in between Open/Close Brace
// 4. CloseBrace followed by karma
// 5. Karma followed by whitespace/eol
// 6. KText must not follow CloseBrace
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
        if !cur_input.is_empty() {
            // If space or karma, drop 1, otherwise eat till space/karma
            match cur_input.first() {
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

        if cur_input.is_empty() {
            break;
        }
    }

    Ok((cur_input, ret))
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
        KST($data.to_string(), $karma)
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

#[cfg(test)]
mod test_multi {
    use super::*;

    success_test!(
        test_case_one,
        multi,
        "a++ \"b\"++ [c]++",
        vec![],
        vec![kst!("a", Karma::Up), kst!("b", Karma::Up), kst!("c", Karma::Up)]
    );

    success_test!(
        test_case_two,
        multi,
        "a b++ c d++",
        vec![],
        vec![kst!("a b", Karma::Up), kst!("c d", Karma::Up)]
    );

    success_test!(
        test_case_three,
        multi,
        "abc \"d\"++ def",
        vec![],
        vec![kst!("d", Karma::Up)]
    );

    success_test!(
        test_case_four,
        multi,
        "[a\"b\"c]++",
        vec![],
        vec![kst!("a\"b\"c", Karma::Up)]
    );

    success_test!(
        test_case_five,
        multi,
        "[a b]++ c d++ \"e f\"++",
        vec![],
        vec![kst!("a b", Karma::Up), kst!("c d", Karma::Up), kst!("e f", Karma::Up)]
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
        vec![kst!("a-b", Karma::Up)]
    );
}


#[cfg(test)]
mod test_braced {
    use super::*;

    success_test!(test_case_one,   braced, "[a]++",     vec![], kst!("a", Karma::Up));
    success_test!(test_case_two,   braced, "[a b]++",   vec![], kst!("a b", Karma::Up));
    success_test!(test_case_three, braced, "[ a b ]++", vec![], kst!("a b", Karma::Up));
    success_test!(test_case_four,  braced, "[a++]++",   vec![], kst!("a++", Karma::Up));
    success_test!(test_case_five,  braced, "[++]++",    vec![], kst!("++", Karma::Up));

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
        kst!("a", Karma::Up)
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

    success_test!(test_case_nine, braced, "[\"\"]++", vec![], kst!("\"\"", Karma::Up));
    success_test!(test_case_ten,  braced, "[--++]++",  vec![], kst!("--++", Karma::Up));

    fail_test!(
        test_case_eleven,
        braced,
        "[++]--++",
        vec![karma!("++")],
        ErrorKind::Tag
    );
}


#[cfg(test)]
mod test_quoted {
    use super::*;

    success_test!(test_case_one,   quoted, "\"a\"++",     vec![], kst!("a", Karma::Up));
    success_test!(test_case_two,   quoted, "\"a b\"++",   vec![], kst!("a b", Karma::Up));
    success_test!(test_case_three, quoted, "\" a b \"++", vec![], kst!("a b", Karma::Up));
    success_test!(test_case_four,  quoted, "\"a++\"++",   vec![], kst!("a++", Karma::Up));
    success_test!(test_case_five,  quoted, "\"++\"++",    vec![], kst!("++", Karma::Up));

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
        kst!("a", Karma::Up)
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

    success_test!(test_case_nine, quoted, "\"[]\"++",   vec![], kst!("[]", Karma::Up));
    success_test!(test_case_ten,  quoted, "\"--++\"++", vec![], kst!("--++", Karma::Up));

    fail_test!(
        test_case_eleven,
        quoted,
        "\"++\"--++",
        vec![karma!("++")],
        ErrorKind::Tag
    );
}


#[cfg(test)]
mod test_simple {
    use super::*;

    success_test!(test_case_one, simple, "a++", vec![], kst!("a", Karma::Up));
    success_test!(test_case_two, simple, "a b++", vec![], kst!("a b", Karma::Up));

    success_test!(
        test_case_three,
        simple,
        "a++ b++",
        vec![
            space!(" "),
            text!("b"),
            karma!("++")
        ],
        kst!("a", Karma::Up)
    );

    success_test!(
        test_case_four,
        simple,
        "a b++ c",
        vec![
            space!(" "),
            text!("c"),
        ],
        kst!("a b", Karma::Up)
    );

    success_test!(
        test_case_five,
        simple,
        "  a++",
        vec![],
        kst!("a", Karma::Up)
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

    success_test!(test_case_ten, simple, "a--++", vec![], kst!("a--", Karma::Up));

    fail_test!(
        test_case_eleven,
        simple,
        "--++",
        vec![karma!("++")],
        ErrorKind::Tag
    );

    fail_test!(
        test_case_twelve,
        simple,
        "a --++",
        vec![karma!("++")],
        ErrorKind::Tag
    );
}
