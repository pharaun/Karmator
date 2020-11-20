use nom::{
  IResult,
  bytes::complete::{
      tag,
      take_while1,
  },
  multi::{
      separated_list0,
      many0,
  },
  combinator::map,
  branch::alt,
  sequence::delimited,
  character::complete::{
      multispace1,
      multispace0,
  },
};
use std::fmt;


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


// Now here begins the actual structural karma parser



#[cfg(test)]
mod test_structural_karma {
    use super::*;

    #[test]
    fn test_karma() {
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
