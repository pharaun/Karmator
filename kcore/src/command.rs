use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{multispace0, multispace1},
    combinator::complete,
    multi::separated_list0,
    sequence::delimited,
    IResult,
};

// TODO: add in specific support for parsing at-here and other special <!user_id> entities
// so that in the message subsystem it can do the right thing
#[derive(Debug, PartialEq)]
pub struct Command<'a>(pub &'a str, pub Vec<&'a str>);

pub fn parse(input: &str) -> Result<Command, String> {
    let cmd = complete(command)(input);

    match cmd {
        Err(x) => Err(format!("{:?}", x)),
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
    take_while1(|c: char| c.is_alphanumeric())(input)
}

fn not_multispace1(input: &str) -> IResult<&str, &str> {
    alt((
        delimited(tag("\""), take_while1(|c: char| c != '"'), tag("\"")),
        delimited(tag("“"), take_while1(|c: char| c != '”'), tag("”")),
        delimited(tag("["), take_while1(|c: char| c != ']'), tag("]")),
        take_while1(|c: char| !c.is_whitespace()),
    ))(input)
}

fn args(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(multispace1, not_multispace1)(input)
}

#[cfg(test)]
mod test_command {
    use super::*;

    #[test]
    fn test_command() {
        assert_eq!(command("!karma"), Ok(("", Command("karma", vec![]))));
    }

    #[test]
    fn test_command_one_arg() {
        assert_eq!(command("!karma a"), Ok(("", Command("karma", vec!["a"]))));
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

    #[test]
    fn test_command_smart_quote() {
        assert_eq!(
            command("!karma “:yey: dod”"),
            Ok(("", Command("karma", vec![":yey: dod"])))
        );
    }

    #[test]
    fn test_command_braces() {
        assert_eq!(
            command("!karma [:yey: dod]"),
            Ok(("", Command("karma", vec![":yey: dod"])))
        );
    }

    #[test]
    fn test_nested_command() {
        assert_eq!(
            command("!karma !karma asdf"),
            Ok(("", Command("karma", vec!["!karma", "asdf"])))
        );
    }

    #[test]
    fn test_nested_quoted_command() {
        assert_eq!(
            command("!karma \"!karma asdf\""),
            Ok(("", Command("karma", vec!["!karma asdf"])))
        );
    }
}
