use nom::{
  IResult,
  bytes::complete::{
      take_till,
      tag,
  },
  multi::{
      many1,
  },
  combinator::{
      complete,
      map,
      peek,
      opt,
  },
  branch::alt,
  sequence::{
      delimited,
      preceded,
      separated_pair,
      tuple,
  },
  error::{
      Error,
      ErrorKind,
  },
};
use std::fmt;
use log::{trace, debug, info, warn, error};


#[derive(Debug, PartialEq)]
pub enum Segment<'a> {
    Text(&'a str),
    Channel(&'a str, &'a str),
    User(&'a str, &'a str),
    Group(&'a str, &'a str),
    At(AtType, &'a str),
    Link(&'a str, &'a str),
    Open,

    // Timestamp, Formatter, Url, Label
    Date(&'a str, &'a str, Option<&'a str>, &'a str),
}

#[derive(Debug, PartialEq)]
pub enum AtType {
    Here,
    Channel,
    Everyone,
}

// This is specifically for parsing karma and stowing to the database
impl <'a> fmt::Display for Segment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Segment::Text(t)                 => write!(f, "{}", t),
            Segment::Channel(cid, "")        => write!(f, "#{}", cid),
            Segment::Channel(_, l)           => write!(f, "{}", l),
            Segment::User(uid, "")           => write!(f, "@{}", uid),
            Segment::User(_, l)              => write!(f, "{}", l),
            Segment::Group(gid, "")          => write!(f, "@{}", gid),
            Segment::Group(_, l)             => write!(f, "{}", l),
            Segment::At(AtType::Here, _)     => write!(f, "@here"),
            Segment::At(AtType::Channel, _)  => write!(f, "@channel"),
            Segment::At(AtType::Everyone, _) => write!(f, "@everyone"),
            Segment::Link(url, "")           => write!(f, "{}", url),
            Segment::Link(_, l)              => write!(f, "{}", l),
            Segment::Open                    => write!(f, "<"),
            Segment::Date(_, _, _, l)        => write!(f, "{}", l),
        }
    }
}


pub fn parse(input: &str) -> Result<Vec<Segment>, String> {
    let cmd = complete(many1(segment))(input);

    match cmd {
        Err(x)       => Err(format!("{:?}", x)),
        Ok((_, res)) => Ok(res),
    }
}

fn segment(input: &str) -> IResult<&str, Segment> {
    alt((
        special,
        text,
        // Fallback
        map(tag("<"), |_| Segment::Open),
    ))(input)
}

fn text(input: &str) -> IResult<&str, Segment> {
    let (input, content) = take_till(|c:char| c == '<')(input)?;

    if content.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        Ok((input, Segment::Text(content)))
    }
}

fn special(input: &str) -> IResult<&str, Segment> {
    delimited(
        tag("<"),
        alt((
            channel,
            user,
            group,
            mention,
            date,
            link,
        )),
        tag(">"),
    )(input)
}

fn channel(input: &str) -> IResult<&str, Segment> {
    preceded(
        peek(tag("#C")),
        preceded(
            tag("#"),
            map(content, |(c, l)| Segment::Channel(c, l))
        )
    )(input)
}

fn user(input: &str) -> IResult<&str, Segment> {
    preceded(
        peek(alt((
            tag("@U"), tag("@W")
        ))),
        preceded(
            tag("@"),
            map(content, |(u, l)| Segment::User(u, l))
        )
    )(input)
}

fn group(input: &str) -> IResult<&str, Segment> {
    preceded(
        tag("!subteam^"),
        map(content, |(g, l)| Segment::Group(g, l))
    )(input)
}

fn mention(input: &str) -> IResult<&str, Segment> {
    preceded(
        tag("!"),
        alt((
            map(
                separated_pair(mention_type, tag("|"), take_till(|c:char| c == '>')),
                |(t, l)| Segment::At(t, l)
            ),
            map(mention_type, |t| Segment::At(t, "")),
        ))
    )(input)
}

fn mention_type(input: &str) -> IResult<&str, AtType> {
    alt((
        map(tag("here"), |_| AtType::Here),
        map(tag("channel"), |_| AtType::Channel),
        map(tag("everyone"), |_| AtType::Everyone),
    ))(input)
}

fn link(input: &str) -> IResult<&str, Segment> {
    map(content, |(u, l)| Segment::Link(u, l))(input)
}

fn content(input: &str) -> IResult<&str, (&str, &str)> {
    alt((
        separated_pair(
            take_till(|c:char| c == '|'),
            tag("|"),
            take_till(|c:char| c == '>'),
        ),
        map(take_till(|c:char| c == '>'), |s:&str| (s, "")),
    ))(input)
}

fn date(input: &str) -> IResult<&str, Segment> {
    // <!date^123123^{text} asdf[^link]|fallback>
    preceded(
        tag("!date"),
        map(tuple((
            preceded(tag("^"), take_till(|c:char| c == '^')),
            preceded(tag("^"), take_till(|c:char| c == '^' || c == '|')),
            opt(preceded(tag("^"), take_till(|c:char| c == '|'))),
            preceded(tag("|"), take_till(|c:char| c == '>')),
        )), |(timestamp, format, link, fallback)| Segment::Date(timestamp, format, link, fallback))
    )(input)
}


#[derive(Debug, PartialEq)]
enum SegmentLite<'a> {
    Text(&'a str),
    At(AtType),
    Open,
}

// This is specifically for parsing any output and santizing it
impl <'a> fmt::Display for SegmentLite<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SegmentLite::Text(t)              => write!(f, "{}", t),
            SegmentLite::At(AtType::Here)     => write!(f, "@here"),
            SegmentLite::At(AtType::Channel)  => write!(f, "@channel"),
            SegmentLite::At(AtType::Everyone) => write!(f, "@everyone"),
            SegmentLite::Open                 => write!(f, "<"),
        }
    }
}

pub fn santize_output(input: &str) -> String {
    let res = complete(many1(segment_lite))(input);

    res.map(
        |(_, i)| i.iter().map(|i| i.to_string()).collect::<Vec<String>>().join("")
    ).unwrap_or(
        input.to_string()
    )
}

fn segment_lite(input: &str) -> IResult<&str, SegmentLite> {
    alt((
        delimited(tag("<"), mention_lite, tag(">")),
        text_lite,
        // Fallback
        map(tag("<"), |_| SegmentLite::Open),
    ))(input)
}

fn text_lite(input: &str) -> IResult<&str, SegmentLite> {
    let (input, content) = take_till(|c:char| c == '<')(input)?;

    if content.is_empty() {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Eof)))
    } else {
        Ok((input, SegmentLite::Text(content)))
    }
}

fn mention_lite(input: &str) -> IResult<&str, SegmentLite> {
    preceded(
        tag("!"),
        alt((
            map(
                separated_pair(mention_type, tag("|"), take_till(|c:char| c == '>')),
                |(t, _)| SegmentLite::At(t)
            ),
            map(mention_type, |t| SegmentLite::At(t)),
        ))
    )(input)
}


#[cfg(test)]
mod test_segment {
    use super::*;

    #[test]
    fn test_plain_text() {
        assert_eq!(
            segment("as dfas <"),
            Ok(("<", Segment::Text("as dfas ")))
        );
    }

    #[test]
    fn test_plain_text_eof() {
        assert_eq!(
            segment("as dfas "),
            Ok(("", Segment::Text("as dfas ")))
        );
    }

    #[test]
    fn test_plain_text_fallback() {
        assert_eq!(
            segment("<!here"),
            Ok(("!here", Segment::Open))
        );
    }

    #[test]
    fn test_gibbish_fallback() {
        assert_eq!(
            segment("<gibbish>"),
            Ok(("", Segment::Link("gibbish", "")))
        );
    }


    #[test]
    fn test_unlabeled_channel() {
        assert_eq!(
            segment("<#CASDF> text"),
            Ok((" text", Segment::Channel("CASDF", "")))
        );
    }

    #[test]
    fn test_channel() {
        assert_eq!(
            segment("<#CASDF|lol> text"),
            Ok((" text", Segment::Channel("CASDF", "lol")))
        );
    }

    #[test]
    fn test_unlabeled_user_one() {
        assert_eq!(
            segment("<@UASDF> text"),
            Ok((" text", Segment::User("UASDF", "")))
        );
    }

    #[test]
    fn test_user_one() {
        assert_eq!(
            segment("<@UASDF|rose> text"),
            Ok((" text", Segment::User("UASDF", "rose")))
        );
    }

    #[test]
    fn test_unlabeled_user_two() {
        assert_eq!(
            segment("<@WASDF> text"),
            Ok((" text", Segment::User("WASDF", "")))
        );
    }

    #[test]
    fn test_user_two() {
        assert_eq!(
            segment("<@WASDF|daisy> text"),
            Ok((" text", Segment::User("WASDF", "daisy")))
        );
    }

    #[test]
    fn test_unlabeled_group() {
        assert_eq!(
            segment("<!subteam^WASDF> text"),
            Ok((" text", Segment::Group("WASDF", "")))
        );
    }

    #[test]
    fn test_group() {
        assert_eq!(
            segment("<!subteam^WASDF|soylent> text"),
            Ok((" text", Segment::Group("WASDF", "soylent")))
        );
    }

    #[test]
    fn test_unlabeled_at_here() {
        assert_eq!(
            segment("<!here> text"),
            Ok((" text", Segment::At(AtType::Here, "")))
        );
    }

    #[test]
    fn test_at_here() {
        assert_eq!(
            segment("<!here|here> text"),
            Ok((" text", Segment::At(AtType::Here, "here")))
        );
    }

    #[test]
    fn test_unlabeled_at_channel() {
        assert_eq!(
            segment("<!channel> text"),
            Ok((" text", Segment::At(AtType::Channel, "")))
        );
    }

    #[test]
    fn test_at_channel() {
        assert_eq!(
            segment("<!channel|ye> text"),
            Ok((" text", Segment::At(AtType::Channel, "ye")))
        );
    }

    #[test]
    fn test_unlabeled_at_everyone() {
        assert_eq!(
            segment("<!everyone> text"),
            Ok((" text", Segment::At(AtType::Everyone, "")))
        );
    }

    #[test]
    fn test_at_everyone() {
        assert_eq!(
            segment("<!everyone|hi> text"),
            Ok((" text", Segment::At(AtType::Everyone, "hi")))
        );
    }

    #[test]
    fn test_unlabeled_link() {
        assert_eq!(
            segment("<http://www.google.com> text"),
            Ok((" text", Segment::Link("http://www.google.com", "")))
        );
    }

    #[test]
    fn test_link() {
        assert_eq!(
            segment("<http://www.google.com|gog> text"),
            Ok((" text", Segment::Link("http://www.google.com", "gog")))
        );
    }

    #[test]
    fn test_basic_date() {
        assert_eq!(
            segment("<!date^12345^{text}|timedate> text"),
            Ok((" text", Segment::Date("12345", "{text}", None, "timedate")))
        );
    }

    #[test]
    fn test_url_date() {
        assert_eq!(
            segment("<!date^12345^{text}^http://google.com|timedate> text"),
            Ok((" text", Segment::Date("12345", "{text}", Some("http://google.com"), "timedate")))
        );
    }

    #[test]
    fn test_multiples() {
        assert_eq!(
            parse("<!here> for <#CASDF|Weebs> text"),
            Ok(vec![
               Segment::At(AtType::Here, ""),
               Segment::Text(" for "),
               Segment::Channel("CASDF", "Weebs"),
               Segment::Text(" text")
            ])
        );
    }

    #[test]
    fn test_santized_output() {
        assert_eq!(
            parse("<!here> for <#CASDF|Weebs> text").map(
                |i| i.iter().map(|i| i.to_string()).collect::<Vec<String>>().join("")
            ),
            Ok("@here for Weebs text".to_string())
        );
    }

    #[test]
    fn test_plain_text_fallback_one() {
        assert_eq!(
            parse("<!here <!here> bad"),
            Ok(vec![
               Segment::Open,
               Segment::Text("!here "),
               Segment::At(AtType::Here, ""),
               Segment::Text(" bad")
            ])
        );
    }

    #[test]
    fn test_plain_text_fallback_two() {
        assert_eq!(
            parse(">!here <!here>> bad"),
            Ok(vec![
               Segment::Text(">!here "),
               Segment::At(AtType::Here, ""),
               Segment::Text("> bad")
            ])
        );
    }

    #[test]
    fn test_safe_output() {
        assert_eq!(
            santize_output(">!here <!here bad"),
            ">!here <!here bad".to_string()
        );
    }

    #[test]
    fn test_unsafe_output() {
        assert_eq!(
            santize_output("<!channel> <!here> <!everyone> hi"),
            "@channel @here @everyone hi".to_string()
        );
    }
}
