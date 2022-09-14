use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;

use chrono::prelude::{Utc, DateTime, NaiveTime};
use chrono_tz::Tz;
use chrono_tz::OffsetComponents;

use crate::core::cache;

use crate::core::event::MsgId;
use crate::core::event::send_simple_message;

use nom::{
  IResult,
  bytes::complete::{
      tag,
      tag_no_case,
      take_while1,
      is_a,
  },
  character::complete::{
      multispace0,
      digit1,
  },
  combinator::{
      map_res,
      opt,
      map,
      recognize,
      fail,
  },
  branch::alt,
  sequence::{
      preceded,
      delimited,
      separated_pair,
  },
  multi::{
      many0,
  },
  error::{
      Error,
      ErrorKind,
  },
};


const TIME_FORMAT: &str = "%l:%M%P";


pub async fn timezone<R>(
    msg_id: MsgId,
    tx: &mut mpsc::Sender<tungstenite::tungstenite::Message>,
    cache: &cache::Cache<R>,
    channel_id: String,
    thread_ts: Option<String>,
    user_id: String,
    input: Vec<&str>,
)
where
    R: slack::requests::SlackWebRequestSender + std::clone::Clone
{
    // TODO: should check if its within reasonable hours and if its borderline
    // color it yellow, or bad, red, otherwise green.
    if input.is_empty() {
        // !tz --> utc current time + 3 tz (pacific, eastern, uk)
        let utc_time: DateTime<Utc> = Utc::now();
        let pacific = utc_time.with_timezone(&chrono_tz::America::Los_Angeles);
        let eastern = utc_time.with_timezone(&chrono_tz::America::Toronto);
        let london  = utc_time.with_timezone(&chrono_tz::Europe::London);

        let _ = send_simple_message(
            msg_id,
            tx,
            channel_id,
            thread_ts,
            format!(
                "<@{}>: Pacific: {}, Eastern: {}, UK: {}",
                user_id,
                pacific.format(TIME_FORMAT),
                eastern.format(TIME_FORMAT),
                london.format(TIME_FORMAT),
            ),
        ).await;

    } else {

        let user_tz = cache.get_user_tz(&user_id).await.unwrap();

        // Parse their TZ and compare it to the offset
        let tz: Tz = user_tz.tz.parse().unwrap();


        // Convert to this TZ from UTC then ask for offset.
        let utc_time: DateTime<Utc> = Utc::now();
        let tz_time = utc_time.with_timezone(&tz);
        let tz_offset = tz_time.offset();
        let tz_offset2 = tz_offset.base_utc_offset();
        let tz_offset3 = tz_offset.dst_offset();
        let tz_offset4 = tz_offset2 + tz_offset3;


        let _ = send_simple_message(
            msg_id.clone(),
            tx,
            channel_id.clone(),
            thread_ts.clone(),
            format!(
                "<@{}>: Slack Tz: {:?}, Slack Offset: {:?}, utc: {:?}, tz_time: {:?}",
                user_id, user_tz.tz, user_tz.offset, utc_time, tz_time),
        ).await;

        let _ = send_simple_message(
            msg_id,
            tx,
            channel_id,
            thread_ts,
            format!(
                "<@{}>: Slack Offset: {:?}, base offset: {:?}, dst offset: {:?} actual offset: {:?}",
                user_id, user_tz.offset, tz_offset2, tz_offset3, tz_offset4),
        ).await;


        // Parser stuff here
        // 24hr
        //  !tz 0200
        //  !tz 02:00
        //
        // 12hr
        //  !tz 2pm
        //  !tz 2 am
        //  !tz 2:00PM
        //  !tz 2:00AM
        //  !tz [AaPp][Mm] for am/pm
        //
        // timezone (optional) (mandatory space) - if not specified TZ is the slack's
        //  !tz [time]am/pm pdt
        //  !tz [time]am/pm PST
        //  !tz [time]est
        //  !tz pdt/pst, est/edt, ???(uk)
        // optional?: pacific, eastern, london
        let input = input.join(" ");



        println!("{:?}", input);
    }
}

#[derive(Debug, PartialEq)]
struct TzReq {
    time: NaiveTime,
    zone: Option<Tz>
}

fn parse(input: &str) -> IResult<&str, TzReq> {
    let (input, time) = alt((
        twelve,
        twenty_four,
    ))(input)?;
    let (input, tz) = preceded(multispace0, opt(tz_abbv))(input)?;

    Ok((input, TzReq {
        time: time,
        zone: tz,
    }))
}


// Note: Nonstandard timezones, for user convience only:
// PST, PDT (standard/Daylight(summer)) - pacific
// EST, EDT (standard/Daylight(summer)) - eastern
// BST, GMT (summer time/gmt) - london (GMT != UTC)
fn tz_abbv(input: &str) -> IResult<&str, Tz> {
    alt((
        map(tag_no_case("PST"), |_| chrono_tz::Etc::GMTMinus8),
        map(tag_no_case("PDT"), |_| chrono_tz::Etc::GMTMinus7),
        map(tag_no_case("EST"), |_| chrono_tz::Etc::GMTMinus5),
        map(tag_no_case("EDT"), |_| chrono_tz::Etc::GMTMinus4),
        map(tag_no_case("BST"), |_| chrono_tz::Etc::GMTPlus1),
        map(tag_no_case("GMT"), |_| chrono_tz::Etc::GMTPlus0),
    ))(input)
}

fn meridiem(input: &str) -> IResult<&str, &str> {
    alt((
        map(tag_no_case("am"), |_| "AM"),
        map(tag_no_case("pm"), |_| "PM"),
    ))(input)
}

fn time(input: &str) -> IResult<&str, &str> {
    alt((
        recognize(delimited(digit1, tag(":"), digit1)),
        digit1,
    ))(input)
}

fn twelve(input: &str) -> IResult<&str, NaiveTime> {
    let (input, (time, meri)) = separated_pair(
        time,
        multispace0,
        meridiem
    )(input)?;

    let time_str = if time.contains(":") {
        format!("{}{}", time, meri)
    } else {
        format!("{}:00{}", time, meri)
    };

    match NaiveTime::parse_from_str(&time_str, "%-I:%M%p") {
        Ok(nt) => Ok((input, nt)),
        Err(e) => Err(nom::Err::Error(Error::new(input, ErrorKind::Fail))),
    }
}

fn twenty_four(input: &str) -> IResult<&str, NaiveTime> {
    let (input, time) = map(time, |t| t.replace(":", ""))(input)?;

    match NaiveTime::parse_from_str(&time, "%H%M") {
        Ok(nt) => Ok((input, nt)),
        Err(e) => Err(nom::Err::Error(Error::new(input, ErrorKind::Fail))),
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_meridiem() {
        assert_eq!(
            meridiem("AM"),
            Ok(("", "AM"))
        );

        assert_eq!(
            meridiem("pm"),
            Ok(("", "PM"))
        );
    }

    #[test]
    fn test_time() {
        assert_eq!(
            time("0200"),
            Ok(("", "0200"))
        );
        assert_eq!(
            time("1700"),
            Ok(("", "1700"))
        );
        assert_eq!(
            time("2"),
            Ok(("", "2"))
        );
        assert_eq!(
            time("12"),
            Ok(("", "12"))
        );
        assert_eq!(
            time("3:03"),
            Ok(("", "3:03"))
        );
        assert_eq!(
            time("12:11"),
            Ok(("", "12:11"))
        );
    }

    #[test]
    fn test_twelve() {
        assert_eq!(
            twelve("1pm"),
            Ok(("", NaiveTime::from_hms(13, 0, 0)))
        );
        assert_eq!(
            twelve("5 AM"),
            Ok(("", NaiveTime::from_hms(5, 0, 0)))
        );
        assert_eq!(
            twelve("5:10pm"),
            Ok(("", NaiveTime::from_hms(17, 10, 0)))
        );
        assert_eq!(
            twelve("11:20 am"),
            Ok(("", NaiveTime::from_hms(11, 20, 0)))
        );
    }

    #[test]
    fn test_twenty_four() {
        assert_eq!(
            twenty_four("0220"),
            Ok(("", NaiveTime::from_hms(2, 20, 0)))
        );
        assert_eq!(
            twenty_four("17:10"),
            Ok(("", NaiveTime::from_hms(17, 10, 0)))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("2pm"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms(14, 00, 0),
                zone: None,
            }))
        );
        assert_eq!(
            parse("5:30 pm pst"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms(17, 30, 0),
                zone: Some(chrono_tz::Etc::GMTMinus8),
            }))
        );
        assert_eq!(
            parse("0801 GMT"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms(8, 1, 0),
                zone: Some(chrono_tz::Etc::GMTPlus0),
            }))
        );
    }
}
