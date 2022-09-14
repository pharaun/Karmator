use slack_api as slack;
use tokio_tungstenite as tungstenite;

use tokio::sync::mpsc;

use chrono::prelude::{Utc, DateTime, NaiveTime};
use chrono_tz::Tz;
use chrono_tz::OffsetComponents;
use chrono::NaiveDateTime;

use crate::core::cache;

use crate::core::event::MsgId;
use crate::core::event::send_simple_message;

use nom::{
  IResult,
  bytes::complete::{
      tag,
      tag_no_case,
  },
  character::complete::{
      multispace0,
      digit1,
  },
  combinator::{
      opt,
      map,
      recognize,
  },
  branch::alt,
  sequence::{
      preceded,
      delimited,
      separated_pair,
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
        let input = input.join(" ");
        match parse(&input) {
            Err(e) => {
                eprintln!("ERROR [TZ]: {:?}", e);

                let message = format!(
                    "<@{}>: Usage: `!tz TIME [TZ]`
                    24hr: `!tz 05:00 [TZ]` or `!tz 0500 [TZ]`
                    12hr: `!tz 2:00pm [TZ]` or `!tz 5pm [TZ]`
                    [TZ]: (optional) `PST/PDT`, `EST/EDT`, `BST/GMT`
                    [TZ]: (note) if not specified, will use your slack Timezone",
                    user_id,
                );
                let _ = send_simple_message(
                    msg_id,
                    tx,
                    channel_id,
                    thread_ts,
                    message,
                ).await;
            },
            Ok((_, TzReq { time: nt, zone: None })) => {
                let user_tz = cache.get_user_tz(&user_id).await.unwrap();
                let tz: Tz = user_tz.tz.parse().unwrap();

                // Validate the offset
                validate_offset(&tz, user_tz.offset);

                // Convert time
                let given_datetime_tz = convert_naivetime(nt, tz);

                // Supported locals
                let out_tz = convert_to_locales(given_datetime_tz);

                let _ = send_simple_message(
                    msg_id,
                    tx,
                    channel_id,
                    thread_ts,
                    format!(
                        "<@{}>: Pacific: {}, Eastern: {}, UK: {}",
                        user_id,
                        out_tz.pacific.format(TIME_FORMAT),
                        out_tz.eastern.format(TIME_FORMAT),
                        out_tz.london.format(TIME_FORMAT),
                    ),
                ).await;
            },
            Ok((_, TzReq { time: nt, zone: Some(tz_abbv) })) => {
                println!("lol");
            },
        }
    }
}


fn validate_offset(tz: &Tz, offset: i64) {
    // Validate and compare the offset
    let utc_time: DateTime<Utc> = Utc::now();
    let tz_time = utc_time.with_timezone(tz);
    let tz_offset = tz_time.offset().base_utc_offset() + tz_time.offset().dst_offset();

    if tz_offset.num_seconds() != offset {
        eprintln!(
            "ERROR [TZ]: Slack offset {}, parsed offset {} does not match!",
            offset,
            tz_offset.num_seconds(),
        );
    }
}


fn convert_naivetime(nt: NaiveTime, tz: Tz) -> DateTime<Tz> {
    // Convert the given naive time to the given Tz
    let utc_time: DateTime<Utc> = Utc::now();
    let tz_time = utc_time.with_timezone(&tz);
    let naive_datetime = tz_time.naive_local();
    let naive_date = naive_datetime.date();
    let given_datetime = NaiveDateTime::new(naive_date, nt);
    let given_datetime_tz = given_datetime.and_local_timezone(tz).single().unwrap();

    given_datetime_tz
}

struct OutTz {
    pacific: DateTime<Tz>,
    eastern: DateTime<Tz>,
    london: DateTime<Tz>,
}

fn convert_to_locales(dt: DateTime<Tz>) -> OutTz {
    OutTz {
        pacific: dt.with_timezone(&chrono_tz::America::Los_Angeles),
        eastern: dt.with_timezone(&chrono_tz::America::Toronto),
        london:  dt.with_timezone(&chrono_tz::Europe::London),
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
        Err(_) => Err(nom::Err::Error(Error::new(input, ErrorKind::Fail))),
    }
}

fn twenty_four(input: &str) -> IResult<&str, NaiveTime> {
    let (input, time) = map(time, |t| t.replace(":", ""))(input)?;

    match NaiveTime::parse_from_str(&time, "%H%M") {
        Ok(nt) => Ok((input, nt)),
        Err(_) => Err(nom::Err::Error(Error::new(input, ErrorKind::Fail))),
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
