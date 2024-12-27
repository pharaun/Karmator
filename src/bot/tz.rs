use chrono::prelude::{Utc, DateTime, NaiveTime};
use chrono_tz::Tz;
use chrono_tz::OffsetComponents;
use chrono::NaiveDateTime;
use chrono::TimeZone;
use chrono::Timelike;

use log::error;

use kcore::slack;

use crate::bot::user_event::Event;

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
      eof,
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


const TIME_FORMAT: &str = "%-l:%M%P";


pub async fn timezone<S>(
    event: &mut Event<S>,
    input: Vec<&str>,
)
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    if input.is_empty() {
        // !tz --> utc current time + 3 tz (pacific, eastern, uk)
        let utc_time: DateTime<Utc> = Utc::now();

        event.send_reply(&format_time(utc_time)).await;
    } else {
        let input = input.join(" ");
        match parse(&input) {
            Err(e) => {
                error!("ERROR [TZ]: {:?}", e);

                let message =
                    "Usage: `!tz TIME [TZ]`
                    24hr: `!tz 05:00 [TZ]` or `!tz 0500 [TZ]`
                    12hr: `!tz 2:00pm [TZ]` or `!tz 5pm [TZ]`
                    [TZ]: (optional) `SFO`, `TOR`, `LON`, `HAM`
                    [TZ]: (note) if not specified, will use your slack Timezone";
                event.send_reply(message).await;
            },
            Ok((_, TzReq { time: nt, zone: None })) => {
                match event.get_user_tz().await {
                    None => {
                        event.send_reply("Internal error, unable to get your slack timezone").await;
                    },
                    Some(user_tz) => {
                        match user_tz.tz.parse() {
                            Err(_) => {
                                event.send_reply("Internal error, unable to parse your slack timezone").await;
                            },
                            Ok(tz) => {
                                // Validate the offset
                                validate_offset(&tz, user_tz.offset);

                                // Convert time
                                match convert_naive_time(nt, tz) {
                                    None => {
                                        event.send_reply("Internal error, timezone happened!").await;
                                    },
                                    Some(given_datetime_tz) => {
                                        event.send_reply(&format_time(given_datetime_tz)).await;
                                    },
                                }
                            },
                        }
                    },
                }
            },
            Ok((_, TzReq { time: nt, zone: Some(tz_abbv) })) => {
                match convert_naive_time(nt, tz_abbv.0) {
                    None => {
                        event.send_reply("Internal error, timezone happened!").await;
                    },
                    Some(given_datetime_tz) => {
                        let note = match tz_abbv.1 {
                            None => "".to_string(),
                            Some(city) => format!(
                                "This is deprecated, use `{}` instead.\n",
                                city,
                            ),
                        };
                        event.send_reply(&format!("{} - {}", note, &format_time(given_datetime_tz))).await;
                    },
                }
            },
        }
    }
}

// Core hours:
// Green  - 11-15
// Yellow - 9-11, 15-17
// Orange - 7-9, 17-19
// Red    - 0-7, 19-24
fn core_hours<T: TimeZone>(dt: DateTime<T>) -> &'static str {

    match dt.hour() {
        // 11am to 3pm
        11 | 12 | 13 | 14  => ":green:",
        // 9am to 5pm
        9 | 10 | 15 | 16 => ":yellow:",
        // 7am to 7pm
        7 | 8 | 17 | 18 => ":orange:",
        // Anything else
        _ => ":red:",
    }
}


fn format_time<T: TimeZone>(dt: DateTime<T>) -> String {
    // Supported locals
    let out_tz = convert_to_locales(dt);

    format!(
        "{}Pacific: {}, {}Eastern: {}, {}UK: {}, {}Germany: {}",
        core_hours(out_tz.pacific),
        out_tz.pacific.format(TIME_FORMAT),
        core_hours(out_tz.eastern),
        out_tz.eastern.format(TIME_FORMAT),
        core_hours(out_tz.london),
        out_tz.london.format(TIME_FORMAT),
        core_hours(out_tz.berlin),
        out_tz.berlin.format(TIME_FORMAT),
    )
}


fn validate_offset(tz: &Tz, offset: i64) {
    // Validate and compare the offset
    let utc_time: DateTime<Utc> = Utc::now();
    let tz_time = utc_time.with_timezone(tz);
    let tz_offset = tz_time.offset().base_utc_offset() + tz_time.offset().dst_offset();

    if tz_offset.num_seconds() != offset {
        error!(
            "[TZ]: Slack offset {}, parsed offset {} does not match!",
            offset,
            tz_offset.num_seconds(),
        );
    }
}


fn convert_naive_time(nt: NaiveTime, tz: Tz) -> Option<DateTime<Tz>> {
    // Convert the given naive time to the given Tz
    let utc_time: DateTime<Utc> = Utc::now();
    let tz_time = utc_time.with_timezone(&tz);
    let naive_datetime = tz_time.naive_local();
    let naive_date = naive_datetime.date();
    let given_datetime = NaiveDateTime::new(naive_date, nt);

    given_datetime.and_local_timezone(tz).earliest()
}

struct OutTz {
    pacific: DateTime<Tz>,
    eastern: DateTime<Tz>,
    london: DateTime<Tz>,
    berlin: DateTime<Tz>,
}

fn convert_to_locales<T: TimeZone>(dt: DateTime<T>) -> OutTz {
    OutTz {
        pacific: dt.with_timezone(&chrono_tz::America::Los_Angeles),
        eastern: dt.with_timezone(&chrono_tz::America::Toronto),
        london:  dt.with_timezone(&chrono_tz::Europe::London),
        berlin:  dt.with_timezone(&chrono_tz::Europe::Berlin),
    }
}


#[derive(Debug, PartialEq)]
struct TzReq<'a> {
    time: NaiveTime,
    zone: Option<(Tz, Option<&'a str>)>
}

fn parse(input: &str) -> IResult<&str, TzReq> {
    let (input, time) = alt((
        twelve,
        twenty_four,
    ))(input)?;
    let (input, tz) = preceded(multispace0, opt(tz_abbv))(input)?;
    let (input, _) = eof(input)?;

    Ok((input, TzReq {
        time: time,
        zone: tz,
    }))
}

// Note: Nonstandard timezones, for user convience only:
// PST, PDT (standard/Daylight(summer)) - pacific
// EST, EDT (standard/Daylight(summer)) - eastern
// BST, GMT (summer time/gmt) - london (GMT != UTC)
// CEST, CET (summer time) - Hamburg
fn tz_abbv(input: &str) -> IResult<&str, (Tz, Option<&str>)> {
    alt((
        map(tag_no_case("SFO"),  |_| (chrono_tz::America::Los_Angeles, None)),
        map(tag_no_case("TOR"),  |_| (chrono_tz::America::Toronto, None)),
        map(tag_no_case("LON"),  |_| (chrono_tz::Europe::London, None)),
        map(tag_no_case("HAM"),  |_| (chrono_tz::Europe::Berlin, None)),

        map(tag_no_case("PST"),  |_| (chrono_tz::America::Los_Angeles, Some("SFO"))),
        map(tag_no_case("PDT"),  |_| (chrono_tz::America::Los_Angeles, Some("SFO"))),
        map(tag_no_case("EST"),  |_| (chrono_tz::America::Toronto, Some("TOR"))),
        map(tag_no_case("EDT"),  |_| (chrono_tz::America::Toronto, Some("TOR"))),
        map(tag_no_case("BST"),  |_| (chrono_tz::Europe::London, Some("LON"))),
        map(tag_no_case("GMT"),  |_| (chrono_tz::Europe::London, Some("LON"))),
        map(tag_no_case("CEST"), |_| (chrono_tz::Europe::Berlin, Some("HAM"))),
        map(tag_no_case("CET"),  |_| (chrono_tz::Europe::Berlin, Some("HAM"))),
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
            Ok(("", NaiveTime::from_hms_opt(13, 0, 0).unwrap()))
        );
        assert_eq!(
            twelve("5 AM"),
            Ok(("", NaiveTime::from_hms_opt(5, 0, 0).unwrap()))
        );
        assert_eq!(
            twelve("5:10pm"),
            Ok(("", NaiveTime::from_hms_opt(17, 10, 0).unwrap()))
        );
        assert_eq!(
            twelve("11:20 am"),
            Ok(("", NaiveTime::from_hms_opt(11, 20, 0).unwrap()))
        );
    }

    #[test]
    fn test_twenty_four() {
        assert_eq!(
            twenty_four("0220"),
            Ok(("", NaiveTime::from_hms_opt(2, 20, 0).unwrap()))
        );
        assert_eq!(
            twenty_four("17:10"),
            Ok(("", NaiveTime::from_hms_opt(17, 10, 0).unwrap()))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("2pm"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms_opt(14, 00, 0).unwrap(),
                zone: None,
            }))
        );
        assert_eq!(
            parse("5:30 pm pst"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms_opt(17, 30, 0).unwrap(),
                zone: Some((chrono_tz::America::Los_Angeles, Some("SFO"))),
            }))
        );
        assert_eq!(
            parse("0801 GMT"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms_opt(8, 1, 0).unwrap(),
                zone: Some((chrono_tz::Europe::London, Some("LON"))),
            }))
        );
        assert_eq!(
            parse("5:30 pm sfo"),
            Ok(("", TzReq {
                time: NaiveTime::from_hms_opt(17, 30, 0).unwrap(),
                zone: Some((chrono_tz::America::Los_Angeles, None)),
            }))
        );
    }

    #[test]
    fn test_convert_naive_time() {
        let tz_req = parse("5pm PST").unwrap();
        let conv_req = convert_naive_time(tz_req.1.time, tz_req.1.zone.unwrap().0).unwrap();

        assert_eq!(
            conv_req.time(),
            NaiveTime::from_hms_opt(17, 0, 0).unwrap(),
        );
        assert_eq!(
            conv_req.timezone(),
            chrono_tz::America::Los_Angeles,
        );
    }

    #[test]
    fn test_convert_to_locales() {
        let tz_req = parse("5pm PST").unwrap();
        let conv_req = convert_naive_time(tz_req.1.time, tz_req.1.zone.unwrap().0).unwrap();
        let out_req = convert_to_locales(conv_req);

        assert_eq!(
            out_req.pacific.time(),
            NaiveTime::from_hms_opt(17, 0, 0).unwrap(),
        );
        assert_eq!(
            out_req.pacific.timezone(),
            chrono_tz::America::Los_Angeles,
        );
    }
}
