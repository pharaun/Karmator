pub mod karma;
pub mod partial;
pub mod ranking;
pub mod reacji;
pub mod top_n;

use tokio_postgres::types::to_sql_checked;
use tokio_postgres::types::IsNull;
use tokio_postgres::types::ToSql;
use tokio_postgres::types::Type;
use tokio_postgres::GenericClient;

use log::error;

use anyhow::Result as AResult;

use bytes::BytesMut;

use std::error::Error;
use std::fmt;
use unicase::UniCase;

use unicode_normalization::{is_nfc_quick, IsNormalized, UnicodeNormalization};

use crate::parser::karma::Karma;
use kcore::santizer;
use kcore::slack;

// Normalize any incoming string to be stored in the database
pub fn normalize(input: &str) -> String {
    match is_nfc_quick(input.chars()) {
        IsNormalized::Yes => input.to_string(),
        _ => input.nfc().collect(),
    }
}

// Custom Type to handle unicase for the query users
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct KarmaName(UniCase<String>);

impl KarmaName {
    pub fn new(name: &str) -> KarmaName {
        KarmaName(UniCase::new(normalize(name)))
    }
}

impl std::fmt::Display for KarmaName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let KarmaName(n) = self;
        write!(f, "{}", n)
    }
}

impl ToSql for KarmaName {
    fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, Box<dyn Error + Sync + Send>> {
        self.to_string().to_sql(ty, w)?;
        Ok(IsNull::No)
    }

    fn accepts(ty: &Type) -> bool {
        matches!(*ty, Type::TEXT | Type::VARCHAR)
    }
    to_sql_checked!();
}

impl ToSql for Karma {
    fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, Box<dyn Error + Sync + Send>> {
        let value: i16 = match *self {
            Karma::Up => 1,
            Karma::Down => -1,
            Karma::Side => 0,
        };
        value.to_sql(ty, w)?;
        Ok(IsNull::No)
    }

    fn accepts(ty: &Type) -> bool {
        matches!(*ty, Type::INT2)
    }
    to_sql_checked!();
}

#[derive(Debug, Clone, Copy)]
pub enum ReacjiAction {
    Add,
    Del,
}

impl ToSql for ReacjiAction {
    fn to_sql(&self, ty: &Type, w: &mut BytesMut) -> Result<IsNull, Box<dyn Error + Sync + Send>> {
        let value: i16 = match *self {
            ReacjiAction::Add => 1,
            ReacjiAction::Del => -1,
        };
        value.to_sql(ty, w)?;
        Ok(IsNull::No)
    }

    fn accepts(ty: &Type) -> bool {
        matches!(*ty, Type::INT2)
    }
    to_sql_checked!();
}

#[derive(Debug, Clone, Copy)]
pub enum OrdQuery {
    Asc,
    Desc,
}

impl fmt::Display for OrdQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OrdQuery::Asc => write!(f, "ASC"),
            OrdQuery::Desc => write!(f, "DESC"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum KarmaCol {
    Given,
    Received,
}

impl fmt::Display for KarmaCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaCol::Given => write!(f, "karma_given_count"),
            KarmaCol::Received => write!(f, "karma_received_count"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum KarmaTyp {
    Total,
    Side,
}

impl fmt::Display for KarmaTyp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaTyp::Total => write!(f, "up - down"),
            KarmaTyp::Side => write!(f, "side"),
        }
    }
}

pub async fn santizer<S>(input: &str, slack: &slack::Client<S>) -> String
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    match santizer::parse(input).ok() {
        None => {
            error!("Failed to santize: {:?}", input);
            input.to_string()
        }

        Some(san) => {
            let mut safe_text = vec![];

            for seg in san.iter() {
                // TODO: do other id reprocessing such as:
                // 1. channel...
                match seg {
                    santizer::Segment::User(uid, l) => {
                        // Do a user id lookup
                        let username = slack.get_username(uid).await;

                        match username {
                            Ok(Some(name)) => safe_text.push(name),
                            _ => {
                                // TODO: Log this, but for now fallback to
                                // just rendering it straight into the db
                                safe_text.push(santizer::Segment::User(uid, l).to_string());
                            }
                        }
                    }
                    // Everything else
                    s => safe_text.push(s.to_string()),
                }
            }

            safe_text.join("")
        }
    }
}

pub async fn add_nick<C: GenericClient>(
    client: &C,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
) -> AResult<i64> {
    let row = client
        .query_opt(
            "SELECT id FROM nick_metadata WHERE username = $1 ORDER BY id DESC LIMIT 1",
            &[&user_id],
        )
        .await?;
    match row {
        Some(r) => Ok(r.try_get(0)?),
        None => {
            let row = client.query_one(
                "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES ($1, $2, $3, $4) RETURNING id",
                &[&username, &real_name, &user_id, &"SlackServer"]
            ).await?;
            Ok(row.try_get(0)?)
        }
    }
}

pub async fn add_channel_opt<C: GenericClient>(
    client: &C,
    channel_id: Option<String>,
) -> AResult<Option<i64>> {
    Ok(match channel_id {
        Some(cid) => Some(add_channel(client, cid).await?),
        None => None,
    })
}

pub async fn add_channel<C: GenericClient>(client: &C, channel_id: String) -> AResult<i64> {
    let row = client
        .query_opt(
            "SELECT id FROM chan_metadata WHERE channel = $1",
            &[&channel_id],
        )
        .await?;
    match row {
        Some(r) => Ok(r.try_get(0)?),
        None => {
            let row = client
                .query_one(
                    "INSERT INTO chan_metadata (channel) VALUES ($1) RETURNING id",
                    &[&channel_id],
                )
                .await?;
            Ok(row.try_get(0)?)
        }
    }
}
