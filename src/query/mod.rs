pub mod karma;
pub mod partial;
pub mod ranking;
pub mod reacji;
pub mod top_n;

use deadpool_postgres::GenericClient;
use tokio_postgres::types::to_sql_checked;
use tokio_postgres::types::IsNull;
use tokio_postgres::types::ToSql;
use tokio_postgres::types::Type;

use log::error;

use anyhow::Result as AResult;

use bytes::BytesMut;

use std::error::Error;
use std::fmt;
use unicase::UniCase;

use unicode_normalization::is_nfc_quick;
use unicode_normalization::IsNormalized;
use unicode_normalization::UnicodeNormalization as _;

use crate::parser::karma::Karma;
use kcore::sanitizer;
use kcore::slack;

// Normalize any incoming string to be stored in the database
pub fn normalize(input: &str) -> String {
    match is_nfc_quick(input.chars()) {
        IsNormalized::Yes => input.to_owned(),
        _ => input.nfc().collect(),
    }
}

// Custom Type to handle unicase for the query users
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct KarmaName(UniCase<String>);

impl KarmaName {
    pub fn new(name: &str) -> Self {
        Self(UniCase::new(normalize(name)))
    }
}

impl fmt::Display for KarmaName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(n) = self;
        write!(f, "{n}")
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
            Self::Up => 1,
            Self::Down => -1,
            Self::Side => 0,
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
            Self::Add => 1,
            Self::Del => -1,
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
            Self::Asc => write!(f, "ASC"),
            Self::Desc => write!(f, "DESC"),
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
            Self::Given => write!(f, "karma_given_count"),
            Self::Received => write!(f, "karma_received_count"),
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
            Self::Total => write!(f, "up - down"),
            Self::Side => write!(f, "side"),
        }
    }
}

pub async fn sanitizer<S>(input: &str, slack: &slack::Client<S>) -> String
where
    S: slack::HttpSender + Clone + Send + Sync + Sized,
{
    match sanitizer::parse(input).ok() {
        None => {
            error!("Failed to sanitize: {input:?}");
            input.to_owned()
        }

        Some(san) => {
            let mut safe_text = vec![];

            for seg in &san {
                // TODO: do other id reprocessing such as:
                // 1. channel...
                match seg {
                    sanitizer::Segment::User(uid, l) => {
                        // Do a user id lookup
                        let username = slack.get_username(uid).await;

                        match username {
                            Ok(Some(name)) => safe_text.push(name),
                            _ => {
                                // TODO: Log this, but for now fallback to
                                // just rendering it straight into the db
                                safe_text.push(sanitizer::Segment::User(uid, l).to_string());
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

async fn upsert_returning_id<C: GenericClient>(
    client: &C,
    insert_sql: &str,
    insert_params: &[&(dyn ToSql + Sync)],
    select_sql: &str,
    select_params: &[&(dyn ToSql + Sync)],
) -> AResult<i64> {
    // This is tricky to deal with, but this approach should be correct assuming: READ COMMITTED
    // isolation level remains in effect (The default). If this gets violated will start to see
    // race on this.
    let row = client.query_opt(insert_sql, insert_params).await?;
    if let Some(r) = row {
        Ok(r.try_get(0)?)
    } else {
        let row = client.query_one(select_sql, select_params).await?;
        Ok(row.try_get(0)?)
    }
}

pub async fn add_nick<C: GenericClient>(
    client: &C,
    user_id: String,
    username: KarmaName,
    real_name: KarmaName,
) -> AResult<i64> {
    upsert_returning_id(
        client,
        "INSERT INTO nick_metadata (cleaned_nick, full_name, username, hostmask) VALUES ($1, $2, $3, $4)
        ON CONFLICT(cleaned_nick, full_name, username, hostmask) DO NOTHING
        RETURNING id",
        &[&username, &real_name, &user_id, &"SlackServer"],
        "SELECT id FROM nick_metadata WHERE username = $1 ORDER BY id DESC LIMIT 1",
        &[&user_id],
    ).await
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
    upsert_returning_id(
        client,
        "INSERT INTO chan_metadata (channel) VALUES($1)
        ON CONFLICT (channel) DO NOTHING
        RETURNING id",
        &[&channel_id],
        "SELECT id FROM chan_metadata WHERE channel = $1",
        &[&channel_id],
    )
    .await
}
