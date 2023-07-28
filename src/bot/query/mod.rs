pub mod karma;
pub mod partial;
pub mod ranking;
pub mod reacji;
pub mod top_n;

use rusqlite as rs;

use unicase::UniCase;
use std::fmt;

use unicode_normalization::{UnicodeNormalization, is_nfc_quick, IsNormalized};

use crate::bot::parser::karma::Karma;
use crate::core::cache;
use crate::core::santizer;


// Normalize any incoming string to be stored in the database
pub fn normalize(input: &str) -> String {
    match is_nfc_quick(input.chars()) {
        IsNormalized::Yes => input.to_string(),
        _ => input.nfc().collect(),
    }
}

// Custom Type to handle unicase for the query users
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct KarmaName(UniCase<String>);

impl KarmaName {
    pub fn new(name: &str) -> KarmaName {
        KarmaName(UniCase::new(normalize(name)))
    }
}

impl ToString for KarmaName {
    fn to_string(&self) -> String {
        let KarmaName(n) = self;
        n.to_string()
    }
}

impl rs::ToSql for KarmaName {
    fn to_sql(&self) -> rs::Result<rs::types::ToSqlOutput<'_>> {
        Ok(rs::types::ToSqlOutput::from(self.to_string()))
    }
}

impl rs::ToSql for Karma {
    fn to_sql(&self) -> rs::Result<rs::types::ToSqlOutput<'_>> {
        Ok(rs::types::ToSqlOutput::from(
            match self {
                Karma::Up   => 1,
                Karma::Down => -1,
                Karma::Side => 0,
            }
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ReacjiAction { Add, Del }

impl rs::ToSql for ReacjiAction {
    fn to_sql(&self) -> rs::Result<rs::types::ToSqlOutput<'_>> {
        Ok(rs::types::ToSqlOutput::from(
            match self {
                ReacjiAction::Add => 1,
                ReacjiAction::Del => -1,
            }
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OrdQuery { Asc, Desc }

impl fmt::Display for OrdQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OrdQuery::Asc  => write!(f, "ASC"),
            OrdQuery::Desc => write!(f, "DESC"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum KarmaCol { Given, Recieved }

impl fmt::Display for KarmaCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaCol::Given    => write!(f, "karma_given_count"),
            KarmaCol::Recieved => write!(f, "karma_received_count"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum KarmaTyp { Total, Side }

impl fmt::Display for KarmaTyp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KarmaTyp::Total => write!(f, "up - down"),
            KarmaTyp::Side  => write!(f, "side"),
        }
    }
}

pub async fn santizer(
    input: &str,
    cache: &cache::Cache,
) -> String {
    match santizer::parse(input).ok() {
        None      => {
            eprintln!("ERROR [Santizer]: Failed to santize: {:?}", input);
            input.to_string()
        },

        Some(san) => {
            let mut safe_text = vec![];

            for seg in san.iter() {
                // TODO: do other id reprocessing such as:
                // 1. channel...
                match seg {
                    santizer::Segment::User(uid, l) => {
                        // Do a user id lookup
                        let username = cache.get_username(&uid).await;

                        match username {
                            Some(name) => safe_text.push(name),
                            None => {
                                // TODO: Log this, but for now fallback to
                                // just rendering it straight into the db
                                safe_text.push(
                                    santizer::Segment::User(uid, l).to_string()
                                );
                            },
                        }
                    },
                    // Everything else
                    s => safe_text.push(s.to_string()),
                }
            }

            safe_text.join("")
        },
    }
}
