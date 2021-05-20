pub mod karma;
mod karma_def;
mod karma_token;
mod tokenizer;

#[cfg(feature = "arbitrary")]
pub mod test_all_token {
    use nom::IResult;
    pub use crate::bot::parser::karma_token::KarmaToken;

    pub fn all_token(input: &str) -> IResult<&str, Vec<KarmaToken>> {
        crate::bot::parser::karma_token::all_token(input)
    }
}

pub fn reacji_to_karma(input: &str) -> Option<karma::Karma> {
    karma_def::reacji_to_karma(input)
}
