use nom::Input;
use nom::Needed;

use std::iter::Enumerate;
use std::slice::Iter;

use crate::parser::karma_token::KarmaToken;

// Engine for allowing us to parse on top of tokens
#[derive(Debug, Clone, Copy)]
pub(super) struct Tokens<'a> {
    tok: &'a [KarmaToken],
    start: usize,
    end: usize,
}

impl PartialEq for Tokens<'_> {
    fn eq(&self, other: &Tokens<'_>) -> bool {
        self.tok == other.tok
    }
}

impl<'a> Tokens<'a> {
    pub(super) fn new(vec: &'a Vec<KarmaToken>) -> Self {
        Tokens {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }

    // Accessors for reading the current token stream
    pub(super) fn first(&self) -> Option<KarmaToken> {
        self.tok.first().cloned()
    }

    pub(super) fn last(&self) -> Option<KarmaToken> {
        self.tok.last().cloned()
    }

    pub(super) fn second_to_last(&self) -> Option<KarmaToken> {
        self.tok
            .split_last()
            .and_then(|(_, rest)| rest.last().cloned())
    }

    pub(super) fn is_empty(&self) -> bool {
        self.tok.is_empty()
    }

    pub(super) fn iter(&self) -> Iter<'_, KarmaToken> {
        self.tok.iter()
    }
}


impl<'a> Input for Tokens<'a> {
    type Item = &'a KarmaToken;
    type Iter = Iter<'a, KarmaToken>;
    type IterIndices = Enumerate<Iter<'a, KarmaToken>>;

    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }

    #[inline]
    fn take(&self, index: usize) -> Self {
        Tokens {
            tok: &self.tok[..index],
            start: self.start,
            end: index,
        }
    }

    #[inline]
    fn take_from(&self, index: usize) -> Self {
        Tokens {
            tok: &self.tok[index..],
            start: index,
            end: self.end,
        }
    }

    #[inline]
    fn take_split(&self, index: usize) -> (Self, Self) {
        // unclear why its flipped but all examaple i could find had this
        let (front, back) = self.tok.split_at(index);
        (
            Tokens {
                tok: back,
                start: index,
                end: self.end,
            },
            Tokens {
                tok: front,
                start: self.start,
                end: index,
            },
        )
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }

    #[inline]
    fn iter_elements(&self) -> <Self as Input>::Iter {
        self.tok.iter()
    }

    #[inline]
    fn iter_indices(&self) -> <Self as Input>::IterIndices {
        self.tok.iter().enumerate()
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}

// We Only need one type of token
#[cfg(test)]
macro_rules! text {
    ($data:expr) => {
        KarmaToken::Text($data.to_string())
    };
}

#[cfg(test)]
mod test_tokenizer {
    use super::*;

    fn token_data() -> Vec<KarmaToken> {
        vec![
            text!("A"),
            text!("B"),
            text!("C"),
            text!("D"),
            text!("E"),
            text!("F"),
        ]
    }

    #[test]
    fn test_first() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.first(),
            Some(text!("A")),
        );
    }

    #[test]
    fn test_last() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.last(),
            Some(text!("F")),
        );
    }

    #[test]
    fn test_second_to_last() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.second_to_last(),
            Some(text!("E")),
        );
    }

    #[test]
    fn test_is_empty() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.is_empty(),
            false,
        );

        let data = vec![];
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.is_empty(),
            true,
        );
    }

    #[test]
    fn test_input_len() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.input_len(),
            data.len(),
        );
    }

    #[test]
    fn test_take() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.take(0),
            Tokens {
                tok: &vec![],
                start: 0,
                end: 0,
            }
        );
        assert_eq!(
            tokens.take(1),
            Tokens {
                tok: &vec![text!("A")],
                start: 0,
                end: 1,
            }
        );
        assert_eq!(
            tokens.take(data.len()),
            Tokens {
                tok: &data,
                start: 0,
                end: data.len(),
            }
        );
    }

    #[test]
    fn test_take_from() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.take_from(0),
            Tokens {
                tok: &data,
                start: 0,
                end: data.len(),
            }
        );
        assert_eq!(
            tokens.take_from(1),
            Tokens {
                tok: &data[1..],
                start: 1,
                end: data.len(),
            }
        );
        assert_eq!(
            tokens.take_from(data.len()),
            Tokens {
                tok: &vec![],
                start: data.len(),
                end: data.len(),
            }
        );
    }

    #[test]
    fn test_take_split() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        // unclear why its flipped but all examaple i could find had this
        assert_eq!(
            tokens.take_split(0),
            (Tokens {
                tok: &data,
                start: 0,
                end: data.len(),
            }, Tokens {
                tok: &vec![],
                start: 0,
                end: 0,
            }),
        );
        assert_eq!(
            tokens.take_split(1),
            (Tokens {
                tok: &data[1..],
                start: 1,
                end: data.len(),
            }, Tokens {
                tok: &vec![text!("A")],
                start: 0,
                end: 1,
            }),
        );
        assert_eq!(
            tokens.take_split(data.len()),
            (Tokens {
                tok: &vec![],
                start: data.len(),
                end: data.len(),
            }, Tokens {
                tok: &data,
                start: 0,
                end: data.len(),
            }),
        );
    }

    #[test]
    fn test_position() {
        let data = token_data();
        let tokens = Tokens::new(&data);

        assert_eq!(
            tokens.position(|t| *t == text!("Z")),
            None,
        );
        assert_eq!(
            tokens.position(|t| *t == text!("A")),
            Some(0),
        );
        assert_eq!(
            tokens.position(|t| *t == text!("F")),
            Some(5),
        );
    }
}
