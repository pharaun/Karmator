use nom::Input;
use nom::Needed;

use std::iter::Enumerate;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;
use std::slice::Iter;
use std::slice;


use crate::parser::karma_token::KarmaToken;

// Engine for allowing us to parse on top of tokens
#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) struct Tokens<'a> {
    tok: &'a [KarmaToken],
    start: usize,
    end: usize,
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
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_from(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[count..],
            start: count,
            end: self.end,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
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


//impl Slice<Range<usize>> for Tokens<'_> {
//    #[inline]
//    fn slice(&self, range: Range<usize>) -> Self {
//        Tokens {
//            tok: self.tok.slice(range.clone()),
//            start: self.start + range.start,
//            end: self.start + range.end,
//        }
//    }
//}
//
//impl Slice<RangeTo<usize>> for Tokens<'_> {
//    #[inline]
//    fn slice(&self, range: RangeTo<usize>) -> Self {
//        self.slice(0..range.end)
//    }
//}
//
//impl Slice<RangeFrom<usize>> for Tokens<'_> {
//    #[inline]
//    fn slice(&self, range: RangeFrom<usize>) -> Self {
//        self.slice(range.start..self.end - self.start)
//    }
//}
//
//impl Slice<RangeFull> for Tokens<'_> {
//    #[inline]
//    fn slice(&self, _: RangeFull) -> Self {
//        Tokens {
//            tok: self.tok,
//            start: self.start,
//            end: self.end,
//        }
//    }
//}
