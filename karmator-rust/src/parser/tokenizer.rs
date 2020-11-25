use nom::InputLength;
use nom::InputTake;
use nom::InputIter;
use nom::Slice;
use nom::UnspecializedInput;

use std::iter::Enumerate;
use std::ops::RangeFull;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::ops::Range;
use std::slice::Iter;

use crate::parser::karma_token::KarmaToken;

// Engine for allowing us to parse on top of tokens
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Tokens<'a> {
    tok: &'a [KarmaToken],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a Vec<KarmaToken>) -> Self {
        Tokens {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }

    // Accessors for reading the current token stream
    pub fn first(&self) -> Option<KarmaToken> {
        self.tok.get(0).map(|t| t.clone())
    }

    pub fn last(&self) -> Option<KarmaToken> {
        self.tok.last().map(|t| t.clone())
    }

    pub fn is_empty(&self) -> bool {
        self.tok.is_empty()
    }

    pub fn iter(&self) -> Iter<'_, KarmaToken>{
        self.tok.iter()
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
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
}

impl<'a> InputLength for KarmaToken {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a KarmaToken;
    type Iter = Enumerate<::std::slice::Iter<'a, KarmaToken>>;
    type IterElem = ::std::slice::Iter<'a, KarmaToken>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, KarmaToken>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, KarmaToken> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(|b| predicate(b))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

impl UnspecializedInput for Tokens<'_> {}
