use std::slice;
use std::iter::Enumerate;
use std::ops::{Range, RangeTo, RangeFrom, RangeFull};

use nom::{InputLength, InputIter, Slice};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'ctx> {
    Identifier(&'ctx str),
    Literal(Literal<'ctx>),
    Reserved(Reserved),
    Operator(Operator),
    Punctuation(Punctuation),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Assignment,
    Addition,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Reserved {
    Let,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal<'ctx> {
    Integer(&'ctx str),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Punctuation {
    Curly(Balanced),
    Parenthesis(Balanced),
    Placeholder,
    SemiColon,
    Colon,
    Bar,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Balanced {
    Open,
    Close
}

impl<'ctx> From<Operator> for Token<'ctx> {
    fn from(t: Operator) -> Self {
        Token::Operator(t)
    }
}

impl<'ctx> From<Reserved> for Token<'ctx> {
    fn from(t: Reserved) -> Self {
        Token::Reserved(t)
    }
}

impl<'ctx> From<Literal<'ctx>> for Token<'ctx> {
    fn from(t: Literal<'ctx>) -> Self {
        Token::Literal(t)
    }
}

impl<'ctx> From<Punctuation> for Token<'ctx> {
    fn from(t: Punctuation) -> Self {
        Token::Punctuation(t)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Tokens<'ctx> {
    pub tokens: Vec<Token<'ctx>>,
}

impl<'ctx> Tokens<'ctx> {
    pub fn borrow<'a>(&'a self) -> Tks<'a> {
        Tks {
            tokens: &self.tokens[..]
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Tks<'ctx> {
    pub tokens: &'ctx [Token<'ctx>],
}

impl<'ctx> InputLength for Tks<'ctx> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'ctx> Slice<Range<usize>> for Tks<'ctx> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tks {
            tokens: &self.tokens[range]
        }
    }
}

impl<'ctx> Slice<RangeTo<usize>> for Tks<'ctx> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'ctx> Slice<RangeFrom<usize>> for Tks<'ctx> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.input_len())
    }
}

impl<'ctx> Slice<RangeFull> for Tks<'ctx> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        self.clone()
    }
}

impl<'ctx> InputIter for Tks<'ctx> {
    type Item = &'ctx Token<'ctx>;
    type RawItem = Token<'ctx>;
    type Iter = Enumerate<slice::Iter<'ctx, Token<'ctx>>>;
    type IterElem = slice::Iter<'ctx, Token<'ctx>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<slice::Iter<'ctx, Token<'ctx>>> {
        self.tokens.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> slice::Iter<'ctx, Token<'ctx>> {
        self.tokens.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
        where P: Fn(Self::RawItem) -> bool,
    {
        self.tokens.iter().position(|b| predicate(b.clone()))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if count <= self.tokens.len() {
            Some(count)
        } else {
            None
        }
    }
}
