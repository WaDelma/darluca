use std::slice;
use std::iter::Enumerate;
use std::ops::{Range, RangeTo, RangeFrom, RangeFull};

use nom::{InputLength, InputIter, Slice};

use interner::Symbol;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Identifier(Symbol),
    Literal(Literal),
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
pub enum Literal {
    Integer(Symbol),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Punctuation {
    Curly(Balanced),
    Parenthesis(Balanced),
    Square(Balanced),
    Angle(Balanced),
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

impl From<Operator> for Token {
    fn from(t: Operator) -> Self {
        Token::Operator(t)
    }
}

impl From<Reserved> for Token {
    fn from(t: Reserved) -> Self {
        Token::Reserved(t)
    }
}

impl From<Literal> for Token {
    fn from(t: Literal) -> Self {
        Token::Literal(t)
    }
}

impl From<Punctuation> for Token {
    fn from(t: Punctuation) -> Self {
        Token::Punctuation(t)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Tokens {
    pub tokens: Vec<Token>,
}

impl Tokens {
    pub fn borrow(&self) -> Tks {
        Tks {
            tokens: &self.tokens[..]
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Tks<'a> {
    pub tokens: &'a [Token],
}

impl<'a> InputLength for Tks<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> Slice<Range<usize>> for Tks<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tks {
            tokens: &self.tokens[range]
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tks<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tks<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.input_len())
    }
}

impl<'a> Slice<RangeFull> for Tks<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        self.clone()
    }
}

impl<'a> InputIter for Tks<'a> {
    type Item = &'a Token;
    type RawItem = Token;
    type Iter = Enumerate<slice::Iter<'a, Token>>;
    type IterElem = slice::Iter<'a, Token>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<slice::Iter<'a, Token>> {
        self.tokens.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> slice::Iter<'a, Token> {
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
