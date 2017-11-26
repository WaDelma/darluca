use symtern::prelude::*;

use lexer::Lexer;
use interner::Interner;

use super::tokens::{Token, Tokens, Tks};
use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Balanced::*;
use super::tokens::Operator::*;
use super::tokens::Reserved::*;
use super::tokens::Literal::*;

macro_rules! assert_tokens {
    ($interner:ident {
        $($code:tt)*
    }{
        $($tokens:expr)*
    }) => {
        assert_eq!(
            Lexer::new(&mut $interner)
                .tokenize(stringify!($($code)*).as_bytes()).1,
            ::nom::IResult::Done(
                &[][..],
                Tokens {
                    tokens: vec![
                        $($tokens,)*
                    ]
                }
            )
        );
    };
    ($($tks:tt)*) => {
        {
            let mut interner = Interner::new();
            assert_tokens!(interner $($tks)*);
        }
    };
}

#[test]
fn tokenize_assignment() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    assert_tokens!(interner {
        x = 1
    }{
        Identifier(x)
        Operator(Assignment)
        Literal(Integer(one))
    });
}

#[test]
fn tokenize_tuple() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_tokens!(interner {
        (1, 2)
    }{
        Punctuation(Parenthesis(Open))
        Literal(Integer(one))
        Punctuation(Colon)
        Literal(Integer(two))
        Punctuation(Parenthesis(Close))
    });
}

#[test]
fn tokenize_initial() {
    assert_tokens!({
        ()
    }{
        Punctuation(Parenthesis(Open))
        Punctuation(Parenthesis(Close))
    });
}

#[test]
fn tokenize_union() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    assert_tokens!(interner {
        (1|_)
    }{
        Punctuation(Parenthesis(Open))
        Literal(Integer(one))
        Punctuation(Bar)
        Punctuation(Placeholder)
        Punctuation(Parenthesis(Close))
    });
}

#[test]
fn tokenize_terminal() {
    assert_tokens!({
        (|)
    }{
        Punctuation(Parenthesis(Open))
        Punctuation(Bar)
        Punctuation(Parenthesis(Close))
    });
}

#[test]
fn tokenize_addition() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_tokens!(interner {
        (1 + 2)
    }{
        Punctuation(Parenthesis(Open))
        Literal(Integer(one))
        Operator(Addition)
        Literal(Integer(two))
        Punctuation(Parenthesis(Close))
    })
}
