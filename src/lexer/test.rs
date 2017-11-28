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
fn tokenize_declaration() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_tokens!(interner {
        let x: I32 = 1
    }{
        Reserved(Let)
        Identifier(x)
        Punctuation(Colon)
        Type(int)
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
        Punctuation(Comma)
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
fn tokenize_index_tuple() {
    let mut interner = Interner::new();
    let zero = interner.intern("0").unwrap();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_tokens!(interner {
        (1, 2)[0]
    }{
        Punctuation(Parenthesis(Open))
        Literal(Integer(one))
        Punctuation(Comma)
        Literal(Integer(two))
        Punctuation(Parenthesis(Close))
        Punctuation(Square(Open))
        Literal(Integer(zero))
        Punctuation(Square(Close))
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

#[test]
fn tokenize_true() {
    assert_tokens!({
        true
    }{
        Reserved(True)
    });
}

#[test]
fn tokenize_false() {
    assert_tokens!({
        false
    }{
        Reserved(False)
    });
}

#[test]
fn tokenize_if_else() {
    assert_tokens!({
        if true {
        } else {
        }
    }{
        Reserved(If)
        Reserved(True)
        Punctuation(Curly(Open))
        Punctuation(Curly(Close))
        Reserved(Else)
        Punctuation(Curly(Open))
        Punctuation(Curly(Close))
    });
}
