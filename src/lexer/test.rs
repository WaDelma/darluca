use super::tokenize;
use super::tokens::{Token, Tokens, Tks};
use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Balanced::*;
use super::tokens::Operator::*;
use super::tokens::Reserved::*;
use super::tokens::Literal::*;

macro_rules! assert_tokens {
    ({
        $($code:tt)*
    }{
        $($tokens:expr)*
    }) => {
        assert_eq!(
            tokenize(stringify!($($code)*).as_bytes()),
            ::nom::IResult::Done(
                &[][..],
                Tokens {
                    tokens: vec![
                        $($tokens,)*
                    ]
                }
            )
        );
    }
}

#[test]
fn tokenize_assignment() {
    assert_tokens!({
        x = 1
    }{
        Identifier("x")
        Operator(Assignment)
        Literal(Integer("1"))
    });
}

#[test]
fn tokenize_tuple() {
    assert_tokens!({
        (1 2)
    }{
        Punctuation(Parenthesis(Open))
        Literal(Integer("1"))
        Literal(Integer("2"))
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
    assert_tokens!({
        |1 _|
    }{
        Punctuation(Bar)
        Literal(Integer("1"))
        Punctuation(Placeholder)
        Punctuation(Bar)
    });
}

#[test]
fn tokenize_terminal() {
    assert_tokens!({
        ||
    }{
        Punctuation(Bar)
        Punctuation(Bar)
    });
}

#[test]
fn tokenize_addition() {
    assert_tokens!({
        +(1 2)
    }{
        Operator(Addition)
        Punctuation(Parenthesis(Open))
        Literal(Integer("1"))
        Literal(Integer("2"))
        Punctuation(Parenthesis(Close))
    })
}
