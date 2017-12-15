use lexer::Lexer;
use interner::Interner;

use super::tokens::Tokens;
use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Balanced::*;
use super::tokens::Operator::*;
use super::tokens::Reserved::*;
use super::tokens::Literal::*;
use super::tokens::Direction::*;

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
    let x = interner.intern("x");
    let one = interner.intern("1");
    let int = interner.intern("I32");
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
    let one = interner.intern("1");
    let two = interner.intern("2");
    assert_tokens!(interner {
        [1, 2,]
    }{
        Punctuation(Square(Open))
        Literal(Integer(one))
        Punctuation(Comma)
        Literal(Integer(two))
        Punctuation(Comma)
        Punctuation(Square(Close))
    });
}

#[test]
fn tokenize_initial() {
    assert_tokens!({
        [,]
    }{
        Punctuation(Square(Open))
        Punctuation(Comma)
        Punctuation(Square(Close))
    });
}

#[test]
fn tokenize_index_tuple() {
    let mut interner = Interner::new();
    let zero = interner.intern("0");
    let one = interner.intern("1");
    let two = interner.intern("2");
    assert_tokens!(interner {
        [1, 2,][0]
    }{
        Punctuation(Square(Open))
        Literal(Integer(one))
        Punctuation(Comma)
        Literal(Integer(two))
        Punctuation(Comma)
        Punctuation(Square(Close))
        Punctuation(Square(Open))
        Literal(Integer(zero))
        Punctuation(Square(Close))
    });
}

#[test]
fn tokenize_union() {
    let mut interner = Interner::new();
    let one = interner.intern("1");
    assert_tokens!(interner {
        [1|_|]
    }{
        Punctuation(Square(Open))
        Literal(Integer(one))
        Punctuation(Bar)
        Punctuation(Placeholder)
        Punctuation(Bar)
        Punctuation(Square(Close))
    });
}

#[test]
fn tokenize_terminal() {
    assert_tokens!({
        [|]
    }{
        Punctuation(Square(Open))
        Punctuation(Bar)
        Punctuation(Square(Close))
    });
}

#[test]
fn tokenize_addition() {
    let mut interner = Interner::new();
    let one = interner.intern("1");
    let two = interner.intern("2");
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

#[test]
fn tokenize_function_declaration() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    let x = interner.intern("x");
    assert_tokens!(interner {
        [x: I32,] -> I32 {
            x
        }
    }{
        Punctuation(Square(Open))
        Identifier(x)
        Punctuation(Colon)
        Type(int)
        Punctuation(Comma)
        Punctuation(Square(Close))
        Punctuation(Arrow(Right))
        Type(int)
        Punctuation(Curly(Open))
        Identifier(x)
        Punctuation(Curly(Close))
    });
}

#[test]
fn tokenize_function_declaration_and_calling() {
    let mut interner = Interner::new();
    let fun = interner.intern("fun");
    let int = interner.intern("I32");
    let x = interner.intern("x");
    let one = interner.intern("1");
    assert_tokens!(interner {
        let fun: (I32 -> I32) = [x: I32,] -> I32 {
            x
        }
        fun[1,]
    }{
        Reserved(Let)
        Identifier(fun)
        Punctuation(Colon)
        Punctuation(Parenthesis(Open))
        Type(int)
        Punctuation(Arrow(Right))
        Type(int)
        Punctuation(Parenthesis(Close))
        Operator(Assignment)
        Punctuation(Square(Open))
        Identifier(x)
        Punctuation(Colon)
        Type(int)
        Punctuation(Comma)
        Punctuation(Square(Close))
        Punctuation(Arrow(Right))
        Type(int)
        Punctuation(Curly(Open))
        Identifier(x)
        Punctuation(Curly(Close))
        Identifier(fun)
        Punctuation(Square(Open))
        Literal(Integer(one))
        Punctuation(Comma)
        Punctuation(Square(Close))
    });
}
