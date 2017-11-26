use symtern::prelude::*;

use interner::Interner;

use super::parse;
use super::ast::{self, Ast, Expression, Identifier, Literal, Operation};
use super::ast::Expression::*;
use super::ast::Literal::*;
use super::ast::Operation::*;

macro_rules! assert_parse {
    ($interner:ident {
        $($code:tt)*
    }{
        $($exprs:expr)*
    }) => {
        {
            let tokens = ::lexer::Lexer::new(&mut $interner).tokenize(
                    stringify!($($code)*).as_bytes()
                ).1.unwrap().1;
            let parsed = ::parser::parse(tokens.borrow());
            let expected = ::nom::IResult::Done(
                    ::lexer::tokens::Tks {
                        tokens: &[][..],
                    },
                    Ast {
                        expressions: vec![
                            $($exprs,)*
                        ]
                    }
                );
            if parsed != expected {
                panic!("Expected:\n{:#?}\nParsed:\n{:#?}", expected, parsed);
            }
        }
    };
    ($($tks:tt)*) => {
        {
            let mut interner = Interner::new();
            assert_parse!(interner $($tks)*);
        }
    };
}

#[test]
fn parse_assignment() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        x = 1
    }{
        Operation(Assignment {
            identifier: Identifier(x),
            value: Box::new(Literal(Integer(one)))
        })
    });
}

#[test]
fn parse_tuple() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_parse!(interner {
        (1, 2)
    }{
        Tuple {
            value: vec![
                Literal(Integer(one)),
                Literal(Integer(two)),
            ]
        }
    });
}

#[test]
fn parse_tuple_with_trailing_colon() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_parse!(interner {
        (1, 2,)
    }{
        Tuple {
            value: vec![
                Literal(Integer(one)),
                Literal(Integer(two)),
            ]
        }
    });
}

#[test]
fn parse_initial() {
    assert_parse!({
        ()
    }{
        Tuple {
            value: vec![]
        }
    });
}
#[test]
fn parse_initial_trailing_colon() {
    assert_parse!({
        (,)
    }{
        Tuple {
            value: vec![]
        }
    });
}

#[test]
fn tokenize_union() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        |1, _|
    }{
        Union(Some(ast::Union {
            value: Box::new(Literal(Integer(one))),
            position: 0,
            size: 2,
        }))
    });
}

#[test]
fn tokenize_union_trailing_colon() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        |1, _,|
    }{
        Union(Some(ast::Union {
            value: Box::new(Literal(Integer(one))),
            position: 0,
            size: 2,
        }))
    });
}

#[test]
fn parse_terminal() {
    assert_parse!({
        ||
    }{
        Union(None)
    });
}

#[test]
fn parse_terminal_trailing_colon() {
    assert_parse!({
        |,|
    }{
        Union(None)
    });
}

#[test]
fn parse_addition() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_parse!(interner {
        (1 + 2)
    }{
        Operation(Addition {
            parameters: vec![Literal(Integer(one)), Literal(Integer(two))],
        })
    })
}
