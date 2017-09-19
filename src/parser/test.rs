use super::parse;
use super::ast::{self, Ast, Expression, Identifier, Literal, Operation};
use super::ast::Expression::*;
use super::ast::Literal::*;
use super::ast::Operation::*;

macro_rules! assert_parse {
    ({
        $($code:tt)*
    }{
        $($exprs:expr)*
    }) => {
        {
            let tokens = ::lexer::tokenize(
                    stringify!($($code)*).as_bytes()
                ).unwrap().1;
            let parsed = parse(tokens.borrow());
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
    }
}

#[test]
fn parse_assignment() {
    assert_parse!({
        x = 1
    }{
        Operation(Assignment {
            identifier: "x".into(),
            value: Box::new(Literal(Integer("1")))
        })
    });
}

#[test]
fn parse_tuple() {
    assert_parse!({
        (1 2)
    }{
        Tuple {
            value: vec![
                Literal(Integer("1")),
                Literal(Integer("2")),
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
fn tokenize_union() {
    assert_parse!({
        |1 _|
    }{
        Union(Some(ast::Union {
            value: Box::new(Literal(Integer("1"))),
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
fn parse_addition() {
    assert_parse!({
        +(1 2)
    }{
        Operation(Addition {
            parameters: vec![Literal(Integer("1")), Literal(Integer("2"))],
        })
    })
}
