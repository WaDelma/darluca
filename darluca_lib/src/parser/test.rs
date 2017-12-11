use interner::Interner;
use typechecker::Type;

use super::ast::{self, Ast, Expression, Identifier};
use super::ast::Expression::*;
use super::ast::Literal::*;
use super::ast::Operation::*;
use super::ast::If::*;

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
                            $(From::from($exprs),)*
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
fn parse_declaration() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: I32 = 1
    }{
        Declaration {
            identifier: Identifier(x),
            ty: Type::Named(int),
            value: Some(Box::new(Literal(Integer(one)).into())),
        }
    });
}

#[test]
fn parse_empty_declaration() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: I32
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Named(int),
        }
    });
}

#[test]
fn parse_terminal_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    assert_parse!(interner {
        let x: [,]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Tuple(vec![]),
        }
    });
}

#[test]
fn parse_tuple_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: [I32, I32,]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Tuple(vec![
                    Type::Named(int),
                    Type::Named(int)
                ]),
        }
    });
}

#[test]
fn parse_tuple_tuple_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: [I32, [I32,],]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Tuple(vec![
                    Type::Named(int),
                    Type::Tuple(vec![
                        Type::Named(int)
                    ])
                ]),
        }
    });
}

#[test]
fn parse_initial_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    assert_parse!(interner {
        let x: [|]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Union(vec![]),
        }
    });
}

#[test]
fn parse_union_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: [I32|I32|]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Union(vec![
                    Type::Named(int),
                    Type::Named(int)
                ]),
        }
    });
}

#[test]
fn parse_union_union_type() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: [I32|[I32|]|]
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Union(vec![
                    Type::Named(int),
                    Type::Union(vec![
                        Type::Named(int)
                    ])
                ]),
        }
    });
}

#[test]
fn parse_assignment() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: I32
        x = 1
    }{
        Declaration {
            identifier: Identifier(x),
            value: None,
            ty: Type::Named(int),
        }
        Operation(Assignment {
            identifier: Identifier(x),
            value: Box::new(Literal(Integer(one)).into()),
        })
    });
}

#[test]
fn parse_scope() {
    let mut interner = Interner::new();
    assert_parse!(interner {
        {
        }
    }{
        Scope {
            expressions: vec![]
        }
    });
}

#[test]
fn parse_scope_with_declaration() {
    let mut interner = Interner::new();
    let x = interner.intern("x").unwrap();
    assert_parse!(interner {
        {
            let x: [|]
        }
    }{
        Scope {
            expressions: vec![
                Declaration {
                    identifier: Identifier(x),
                    value: None,
                    ty: Type::Union(vec![]),
                }.into()
            ]
        }
    });
}

#[test]
fn parse_terminal() {
    assert_parse!({
        [,]
    }{
        Tuple {
            value: vec![]
        }
    });
}

#[test]
fn parse_tuple() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    assert_parse!(interner {
        [1, 2,]
    }{
        Tuple {
            value: vec![
                Literal(Integer(one)).into(),
                Literal(Integer(two)).into(),
            ]
        }
    });
}

#[test]
fn parse_tuple_tuple() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    let two = interner.intern("2").unwrap();
    let three = interner.intern("3").unwrap();
    assert_parse!(interner {
        [1, [2, 3,],]
    }{
        Tuple {
            value: vec![
                Literal(Integer(one)).into(),
                Tuple {
                    value: vec![
                        Literal(Integer(two)).into(),
                        Literal(Integer(three)).into(),
                    ]
                }.into(),
            ]
        }
    });
}

#[test]
fn parse_initial() {
    assert_parse!({
        [|]
    }{
        Union(None)
    });
}

#[test]
fn parse_union() {
    let mut interner = Interner::new();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        [1|_|]
    }{
        Union(Some(ast::Union {
            value: Box::new(Literal(Integer(one)).into()),
            position: 0,
            size: 2,
        }))
    });
}

#[test]
fn parse_union_union() {
    let mut interner = Interner::new();
    let two = interner.intern("2").unwrap();
    assert_parse!(interner {
        [[_|2|]|_|_|]
    }{
        Union(Some(ast::Union {
            value: Box::new(Union(Some(ast::Union {
                value: Box::new(Literal(Integer(two)).into()),
                position: 1,
                size: 2,
            })).into()),
            position: 0,
            size: 3,
        }))
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
            parameters: vec![
                    Literal(Integer(one)).into(),
                    Literal(Integer(two)).into()
                ],
        })
    })
}

#[test]
fn parse_true() {
    assert_parse!({
        true
    }{
        Literal(Boolean(true))
    });
}

#[test]
fn parse_false() {
    assert_parse!({
        false
    }{
        Literal(Boolean(false))
    });
}

#[test]
fn parse_if_else() {
    assert_parse!({
        if true {
            true
        } else {
            false
        }
    }{
        If(Condition {
            condition: Box::new(Literal(Boolean(true)).into()),
            expressions: vec![Literal(Boolean(true)).into()],
            otherwise: Box::new(Else(vec![Literal(Boolean(false)).into()])),
        })
    });
}

#[test]
fn parse_moving_into_scope() {
    let mut interner = Interner::new();
    let int = interner.intern("I32").unwrap();
    let x = interner.intern("x").unwrap();
    let y = interner.intern("y").unwrap();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        let x: I32 = 1
        let y: I32 = {
            x
        }
    }{
        Declaration {
            identifier: Identifier(x),
            ty: Type::Named(int),
            value: Some(Box::new(Literal(Integer(one)).into())),
        }
        Declaration {
            identifier: Identifier(y),
            ty: Type::Named(int),
            value: Some(Box::new(Scope {
                expressions: vec![Expression::Identifier(Identifier(x)).into()]
            }.into())),
        }
    });
}

#[test]
fn parse_function_declaration() {
    let mut interner = Interner::new();
    let int = interner.intern("I32").unwrap();
    let x = interner.intern("x").unwrap();
    assert_parse!(interner {
        [x: I32,] -> I32 {
            x
        }
    }{
        Function {
            params: vec![Identifier(x)],
            expressions: vec![Expression::Identifier(Identifier(x)).into()],
            parameter_ty: Type::Named(int),
            return_ty: Type::Named(int),
        }
    });
}

#[test]
fn parse_function_declaration_and_calling() {
    let mut interner = Interner::new();
    let fun = interner.intern("fun").unwrap();
    let int = interner.intern("I32").unwrap();
    let x = interner.intern("x").unwrap();
    let one = interner.intern("1").unwrap();
    assert_parse!(interner {
        let fun: (I32 -> I32) = [x: I32,] -> I32 {
            x
        }
        fun[1,]
    }{
        Declaration {
            identifier: Identifier(fun),
            value: Some(Box::new(Function {
                params: vec![Identifier(x)],
                expressions: vec![Expression::Identifier(Identifier(x)).into()],
                parameter_ty: Type::Named(int),
                return_ty: Type::Named(int),
            }.into())),
            ty: Type::Function(Box::new(Type::Named(int)), Box::new(Type::Named(int)), None),
        }
        Operation(Calling {
            name: Identifier(fun),
            parameters: vec![Literal(Integer(one)).into()]
        })
    });
}
