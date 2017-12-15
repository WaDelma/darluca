use interner::Interner;
use parser::ast::{Expression, Expr, Identifier, Operation, Literal};
use typechecker::Typechecker;

use super::Value::*;
use super::Ty::*;
use super::TypedValue as TyVal;

mod error;

macro_rules! assert_parse {
    ($interner:ident {
        $($code:tt)*
    }$({
        $($var:ident => $val:expr)*
    })*) => {
        {
            #![allow(unused)]
            use interpreter::Memory;
            use std::collections::HashMap;

            let tokens = {
                ::lexer::Lexer::new(&mut $interner).tokenize(
                    stringify!($($code)*).as_bytes()
                ).1.unwrap().1
            };
            let ast = ::parser::parse(tokens.borrow()).unwrap().1;
            let ast = ::typechecker::typecheck(ast, &mut $interner).unwrap();
            let mut memory = Memory::new();
            match ::interpreter::interpret_scope(&ast.expressions, &mut memory, &mut $interner) {
                Err(e) => panic!("Interpreting failed: {}", e),
                _ => {},
            }
            let mut n = 1;
            $(
                let mut expected = HashMap::new();
                $(
                    let interned = $interner.intern(stringify!($var));
                    expected.insert(Identifier(interned), $val);
                )*
                let scope = &memory.used_scopes[memory.used_scopes.len() - n];
                if scope != &expected {
                    panic!(
                        "Expected:\n{:#?}\nInterpreted:\n{:#?}",
                        expected,
                        scope);
                }
                n += 1;
            )*
        }
    };
    ($($tks:tt)*) => {
        {
            let mut interner = ::interner::Interner::new();
            assert_parse!(interner $($tks)*);
        }
    };
}

#[test]
fn interpret_add_and_declaration() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = (1 + 2)
    }{
        x => TyVal::unchecked(Int(3), Named(int))
    });
}

#[test]
fn interpret_add_from_variable() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let y: I32 = 2
        let x: I32 = (3 + y)
    }{
        y => TyVal::unchecked(Invalid, Named(int))
        x => TyVal::unchecked(Int(5), Named(int))
    });
}

#[test]
fn interpret_assign() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = 1
        x = 2
    }{
        x => TyVal::unchecked(Int(2), Named(int))
    });
}

#[test]
fn interpret_multiple_declarations() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = 1
        let y: I32 = 2
    }{
        x => TyVal::unchecked(Int(1), Named(int))
        y => TyVal::unchecked(Int(2), Named(int))
    });
}

#[test]
fn interpret_move() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = 1
        let y: I32 = x
    }{
        x => TyVal::unchecked(Invalid, Named(int))
        y => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_move_on_assign() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let y: I32 = 1
        let x: I32 = y = 2
    }{
        x => TyVal::unchecked(Int(1), Named(int))
        y => TyVal::unchecked(Int(2), Named(int))
    });
}

#[test]
fn interpret_declaration_declaration() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [,] = let y: I32 = 1
    }{
        x => TyVal::unchecked(Tup(vec![]), Tuple(vec![]))
        y => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_tuple() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [I32, I32, I32,] = [1, 2, 3,]
    }{
        x => TyVal::unchecked(Tup(vec![
                Int(1),
                Int(2),
                Int(3)
            ]),
            Tuple(vec![
                Named(int),
                Named(int),
                Named(int),
            ]))
    });
}

#[test]
fn interpret_tuple_tuple() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [[I32,],] = [[(1 + 1),],]
    }{
        x => TyVal::unchecked(Tup(vec![
                Tup(vec![
                    Int(2)
                ])
            ]),
            Tuple(vec![
                Tuple(vec![
                    Named(int),
                ]),
            ]))
    });
}

#[test]
fn interpret_tuple_indexing() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [I32, I32, I32,] = [1, 2, 3,]
        let y: I32 = x[1]
    }{
        x => TyVal::unchecked(Tup(vec![
                    Int(1),
                    Invalid,
                    Int(3)
                ]),
                Tuple(vec![
                    Named(int),
                    Named(int),
                    Named(int),
                ])
            )
        y => TyVal::unchecked(Int(2), Named(int))
    });
}

#[test]
fn interpret_union() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [I32|I32|I32|] = [1|_|_|]
    }{
        x => TyVal::unchecked(
                Uni(0, Box::new(Int(1)), 3),
                Union(vec![
                    Named(int),
                    Named(int),
                    Named(int)]
                )
            )
    });
}

#[test]
fn interpret_union_union() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: [[I32|I32|]|I32|I32|] = [[_|2|]|_|_|]
    }{
        x => TyVal::unchecked(
                Uni(0, Box::new(
                    Uni(1, Box::new(
                        Int(2)
                    ), 2)
                ), 3),
                Union(vec![
                    Union(vec![
                        Named(int),
                        Named(int),
                    ]),
                    Named(int),
                    Named(int),
                ])
            )
    });
}

#[test]
fn interpret_scope() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let y: I32 = {
            let x: I32 = 1
            2
        }
    }{
        y => TyVal::unchecked(Int(2), Named(int))
    }{
        x => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_boolean() {
    let mut interner = Interner::new();
    let boolean = interner.intern("Bool");
    assert_parse!(interner {
        let x: Bool = true
        let y: Bool = false
    }{
        x => TyVal::unchecked(Bool(true), Named(boolean))
        y => TyVal::unchecked(Bool(false), Named(boolean))
    });
}

#[test]
fn interpret_if() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = if true {
            1
        } else {
            2
        }
    }{
        x => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_else() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = if false {
            1
        } else {
            2
        }
    }{
        x => TyVal::unchecked(Int(2), Named(int))
    });
}

#[test]
fn interpret_else_if() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = if false {
            1
        } else if false {
            2
        } else {
            3
        }
    }{
        x => TyVal::unchecked(Int(3), Named(int))
    });
}

#[test]
fn interpret_moving_into_scope() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    assert_parse!(interner {
        let x: I32 = 1
        let y: I32 = {
            x
        }
    }{
        x => TyVal::unchecked(Invalid, Named(int))
        y => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_function() {
    let mut interner = Interner::new();
    let x = interner.intern("x");
    let int = interner.intern("I32");
    let one = interner.intern("1");
    // TODO: Figure out how to reproduce typechecking here. Or some other way to test this.
    let mut typechecker = Typechecker::new();
    assert_parse!(interner {
        let fun: (I32 -> I32) = [x: I32,] -> I32 {
            (x + 1)
        }
        let x: I32 = fun[1,]
        fun = {
            fun
        }
        let y: I32 = fun[2,]
    }{
        fun => TyVal::unchecked(Fun(
                vec![Identifier(x)],
                vec![Expr::new(Expression::Operation(
                    Operation::Addition {
                        parameters: vec![
                            Expr::new(Expression::Identifier(Identifier(x)), typechecker.new_type(::typechecker::Type::Named(int))),
                            Expr::new(Expression::Literal(Literal::Integer(one)), typechecker.new_type(::typechecker::Type::Named(int)))
                        ]
                    }
                ), typechecker.new_type(::typechecker::Type::Named(int)))],
                vec![]
            ),
            Function(Box::new(Named(int)), Box::new(Named(int)), None)
        )
        x => TyVal::unchecked(Int(2), Named(int))
        y => TyVal::unchecked(Int(3), Named(int))
    });
}

#[test]
// TODO: Need to implement super type for closures to be able to do these tests.
fn interpret_closure() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    let closure = interner.intern("fun¤0");
    assert_parse!(interner {
        let fun: ([,] -> I32) = {
            let x: I32 = 1
            [,] -> I32 {
                x
            }
        }
        let y: I32 = fun[,]
    }{
        fun => TyVal::unchecked(Invalid, Function(Box::new(Tuple(vec![])), Box::new(Named(int)), Some(closure)))
        y => TyVal::unchecked(Int(1), Named(int))
    }{
        x => TyVal::unchecked(Invalid, Named(int))
    });
}


#[test]
fn interpret_closure_closure() {
    let mut interner = Interner::new();
    let int = interner.intern("I32");
    let closure = interner.intern("fun¤0");
    assert_parse!(interner {
        let fun: (I32 -> I32) = {
            let x: I32 = 1
            [y: I32,] -> I32 {
                let z: ([,] -> I32) = [,] -> I32 {
                    let x: [I32, I32,] = [2, y,]
                    (x[1] + x[0])
                }
                (z[,] + x)
            }
        }
        let y: I32 = fun[3,]
    }{
        fun => TyVal::unchecked(Invalid, Function(Box::new(Named(int)), Box::new(Named(int)), Some(closure)))
        y => TyVal::unchecked(Int(6), Named(int))
    }{
        x => TyVal::unchecked(Invalid, Named(int))
    });
}
