use symtern::prelude::*;

use interner::Interner;
use lexer::Lexer;

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
            use parser::ast::Identifier;
            use symtern::prelude::*;
            use std::collections::HashMap;

            let tokens = {
                ::lexer::Lexer::new(&mut $interner).tokenize(
                    stringify!($($code)*).as_bytes()
                ).1.unwrap().1
            };
            let ast = ::parser::parse(tokens.borrow()).unwrap().1;
            let mut memory = Memory::new();
            ::interpreter::interpret_scope(&ast.expressions, &mut memory, &mut $interner).unwrap();
            let mut n = 1;
            $(
                let mut expected = HashMap::new();
                $(
                    let interned = $interner.intern(stringify!($var)).unwrap();
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
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: I32 = (1 + 2)
    }{
        x => TyVal::unchecked(Int(3), Named(int))
    });
}

#[test]
fn interpret_add_from_variable() {
    let mut interner = Interner::new();
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let y: I32 = 2
        let x: I32 = (3 + y)
    }{
        y => TyVal::unchecked(Invalid, Unknown)
        x => TyVal::unchecked(Int(5), Named(int))
    });
}

#[test]
fn interpret_assign() {
    let mut interner = Interner::new();
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
    assert_parse!(interner {
        let x: I32 = 1
        let y: I32 = x
    }{
        x => TyVal::unchecked(Invalid, Unknown)
        y => TyVal::unchecked(Int(1), Named(int))
    });
}

#[test]
fn interpret_move_on_assign() {
    let mut interner = Interner::new();
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
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
    let int = interner.intern("I32").unwrap();
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
                Unknown,
                Named(int),
            ]))
        y => TyVal::unchecked(Int(2), Named(int))
    });
}

/*#[test]
fn interpret_union() {
    assert_parse!({
        let x: [I32|I32|I32|] = [1|_|_|]
    }{
        x => Uni(0, Box::new(Int(1)), 3)
    });
}

#[test]
fn interpret_union_union() {
    assert_parse!({
        let x: [[I32|I32|]|I32|I32|] = [[_|2|]|_|_|]
    }{
        x => Uni(0, Box::new(
                Uni(1, Box::new(
                    Int(2)
                ), 2)
            ), 3)
    });
}

#[test]
fn interpret_scope() {
    assert_parse!({
        let y: I32 = {
            let x: I32 = 1
            2
        }
    }{
        y => Int(2)
    }{
        x => Int(1)
    });
}

#[test]
fn interpret_boolean() {
    assert_parse!({
        let x: Bool = true
        let y: Bool = false
    }{
        x => Bool(true)
        y => Bool(false)
    });
}

#[test]
fn interpret_if() {
    assert_parse!({
        let x: I32 = if true {
            1
        } else {
            2
        }
    }{
        x => Int(1)
    });
}

#[test]
fn interpret_else() {
    assert_parse!({
        let x: I32 = if false {
            1
        } else {
            2
        }
    }{
        x => Int(2)
    });
}

#[test]
fn interpret_else_if() {
    assert_parse!({
        let x: I32 = if false {
            1
        } else if false {
            2
        } else {
            3
        }
    }{
        x => Int(3)
    });
}

#[test]
fn intepret_moving_into_scope() {
    assert_parse!({
        let x: I32 = 1
        let y: I32 = {
            x
        }
    }{
        x => Invalid
        y => Int(1)
    });
}

#[test]
fn interpret_function() {
    assert_parse!({
        let fun: (I32 -> I32) = [x,] -> {
            x
        }
        let y = fun[1,]
    }{
        fun => Invalid
        y => Int(1)
    });
}

#[test]
fn interpret_closure() {
    assert_parse!({
        let fun: ([,] -> I32) = {
            let x: I32 = 1
            [,] -> {
                x
            }
        }
        let y = fun[,]
    }{
        fun => Invalid
        y => Int(1)
    }{
        x => Invalid
    });
}


#[test]
fn interpret_closure_closure() {
    assert_parse!({
        let fun: ([I32,] -> I32) = {
            let x: I32 = 1
            [y,] -> {
                let z = [,] -> {
                    let x: [I32, I32,] = [2, y,]
                    (x[1] + x[0])
                }
                (z[,] + x)
            }
        }
        let y = fun[3,]
    }{
        fun => Invalid
        y => Int(6)
    }{
        x => Invalid
    });
}*/