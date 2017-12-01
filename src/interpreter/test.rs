use interner::Interner;
use lexer::Lexer;

use super::Value::*;

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
            ::interpreter::interpret_scope(&ast.expressions, &mut memory, &$interner);
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
            let mut interner = Interner::new();
            assert_parse!(interner $($tks)*);
        }
    };
}

#[test]
fn interpret_add_and_declaration() {
    assert_parse!({
        let x: I32 = (1 + 2)
    }{
        x => Int(3)
    });
}

#[test]
fn interpret_add_from_variable() {
    assert_parse!({
        let y: I32 = 2
        let x: I32 = (3 + y)
    }{
        y => Invalid
        x => Int(5)
    });
}

#[test]
fn interpret_assign() {
    assert_parse!({
        let x: I32 = 1
        x = 2
    }{
        x => Int(2)
    });
}

#[test]
fn interpret_multiple_declarations() {
    assert_parse!({
        let x: I32 = 1
        let y: I32 = 2
    }{
        x => Int(1)
        y => Int(2)
    });
}

#[test]
fn interpret_move() {
    assert_parse!({
        let x: I32 = 1
        let y: I32 = x
    }{
        x => Invalid
        y => Int(1)
    });
}

#[test]
fn interpret_move_on_assign() {
    assert_parse!({
        let y: I32 = 1
        let x: I32 = y = 2
    }{
        x => Int(1)
        y => Int(2)
    });
}

#[test]
fn interpret_declaration_declaration() {
    assert_parse!({
        let x: () = let y: I32 = 1
    }{
        x => Tup(vec![])
        y => Int(1)
    });
}

#[test]
fn interpret_tuple() {
    assert_parse!({
        let x: (I32, I32, I32) = (1, 2, 3)
    }{
        x => Tup(vec![
                Int(1),
                Int(2),
                Int(3)
            ])
    });
}

#[test]
fn interpret_tuple_tuple() {
    assert_parse!({
        let x: ((I32)) = (((1 + 1)))
    }{
        x => Tup(vec![
                Tup(vec![
                    Int(2)
                ])
            ])
    });
}

#[test]
fn interpret_tuple_indexing() {
    assert_parse!({
        let x: (I32, I32, I32) = (1, 2, 3)
        let y: I32 = x[1]
    }{
        x => Tup(vec![
                Int(1),
                Invalid,
                Int(3)
            ])
        y => Int(2)
    });
}

#[test]
fn interpret_union() {
    assert_parse!({
        let x: (I32|I32|I32) = (1|_|_)
    }{
        x => Uni(0, Box::new(Int(1)), 3)
    });
}

#[test]
fn interpret_union_union() {
    assert_parse!({
        let x: ((I32|I32)|I32|I32) = ((_|2)|_|_)
    }{
        x => Uni(0,Box::new(
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
        let x: bool = true
        let y: bool = false
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
