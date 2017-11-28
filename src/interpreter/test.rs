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
        let x = (1 + 2)
    }{
        x => Nat(3)
    });
}

#[test]
fn interpret_add_from_variable() {
    assert_parse!({
        let y = 2
        let x = (3 + y)
    }{
        y => Invalid
        x => Nat(5)
    });
}

#[test]
fn interpret_assign() {
    assert_parse!({
        let x = 1
        x = 2
    }{
        x => Nat(2)
    });
}

#[test]
fn interpret_multiple_declarations() {
    assert_parse!({
        let x = 1
        let y = 2
    }{
        x => Nat(1)
        y => Nat(2)
    });
}

#[test]
fn interpret_move() {
    assert_parse!({
        let x = 1
        let y = x
    }{
        x => Invalid
        y => Nat(1)
    });
}

#[test]
fn interpret_move_on_assign() {
    assert_parse!({
        let y = 1
        let x = y = 2
    }{
        x => Nat(1)
        y => Nat(2)
    });
}

#[test]
fn interpret_declaration_declaration() {
    assert_parse!({
        let x = let y = 1
    }{
        x => Tup(vec![])
        y => Nat(1)
    });
}

#[test]
fn interpret_tuple() {
    assert_parse!({
        let x = (1, 2, 3)
    }{
        x => Tup(vec![
                Nat(1),
                Nat(2),
                Nat(3)
            ])
    });
}

#[test]
fn interpret_tuple_tuple() {
    assert_parse!({
        let x = (((1 + 1)))
    }{
        x => Tup(vec![
                Tup(vec![
                    Nat(2)
                ])
            ])
    });
}

#[test]
fn interpret_tuple_indexing() {
    assert_parse!({
        let x = (1, 2, 3)
        let y = x[1]
    }{
        x => Tup(vec![
                Nat(1),
                Invalid,
                Nat(3)
            ])
        y => Nat(2)
    });
}

#[test]
fn interpret_union() {
    assert_parse!({
        let x = (1|_|_)
    }{
        x => Uni(0, Box::new(Nat(1)), 3)
    });
}

#[test]
fn interpret_union_union() {
    assert_parse!({
        let x = ((_|2)|_|_)
    }{
        x => Uni(0,Box::new(
                Uni(1, Box::new(
                    Nat(2)
                ), 2)
            ), 3)
    });
}

#[test]
fn interpret_scope() {
    assert_parse!({
        let y = {
            let x = 1
            2
        }
    }{
        y => Nat(2)
    }{
        x => Nat(1)
    });
}

#[test]
fn interpret_boolean() {
    assert_parse!({
        let x = true
        let y = false
    }{
        x => Bool(true)
        y => Bool(false)
    });
}

#[test]
fn interpret_if() {
    assert_parse!({
        let x = if true {
            1
        } else {
            2
        }
    }{
        x => Nat(1)
    });
}

#[test]
fn interpret_else() {
    assert_parse!({
        let x = if false {
            1
        } else {
            2
        }
    }{
        x => Nat(2)
    });
}
