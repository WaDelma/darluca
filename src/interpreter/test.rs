use interner::Interner;
use lexer::Lexer;

use super::Value::*;

macro_rules! assert_parse {
    ($interner:ident {
        $($code:tt)*
    }{
        $($var:ident => $val:expr),* $(,)*
    }) => {
        {
            use parser::ast::Identifier;
            use symtern::prelude::*;
            
            use std::collections::HashMap;
            let tokens = {
                ::lexer::Lexer::new(&mut $interner).tokenize(
                    stringify!($($code)*).as_bytes()
                ).1.unwrap().1
            };
            let ast = ::parser::parse(tokens.borrow()).unwrap().1;
            let mut memory = HashMap::new();
            ::interpreter::interpret(ast, &$interner, &mut memory);
            let mut expected = HashMap::new();
            $(
                let interned = $interner.intern(stringify!($var)).unwrap();
                expected.insert(Identifier(interned), $val);
            )*
            if memory != expected {
                panic!("Expected:\n{:#?}\nInterpreted:\n{:#?}", expected, memory);
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
fn interpret_add_and_assign() {
    assert_parse!({
        x = (1 + 2)
    }{
        x => Nat(3)
    });
}

#[test]
fn interpret_reassign() {
    assert_parse!({
        x = 1
        x = 2
    }{
        x => Nat(2)
    });
}

#[test]
fn interpret_multiple_assigns() {
    assert_parse!({
        x = 1
        y = 2
    }{
        x => Nat(1),
        y => Nat(2)
    });
}

#[test]
fn interpret_move() {
    assert_parse!({
        x = 1
        y = x
    }{
        x => Invalid,
        y => Nat(1)
    });
}

#[test]
fn interpret_assign_assign() {
    assert_parse!({
        x = y = 1
    }{
        x => Tup(vec![]),
        y => Nat(1)
    });
}

#[test]
fn interpret_tuple() {
    assert_parse!({
        x = (1, 2, 3)
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
        x = (((1 + 1)))
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
        x = (1, 2, 3)
        y = x[1]
    }{
        x => Tup(vec![
                Nat(1),
                Invalid,
                Nat(3)
            ]),
        y => Nat(2),
    });
}

#[test]
fn interpret_union() {
    assert_parse!({
        x = (1|_|_)
    }{
        x => Uni(0, Box::new(Nat(1)), 3)
    });
}

#[test]
fn interpret_union_union() {
    assert_parse!({
        x = ((_|2)|_|_)
    }{
        x => Uni(0,Box::new(
                Uni(1, Box::new(
                    Nat(2)
                ), 2)
            ), 3)
    });
}