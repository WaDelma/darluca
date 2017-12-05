use super::super::InterpreterError::*;


macro_rules! assert_error {
    ($interner:ident {
        $($code:tt)*
    }{
        $val:expr
    }) => {
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
            let error = ::interpreter::interpret(ast, &mut $interner).err().unwrap();
            let expected = $val;
            if error != expected {
                panic!(
                        "Expected:\n{}\nInterpreted:\n{}",
                        expected,
                        error);
            }
        }
    };
    ($($tks:tt)*) => {
        {
            let mut interner = ::interner::Interner::new();
            assert_error!(interner $($tks)*);
        }
    };
}

#[test]
fn using_unknown_variable() {
    assert_error!({
        x
    }{
        UnknownVariable("x".into())
    });
}

#[test]
fn assigning_int_unknown_variable() {
    assert_error!({
        x = 1
    }{
        NonExistentAssign("x".into(), "1".into(), "I32".into())
    });
}

#[test]
fn assigning_bool_unknown_variable() {
    assert_error!({
        x = true
    }{
        NonExistentAssign("x".into(), "true".into(), "Bool".into())
    });
}

#[test]
fn assigning_tuple_unknown_variable() {
    assert_error!({
        x = [1, 2,]
    }{
        NonExistentAssign("x".into(), "[1, 2,]".into(), "[I32, I32,]".into())
    });
}

#[test]
fn assigning_union_unknown_variable() {
    assert_error!({
        x = [1|_|]
    }{
        NonExistentAssign("x".into(), "[1|_|]".into(), "[I32|?|]".into())
    });
}

#[test]
fn assign_bool_literal_to_int_variable() {
    assert_error!({
        let x: I32 = true
    }{
        ValueTypeMismatch("true".into(), "Bool".into(), "I32".into())
    });
}

#[test]
fn assign_tuple_variable_to_int_variable() {
    assert_error!({
        let y: [I32,] = [1,]
        let x: I32
        x = y
    }{
        ValueTypeMismatch("[1,]".into(), "[I32,]".into(), "I32".into())
    });
}