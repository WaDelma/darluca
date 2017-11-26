use symtern::prelude::*;

use std::collections::HashMap;

use parser::ast::{self, Ast, Expression};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use interner::{Interner, Symbol};

#[cfg(test)]
mod test;

pub fn interpret(ast: Ast, interner: &Interner, memory: &mut HashMap<ast::Identifier, Value>) {
    for e in &ast.expressions {
        execute(e, memory, interner);
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Invalid,
    Initial,
    Tup(Vec<Value>),
    Nat(usize),
}

fn execute(e: &Expression, memory: &mut HashMap<ast::Identifier, Value>, interner: &Interner) -> Value {
    use self::Value::*;
    match *e {
        Literal(ref l) => match *l {
            Integer(ref n) => Nat(str::parse(interner.resolve(*n).unwrap()).unwrap())
        },
        Identifier(ref i) => memory.insert(*i, Invalid).unwrap(),
        Tuple { ref value } => {
            Tup(value.iter()
                .map(|e| execute(e, memory, interner))
                .collect() 
            )
        }
        Operation(ref o) => match *o {
            Assignment {
                ref identifier,
                ref value
            } => {
                // TODO: Shadowing
                let value = execute(value, memory, interner);
                memory.insert(*identifier, value);
                Tup(vec![])
            },
            Addition {
                ref parameters
            } => {
                Nat(parameters.iter()
                    .fold(0, |a, b| {
                        if let Nat(b) = execute(b, memory, interner) {
                            a + b
                        } else {
                            panic!("Cannot add");
                        }
                    }))
            }
        },
        _ => unimplemented!(),
    }
}
