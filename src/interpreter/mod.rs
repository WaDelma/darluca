use symtern::prelude::*;

use std::collections::HashMap;
use std::mem::replace;

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
    Uni(usize, Box<Value>, usize)
}

fn execute(e: &Expression, memory: &mut HashMap<ast::Identifier, Value>, interner: &Interner) -> Value {
    use self::Value::*;
    match *e {
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let n = interner.resolve(*n).expect("No such literal");
                Nat(str::parse(n).expect("Literal couldn't be parsed to integer"))
            }
        },
        Identifier(ref i) => memory.insert(*i, Invalid).expect("No such variable"),
        Tuple { ref value } => {
            Tup(value.iter()
                .map(|e| execute(e, memory, interner))
                .collect() 
            )
        },
        Union(ref u) => {
            if let Some(ref u) = *u {
                let value = execute(&*u.value, memory, interner);
                Uni(u.position, Box::new(value), u.size)
            } else {
                Uni(0, Box::new(Invalid), 0)
            }
        }
        Operation(ref o) => match *o {
            Assignment {
                ref identifier,
                ref value
            } => {
                // TODO: Shadowing/Scoping
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
            },
            Indexing {
                ref target,
                ref index,
            } => {
                let index = execute(index, memory, interner);
                match *memory.get_mut(target).unwrap() {
                    Tup(ref mut value) => {
                        let index = match index {
                            Nat(n) => n,
                            _ => panic!("Cannot index with non-integer."),
                        };
                        let value = value.get_mut(index)
                            .expect("Out of bounds access");
                        replace(value, Invalid)
                    }
                    _ => panic!("Cannot index"),
                }
            }
        },
        _ => unimplemented!(),
    }
}
