use symtern::prelude::*;

use std::collections::HashMap;
use std::mem::replace;

use parser::ast::{self, Ast, Expression};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use interner::{Interner, Symbol};

use self::Value::*;

#[cfg(test)]
mod test;

struct Memory {
    scopes: Vec<HashMap<ast::Identifier, Value>>,
    #[cfg(test)]
    used_scopes: Vec<HashMap<ast::Identifier, Value>>,
}

impl Memory {
    fn new() -> Self {
        Memory {
            scopes: vec![],
            #[cfg(test)]
            used_scopes: vec![],
        }
    }

    fn get(&self, i: &ast::Identifier) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let r @ Some(_) = scope.get(i) {
                return r;
            }
        }
        None
    }

    fn get_mut(&mut self, i: &ast::Identifier) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let r @ Some(_) = scope.get_mut(i) {
                return r;
            }
        }
        None
    }

    fn insert(&mut self, i: ast::Identifier, v: Value) -> Option<Value> {
        self.scopes.last_mut().and_then(|l| l.insert(i, v))
    }

    fn scope<F, T>(&mut self, f: F) -> T
        where F: FnOnce(&mut Self) -> T
    {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let used_scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(
            used_scope.expect("We pushed scope so this should be always valid.")
        );
        result
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Invalid,
    Initial,
    Tup(Vec<Value>),
    Nat(usize),
    Bool(bool),
    Uni(usize, Box<Value>, usize)
}


pub fn interpret(ast: Ast, interner: &Interner) -> Value {
    let mut memory = Memory::new();
    interpret_scope(&ast.expressions, &mut memory, interner)
}

fn interpret_scope(expressions: &[Expression], memory: &mut Memory, interner: &Interner) -> Value {
    memory.scope(|memory| {
        let mut value = Tup(vec![]);
        for e in expressions {
            value = execute(e, memory, interner);
        }
        value
    })
}

fn execute(e: &Expression, memory: &mut Memory, interner: &Interner) -> Value {
    match *e {
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let n = interner.resolve(*n).expect("No such literal");
                Nat(str::parse(n).expect("Literal couldn't be parsed to integer"))
            },
            Boolean(ref b) => {
                Bool(*b)
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
        },
        Scope { ref expressions } => {
            interpret_scope(expressions, memory, interner)
        },
        If { ref condition, ref expressions, ref elses } => {
            let condition = match execute(condition, memory, interner) {
                Bool(b) => b,
                _ => panic!("Invalid condition."),
            };
            interpret_scope(if condition {
                expressions
            } else {
                elses
            }, memory, interner)
        },
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
