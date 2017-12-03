use symtern::prelude::*;

use std::collections::{HashMap, HashSet};
use std::mem::replace;
use std::iter::{once, FromIterator};

use parser::ast::{self, Ast, Expression, Type};
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

    fn create(&mut self, i: ast::Identifier, v: Value) -> Option<Value> {
        self.scopes.last_mut().and_then(|l| l.insert(i, v))
    }

    fn update(&mut self, i: &ast::Identifier, v: Value) -> Option<Value> {
        self.get_mut(i).map(|va| replace(va, v))
    }

    fn scope<F, T>(&mut self, f: F) -> T
        where F: FnOnce(&mut Self) -> T
    {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let _used_scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(
            _used_scope.expect("We pushed scope so this should be always valid.")
        );
        result
    }
}

impl FromIterator<(ast::Identifier, Value)> for Memory {
    fn from_iter<I>(iter: I) -> Memory
        where I: IntoIterator<Item=(ast::Identifier, Value)>
    {
        let mut mem = Memory::new();
        mem.scopes.push(iter.into_iter().collect());
        mem
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Invalid,
    Tup(Vec<Value>),
    Uni(usize, Box<Value>, usize),
    Int(i32),
    Bool(bool),
    Fun(Vec<ast::Identifier>, Vec<Expression>, Vec<(ast::Identifier, Value)>),
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

fn type_matches(val: &Value, ty: &Type, interner: &Interner) -> bool {
    use self::Value::*;
    match *val {
        Invalid => panic!("Invalid value"),
        Tup(ref v) => if let Type::Tuple(ref t) = *ty {
            v.iter().zip(t.iter())
                .all(|(v, t)| type_matches(v, t, interner))
        } else {
            false
        },
        Uni(ref index, ref v, ref size) => if let Type::Union(ref t) = *ty {
            if t.len() == *size {
                type_matches(&v, &t[*index], interner)
            } else {
                false
            }
        } else {
            false
        },
        Int(_) => if let Type::Named(ref n) = *ty {
            "I32" == interner.resolve(*n).unwrap()
        } else {
            false
        },
        Bool(_) => if let Type::Named(ref n) = *ty {
            "Bool" == interner.resolve(*n).unwrap()
        } else {
            false
        },
        Fun(_, _, _) => if let Type::Function(_, _) = *ty {
            true
        } else {
            false
        },
    }
}

fn find_free_variables<'a, I>(free: &mut HashSet<ast::Identifier>, closed: &mut Memory, exprs: I)
    where I: IntoIterator<Item=&'a Expression>,
{
    for e in exprs {
        match *e {
            Identifier(ref i) => if closed.get(i).is_none() {
                free.insert(*i);
            },
            Operation(ref op) => match *op {
                Assignment { ref identifier, ref value, } => {
                    if closed.get(identifier).is_none() {
                        free.insert(*identifier);
                    }
                    find_free_variables(free, closed, once(&**value));
                },
                Addition { ref parameters, } => {
                    find_free_variables(free, closed, &parameters[..]);
                },
                Indexing { ref target, ref index, } => {
                    if closed.get(target).is_none() {
                        free.insert(*target);
                    }
                    find_free_variables(free, closed, once(&**index));
                },
                Calling { ref name, ref parameters, } => {
                    if closed.get(name).is_none() {
                        free.insert(*name);
                    }
                    find_free_variables(free, closed, &parameters[..]);
                },
            },
            Declaration { ref identifier, ref value, ..} => {
                if let Some(ref e) = *value {
                    find_free_variables(free, closed, once(&**e));
                }
                closed.create(*identifier, Invalid);
            },
            Function { ref params, ref expressions, } => {
                closed.scope(|closed| {
                    for p in params {
                        closed.create(*p, Invalid);
                    }
                    find_free_variables(free, closed, expressions);
                });
            },
            If { ref condition, ref expressions, ref elses, } => {
                find_free_variables(free, closed, once(&**condition));
                closed.scope(|closed|
                    find_free_variables(free, closed, &expressions[..]));
                closed.scope(|closed|
                    find_free_variables(free, closed, &elses[..]));
            },
            Tuple { ref value, } =>
                find_free_variables(free, closed, &value[..]),
            Union(Some(ref u)) =>
                find_free_variables(free, closed, once(&*u.value)),
            Scope { ref expressions, } =>
                closed.scope(|closed|
                    find_free_variables(free, closed, expressions)),
            _ => {}
        }
    }
}

fn execute(e: &Expression, memory: &mut Memory, interner: &Interner) -> Value {
    match *e {
        Scope { ref expressions } => interpret_scope(expressions, memory, interner),
        Function { ref params, ref expressions } => Memory::new().scope(|mut closed| {
            let mut free = HashSet::new();
            for p in params.iter() {
                closed.create(*p, Invalid);
            }
            find_free_variables(&mut free, &mut closed, expressions);
            let closed_over = free.iter()
                .map(|f| {
                    let c = memory.update(f, Invalid)
                        .expect(&format!("Failed to close over variable: {:?} => {:?}", f, interner.resolve(f.0)));
                    (*f, c)
                }).collect();
            Value::Fun(params.clone(), expressions.clone(), closed_over)
        }),
        Declaration { ref identifier, ref value, ref ty } => {
            // TODO: Shadowing
            let value = value
                .as_ref()
                .map(|v| execute(&*v, memory, interner))
                .unwrap_or(Invalid);
            if let Some(ref ty) = *ty {
                if !type_matches(&value, ty, interner) {
                    panic!("Trying to assign wrong type of value. {:?} is not of type {:?}", value, ty);
                }
            }
            memory.create(*identifier, value);
            Tup(vec![])
        },
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let n = interner.resolve(*n).expect("No such literal");
                Int(str::parse(n).expect("Literal couldn't be parsed to integer"))
            },
            Boolean(ref b) => {
                Bool(*b)
            }
        },
        Identifier(ref i) => {
            memory.update(i, Invalid).expect(&format!("No such variable: {:?} => {:?}", i.0, interner.resolve(i.0)))
        },
        Tuple { ref value } => Tup(
            value.iter()
                .map(|e| execute(e, memory, interner))
                .collect()
        ),
        Union(ref u) => if let Some(ref u) = *u {
            let value = execute(&*u.value, memory, interner);
            Uni(u.position, Box::new(value), u.size)
        } else {
            Uni(0, Box::new(Invalid), 0)
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
            Assignment { ref identifier, ref value } => {
                let value = execute(value, memory, interner);
                let target = memory.get_mut(identifier)
                    .expect("Invalid variable.");
                replace(target, value)
            },
            Addition { ref parameters } => Int(
                parameters.iter()
                    .fold(0, |a, b|
                        if let Int(b) = execute(b, memory, interner) {
                            a + b
                        } else {
                            panic!("Cannot add");
                        }
                    )
            ),
            Indexing { ref target, ref index, } => {
                let index = execute(index, memory, interner);
                match *memory.get_mut(target).expect("Indexable should exists") {
                    Tup(ref mut value) => {
                        let index = match index {
                            Int(n) => n,
                            _ => panic!("Cannot index with non-integer."),
                        };
                        let value = value.get_mut(index as usize)
                            .expect("Out of bounds access");
                        replace(value, Invalid)
                    }
                    _ => panic!("Cannot index"),
                }
            },
            Calling { ref name, ref parameters } => {
                match memory.update(name, Invalid).expect("Function should exists") {
                    Fun(params, exprs, closed) => {
                        let mut memory = params.into_iter()
                            .zip(
                                parameters.iter()
                                    .map(|e| execute(e, memory, interner))
                            )
                            .chain(closed.into_iter())
                            .collect();
                        interpret_scope(&exprs[..], &mut memory, interner)
                    }
                    _ => panic!("Cannot call"),
                }
            },
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
