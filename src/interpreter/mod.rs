use symtern::prelude::*;

use std::collections::HashSet;
use std::mem::replace;
use std::iter::once;

use parser::ast::{self, Ast, Expression, Type};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::Interner;

use self::Value::*;
use self::mem::Memory;

#[cfg(test)]
mod test;
mod mem;

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
    interpret_scope(&ast.expressions, &mut Memory::new(), interner)
}

fn interpret_scope(expressions: &[Expression], memory: &mut Memory<Value>, interner: &Interner) -> Value {
    memory.scope(|memory| {
        let mut value = Tup(vec![]);
        for e in expressions {
            value = execute(e, memory, interner);
        }
        value
    })
}

fn execute(e: &Expression, memory: &mut Memory<Value>, interner: &Interner) -> Value {
    match *e {
        Scope { ref expressions } => interpret_scope(expressions, memory, interner),
        Function { ref params, ref expressions } => Memory::new().scope(|mut closed| {
            let mut free = HashSet::new();
            for p in params.iter() {
                closed.create(*p, ());
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
        If(ref branch) => {
            match *branch {
                Condition { ref condition, ref expressions, ref otherwise } => {
                    execute_if(condition, expressions, otherwise, memory, interner)
                }
                _ => panic!("else without if."),
            }
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
                            panic!("Cannot add: {:?} + {:?}", a, b);
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
                            .zip(parameters.iter()
                                    .map(|e| execute(e, memory, interner)))
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

fn execute_if(condition: &Expression, expressions: &[Expression], otherwise: &ast::If, memory: &mut Memory<Value>, interner: &Interner) -> Value {
    let condition = match execute(condition, memory, interner) {
        Bool(b) => b,
        _ => panic!("Invalid condition."),
    };
    if condition {
        interpret_scope(expressions, memory, interner)
    } else {
        match *otherwise {
            Condition { ref condition, ref expressions, ref otherwise } => {
                execute_if(condition, expressions, otherwise, memory, interner)
            },
            Else(ref otherwise) => {
                interpret_scope(otherwise, memory, interner)
            },
        }
    }
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
        // TODO: Typecheck functions
        Fun(_, _, _) => if let Type::Function(_, _) = *ty {
            true
        } else {
            false
        },
    }
}

fn add_not_closed(free: &mut HashSet<ast::Identifier>, closed: &Memory<()>, identifier: &ast::Identifier) {
    if closed.get(identifier).is_none() {
        free.insert(*identifier);
    }
}

fn find_free_variables<'a, I>(free: &mut HashSet<ast::Identifier>, closed: &mut Memory<()>, exprs: I)
    where I: IntoIterator<Item=&'a Expression>,
{
    for e in exprs {
        match *e {
            Identifier(ref i) => add_not_closed(free, &*closed, i),
            Operation(ref op) => match *op {
                Assignment { ref identifier, ref value, } => {
                    add_not_closed(free, &*closed, identifier);
                    find_free_variables(free, closed, once(&**value));
                },
                Addition { ref parameters, } => {
                    find_free_variables(free, closed, &parameters[..]);
                },
                Indexing { ref target, ref index, } => {
                    add_not_closed(free, &*closed, target);
                    find_free_variables(free, closed, once(&**index));
                },
                Calling { ref name, ref parameters, } => {
                    add_not_closed(free, &*closed, name);
                    find_free_variables(free, closed, &parameters[..]);
                },
            },
            Declaration { ref identifier, ref value, ..} => {
                if let Some(ref e) = *value {
                    find_free_variables(free, closed, once(&**e));
                }
                closed.create(*identifier, ());
            },
            Function { ref params, ref expressions, } => {
                closed.scope(|closed| {
                    for p in params {
                        closed.create(*p, ());
                    }
                    find_free_variables(free, closed, expressions);
                });
            },
            If(ref branch) => {
                match *branch {
                    Condition { ref condition, ref expressions, ref otherwise } =>
                        find_free_variables_if(&**condition, expressions, otherwise, free, closed),
                    _ => panic!("else without if."),
                }
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

fn find_free_variables_if(condition: &Expression, expressions: &[Expression], otherwise: &ast::If, free: &mut HashSet<ast::Identifier>, closed: &mut Memory<()>) {
    find_free_variables(free, closed, once(condition));
    closed.scope(|closed|
        find_free_variables(free, closed, &expressions[..]));
    match *otherwise {
        Condition { ref condition, ref expressions, ref otherwise } => {
            find_free_variables_if(condition, expressions, otherwise, free, closed);
        },
        Else(ref otherwise) => {
            closed.scope(|closed|
                find_free_variables(free, closed, &otherwise[..]));
        },
    }
}