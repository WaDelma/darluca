use symtern::prelude::*;

use itertools::Itertools;

use std::collections::HashSet;
use std::mem::replace;
use std::iter::once;
use std::fmt;

use parser::ast::{self, Ast, Expression, Type};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::Interner;

use self::Value::*;
use self::InterpreterError::*;
use self::mem::Memory;

#[cfg(test)]
mod test;
mod mem;

type Result<T> = ::std::result::Result<T, InterpreterError>;

#[derive(Debug, Fail, PartialEq)]
pub enum InterpreterError {
    #[fail(display = "Cannot assign non-existent variable {} with value {}", _0, _1)]
    NonExistentAssign(String, String),
    #[fail(display = "Unknown variable: {}", _0)]
    UnknownVariable(String),
    #[fail(display = "ICE: Interning failed: {:?}", _0)]
    InternFailure(#[cause] ::symtern::Error)
}

impl From<::symtern::Error> for InterpreterError {
    fn from(e: ::symtern::Error) -> Self {
        InternFailure(e)
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Invalid => f.write_str("Invalid"),
            Tup(ref v) => {
                f.write_str("[")?;
                f.write_str(
                    &v.iter()
                        .map(|v| format!("{}", v))
                        .join(", ")
                )?;
                f.write_str(",]")
            },
            Uni(ref index, ref val, ref size) => {
                f.write_str("[")?;
                for _ in 0..*index {
                    f.write_str("_|")?;
                }
                f.write_str(&format!("{}", val))?;
                f.write_str("|")?;
                for _ in (index + 1)..*size {
                    f.write_str("_|")?;
                }
                f.write_str("]")
            },
            Int(ref i) => f.write_str(&format!("{}", i)),
            Bool(ref b) => f.write_str(&format!("{}", b)),
            Fun(ref params, ref exprs, ref closed) => {
                // TODO: Proper function formating
                f.write_str("Function")
            },
        }
    }
}

pub fn interpret(ast: Ast, interner: &Interner) -> Result<Value> {
    interpret_scope(&ast.expressions, &mut Memory::new(), interner)
}

fn interpret_scope(expressions: &[Expression], memory: &mut Memory<Value>, interner: &Interner) -> Result<Value> {
    memory.scope(|memory| {
        let mut value = Tup(vec![]);
        for e in expressions {
            value = execute(e, memory, interner)?;
        }
        Ok(value)
    })
}

fn execute(e: &Expression, memory: &mut Memory<Value>, interner: &Interner) -> Result<Value> {
    Ok(match *e {
        Scope { ref expressions } => interpret_scope(expressions, memory, interner)?,
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
                .unwrap_or(Ok(Invalid))?;
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
            memory.update(i, Invalid).ok_or(UnknownVariable(interner.resolve(i.0)?.into()))?
        },
        Tuple { ref value } => Tup(
            value.iter()
                .map(|e| execute(e, memory, interner))
                .collect::<Result<_>>()?
        ),
        Union(ref u) => if let Some(ref u) = *u {
            let value = execute(&*u.value, memory, interner)?;
            Uni(u.position, Box::new(value), u.size)
        } else {
            Uni(0, Box::new(Invalid), 0)
        },
        If(ref branch) => {
            match *branch {
                Condition { ref condition, ref expressions, ref otherwise } => {
                    execute_if(condition, expressions, otherwise, memory, interner)?
                }
                _ => panic!("else without if."),
            }
        },
        Operation(ref o) => match *o {
            Assignment { ref identifier, ref value } => {
                let value = execute(value, memory, interner)?;
                // TODO: Typecheck
                let target = memory.get_mut(identifier).ok_or(NonExistentAssign(interner.resolve(identifier.0)?.into(), format!("{}", value)))?;
                replace(target, value)
            },
            Addition { ref parameters } => Int(
                parameters.iter()
                    .map(|e| execute(e, memory, interner))
                    .fold_results(0, |a, b|
                        if let Int(b) = b {
                            a + b
                        } else {
                            panic!("Cannot add: {:?} + {:?}", a, b);
                        }
                    )?
            ),
            Indexing { ref target, ref index, } => {
                let index = execute(index, memory, interner)?;
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
                                .map(|e| execute(e, memory, interner))
                                .collect::<Result<Vec<_>>>()?.into_iter()
                            )
                            .chain(closed.into_iter())
                            .collect();
                        interpret_scope(&exprs[..], &mut memory, interner)?
                    }
                    _ => panic!("Cannot call"),
                }
            },
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    })
}

fn execute_if(condition: &Expression, expressions: &[Expression], otherwise: &ast::If, memory: &mut Memory<Value>, interner: &Interner) -> Result<Value> {
    let condition = match execute(condition, memory, interner)? {
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