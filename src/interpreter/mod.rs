use symtern::prelude::*;

use itertools::Itertools;

use std::collections::HashSet;
use std::mem::replace;
use std::iter::once;

use parser::ast::{self, Ast, Expression};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::Interner;

use self::InterpreterError::*;
use self::repr::{Value, TypedValue, Ty, terminal, invalid, int_typed, bool_typed, union_typed, tuple_typed};
use self::repr::Value::*;
use self::repr::mem::Memory;

#[cfg(test)]
mod test;
mod repr;

type Result<T> = ::std::result::Result<T, InterpreterError>;

#[derive(Debug, Fail, PartialEq, Clone)]
pub enum InterpreterError {
    #[fail(display = "Cannot assign non-existent variable {} with value {} of type {}", _0, _1, _2)]
    NonExistentAssign(String, String, String),
    #[fail(display = "Cannot access unknown variable {}", _0)]
    UnknownVariable(String),
    #[fail(display = "Cannot capture unknown variable {}", _0)]
    UnknownCapture(String),
    #[fail(display = "Wrong value {} of type {} for type {}", _0, _1, _2)]
    ValueTypeMismatch(String, String, String),
    #[fail(display = "ICE: Interning failed: {:?}", _0)]
    InternFailure(#[cause] ::symtern::Error)
}

impl From<::symtern::Error> for InterpreterError {
    fn from(e: ::symtern::Error) -> Self {
        InternFailure(e)
    }
}

pub fn interpret(ast: Ast, interner: &mut Interner) -> Result<TypedValue> {
    interpret_scope(&ast.expressions, &mut Memory::new(), interner)
}

fn interpret_scope(expressions: &[Expression], memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
    memory.scope(|memory| {
        let mut value = terminal();
        for e in expressions {
            value = execute(e, memory, interner)?;
        }
        Ok(value)
    })
}

fn execute(e: &Expression, memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
    Ok(match *e {
        Scope { ref expressions } => interpret_scope(expressions, memory, interner)?,
        Function { ref params, ref expressions, ref parameter_ty, ref return_ty } => Memory::new().scope(|mut closed| {
            let mut free = HashSet::new();
            for p in params.iter() {
                closed.create(*p, ());
            }
            find_free_variables(&mut free, &mut closed, expressions);
            let closed_over = free.iter()
                .map(|f| {
                    let c = memory.replace(f, invalid(), interner)
                        .unwrap_or(Err(UnknownCapture(interner.resolve(f.0)?.into())))?;
                    Ok((*f, c))
                }).collect::<Result<_>>()?;
            let fun = Value::Fun(params.clone(), expressions.clone(), closed_over);
            // TODO: Closure vs function
            TypedValue::new(fun, Ty::Function(Box::new(Ty::from(parameter_ty)), Box::new(Ty::from(return_ty))), interner)
        })?,
        Declaration { ref identifier, ref value, ref ty } => {
            // TODO: Shadowing
            let place = TypedValue::new(Invalid, Ty::from(ty), interner)?;
            let val = value
                .as_ref()
                .map(|v| execute(&*v, memory, interner))
                .unwrap_or(Ok(invalid()))?;
            memory.create(*identifier, place.assign(val, interner)?);
            terminal()
        },
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let val = {
                    let n = interner.resolve(*n).expect("No such literal");
                    str::parse(n).expect("Literal couldn't be parsed to integer")
                };
                int_typed(val, interner)?
            },
            Boolean(ref b) => {
                bool_typed(*b, interner)?
            }
        },
        Identifier(ref i) => {
            memory.replace(i, invalid(), interner)
                .unwrap_or(Err(UnknownVariable(interner.resolve(i.0)?.into())))?
        },
        Tuple { ref value } => {
            tuple_typed(value.iter()
                .map(|e| execute(e, memory, interner))
                .collect::<Result<_>>()?)
        },
        Union(ref u) => if let Some(ref u) = *u {
            let value = execute(&*u.value, memory, interner)?;
            union_typed(u.position, value, u.size)
        } else {
            panic!("Initial cannot be constructed!");
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
                let n = NonExistentAssign(
                    interner.resolve(identifier.0)?.into(),
                    value.value().display(interner)?,
                    value.ty().display(interner)?,
                );
                memory.replace(identifier, value, interner).unwrap_or(Err(n))?
            },
            Addition { ref parameters } => {
                int_typed(parameters.iter()
                    .map(|e| execute(e, memory, interner))
                    .fold_results(0, |a, b| {
                        let bd = format!("{:?}", b);
                        if let Int(b) = b.into_value() {
                            a + b
                        } else {
                            panic!("Cannot add: {:?} + {}", a, bd);
                        }
                    })?, interner)?
            },
            Indexing { ref target, ref index, } => {
                let index = execute(index, memory, interner)?;
                let container = memory.get_mut(target).expect("Indexable should exists");
                // TODO: This is ugly. Refactor pls.
                match container.split_mut_and_ref() {
                    (&mut Tup(ref mut value), &Ty::Tuple(ref ty)) => {
                        let index = match index.into_value() {
                            Int(n) => n,
                            _ => panic!("Cannot index with non-integer."),
                        };
                        let value = value.get_mut(index as usize)
                            .expect("Out of bounds access");
                        let ty = ty.get(index as usize).expect("Type should've been checked");
                        TypedValue::new(replace(value, Invalid), ty.clone(), interner)?
                    }
                    _ => panic!("Cannot index"),
                }
            },
            Calling { ref name, ref parameters } => {
                let fun = memory.replace(name, invalid(), interner).expect("Function should exists")?;
                match fun.into_value() {
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
        },
    })
}

fn execute_if(condition: &Expression, expressions: &[Expression], otherwise: &ast::If, memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
    let condition = execute(condition, memory, interner)?;
    let condition = match condition.into_value() {
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
            Function { ref params, ref expressions, ..} => {
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