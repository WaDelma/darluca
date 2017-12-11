use std::collections::HashSet;
use std::mem::replace;
use std::iter::once;

use parser::ast::{self, Ast, Expression, Expr};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::Interner;
use typechecker::{TypedAst, TypeKey, Type};

use self::InterpreterError::*;
use self::repr::{bool_typed, int_typed, invalid, terminal, tuple_typed, union_typed, Ty,
                 Value};
use self::repr::Value::*;

pub use self::repr::TypedValue;
pub use self::repr::mem::Memory;

#[cfg(test)]
mod test;
mod repr;

type Result<T> = ::std::result::Result<T, InterpreterError>;

#[derive(Debug, Fail, PartialEq, Clone)]
pub enum InterpreterError {
    #[fail(display = "Cannot assign non-existent variable {} with value {} of type {}", _0, _1,
           _2)]
    NonExistentAssign(String, String, String),
    #[fail(display = "Cannot {} unknown {} named {}", action, target, name)]
    Unknown {
        target: String,
        action: String,
        name: String,
    },
    #[fail(display = "Wrong value {} of type {} for type {}", _0, _1, _2)]
    ValueTypeMismatch(String, String, String),
    #[fail(display = "Empty union (aka initial value) cannot be constructed")]
    InitialConstruction,
    #[fail(display = "Value {} of type {} cannot be added to type {}", _0, _1, _2)]
    AddingImpossible(String, String, String),
    #[fail(display = "Value {} of type {} cannot be {}", value, ty, action)]
    Unoperable {
        value: String,
        ty: String,
        action: String,
    },
    #[fail(display = "ICE: Interning failed: {:?}", _0)] InternFailure(#[cause] ::symtern::Error),
}

fn unknown<A: Into<String>, B: Into<String>, C: Into<String>>(target: A, action: B, name: C) -> InterpreterError {
    Unknown {
        target: target.into(),
        action: action.into(),
        name: name.into(),
    }
}

fn unoperable<A: Into<String>, B: Into<String>, C: Into<String>>(value: A, ty: B, action: C) -> InterpreterError {
    Unoperable {
        value: value.into(),
        ty: ty.into(),
        action: action.into(),
    }
}

impl From<::symtern::Error> for InterpreterError {
    fn from(e: ::symtern::Error) -> Self {
        InternFailure(e)
    }
}

pub fn interpret(ast: &TypedAst, interner: &mut Interner) -> Result<TypedValue> {
    interpret_scope(&ast.expressions, &mut Memory::new(), interner)
}

pub fn interpret_noscope(
    expressions: &[Expr<TypeKey>],
    memory: &mut Memory<TypedValue>,
    interner: &mut Interner,
) -> Result<TypedValue> {
    let mut value = terminal();
    for e in expressions {
        value = execute(e, memory, interner)?;
    }
    Ok(value)
}

fn interpret_scope(
    expressions: &[Expr<TypeKey>],
    memory: &mut Memory<TypedValue>,
    interner: &mut Interner,
) -> Result<TypedValue> {
    memory.scope(|memory| {
        let mut value = terminal();
        for e in expressions {
            value = execute(e, memory, interner)?;
        }
        Ok(value)
    })
}

fn execute(
    e: &Expr<TypeKey>,
    memory: &mut Memory<TypedValue>,
    interner: &mut Interner,
) -> Result<TypedValue> {
    Ok(match e.expression {
        Scope { ref expressions } => interpret_scope(expressions, memory, interner)?,
        Function {
            ref params,
            ref expressions,
            ref parameter_ty,
            ref return_ty,
        } => Memory::new().scope(|mut closed| {
            let mut free = HashSet::new();
            for p in params.iter() {
                closed.create(*p, ());
            }
            find_free_variables(&mut free, &mut closed, expressions);
            let closed_over = free.iter()
                .map(|f| {
                    let c = move_or_copy(f, memory, interner).map_err(|mut e| {
                        if let Unknown { ref mut target, ref mut action, ..} = e {
                            *target = "variable".into();
                            *action = "capture".into();
                        }
                        e
                    })?;
                    Ok((*f, c))
                })
                .collect::<Result<Vec<_>>>()?;
            let is_function = closed_over.is_empty();
            let fun = Value::Fun(params.clone(), expressions.clone(), closed_over);
            // TODO: Closure vs function
            TypedValue::new(
                fun,
                Ty::Function(
                    Box::new(Ty::from(parameter_ty)),
                    Box::new(Ty::from(return_ty)),
                    if is_function {
                        None
                    } else {
                        Some(interner.generate("fun")?)
                    }
                ),
                interner,
            )
        })?,
        Declaration {
            ref identifier,
            ref value,
            ref ty,
        } => {
            // TODO: Shadowing
            let place = TypedValue::new(Invalid, Ty::from(ty), interner)?;
            let val = value
                .as_ref()
                .map(|v| execute(&*v, memory, interner))
                .unwrap_or_else(|| Ok(invalid()))?;
            memory.create(*identifier, place.assign(val, interner)?);
            terminal()
        }
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let val = {
                    let n = interner.resolve(*n).expect("No such literal");
                    str::parse(n).expect("Literal couldn't be parsed to integer")
                };
                int_typed(val, interner)?
            }
            Boolean(ref b) => bool_typed(*b, interner)?,
        },
        Identifier(ref i) => move_or_copy(i, memory, interner)?,
        Tuple { ref value } => tuple_typed(value
            .iter()
            .map(|e| execute(e, memory, interner))
            .collect::<Result<_>>()?),
        Union(ref u) => if let Some(ref u) = *u {
            let value = execute(&*u.value, memory, interner)?;
            union_typed(u.position, value, u.size)
        } else {
            Err(InitialConstruction)?
        },
        If(ref branch) => match *branch {
            Condition {
                ref condition,
                ref expressions,
                ref otherwise,
            } => execute_if(condition, expressions, otherwise, memory, interner)?,
            _ => panic!("else without if."),
        },
        Operation(ref o) => match *o {
            Assignment {
                ref identifier,
                ref value,
            } => {
                let value = execute(value, memory, interner)?;
                let n = NonExistentAssign(
                    interner.resolve(identifier.0)?.into(),
                    value.value().display(interner)?,
                    value.ty().display(interner)?,
                );
                memory
                    .replace(identifier, value, interner)
                    .unwrap_or_else(|| Err(n))?
            }
            Addition { ref parameters } => int_typed({
                    let parameters = parameters
                        .iter()
                        .map(|e| execute(e, memory, interner))
                        .collect::<Result<Vec<_>>>()?;
                    let (first, rest) = parameters.split_at(1);
                    if let Int(ref p) = *first[0].value() {
                        let mut result = *p;
                        for b in rest {
                            if let Int(b) = b.value().clone() {
                                result += b;
                            } else {
                                Err(AddingImpossible(b.value().display(interner)?, b.ty().display(interner)?, "I32".to_owned()))?
                            }
                        }
                        result
                    } else {
                        Err(unoperable(first[0].value().display(interner)?, first[0].ty().display(interner)?, "added"))?
                    }
                },
                interner,
            )?,
            Indexing {
                ref target,
                ref index,
            } => {
                let index = execute(index, memory, interner)?;
                let container = memory.get_mut(target).ok_or(unknown("variable", "index", interner.resolve(target.0)?))?;
                // TODO: This is ugly. Refactor pls.
                match container.split_mut_and_ref() {
                    (&mut Tup(ref mut value), &Ty::Tuple(ref ty)) => {
                        let index = match index.into_value() {
                            Int(n) => n,
                            _ => panic!("Cannot index with non-integer."),
                        };
                        let value = value.get_mut(index as usize).expect("Out of bounds access");
                        let ty = ty.get(index as usize).expect("Type should've been checked");
                        // TODO: move_or_copy
                        TypedValue::new(replace(value, Invalid), ty.clone(), interner)?
                    }
                    (v, t) => Err(unoperable(v.display(&interner)?, t.display(&interner)?, "indexed"))?,
                }
            }
            Calling {
                ref name,
                ref parameters,
            } => {
                let fun = move_or_copy(name, memory, interner).map_err(|mut e| {
                    if let Unknown { ref mut target, ref mut action, .. } = e {
                        *target = "function".into();
                        *action = "call".into();
                    }
                    e
                })?;
                match (fun.value(), fun.ty()) {
                    (&Fun(ref params, ref exprs, ref closed), &Ty::Function(ref param_ty, ref return_ty, _)) => {
                        let mut memory = params
                            .iter()
                            .cloned()
                            .zip(
                                parameters
                                    .iter()
                                    .map(|e| execute(e, memory, interner))
                                    .collect::<Result<Vec<_>>>()?
                                    .into_iter(),
                            )
                            .chain(closed.iter().cloned())
                            .collect();
                        interpret_scope(&exprs[..], &mut memory, interner)?
                    }
                    (v, t) => Err(unoperable(v.display(&interner)?, t.display(&interner)?, "called"))?,
                }
            }
        },
    })
}

fn execute_if(
    condition: &Expr<TypeKey>,
    expressions: &[Expr<TypeKey>],
    otherwise: &ast::If<TypeKey>,
    memory: &mut Memory<TypedValue>,
    interner: &mut Interner,
) -> Result<TypedValue> {
    let condition = execute(condition, memory, interner)?;
    let condition = match condition.into_value() {
        Bool(b) => b,
        _ => panic!("Invalid condition."),
    };
    if condition {
        interpret_scope(expressions, memory, interner)
    } else {
        match *otherwise {
            Condition {
                ref condition,
                ref expressions,
                ref otherwise,
            } => execute_if(condition, expressions, otherwise, memory, interner),
            Else(ref otherwise) => interpret_scope(otherwise, memory, interner),
        }
    }
}

fn move_or_copy(identifier: &ast::Identifier, memory: &mut Memory<TypedValue>, interner: &Interner) -> Result<TypedValue> {
    {
        let var = memory.get(identifier)
            .ok_or(unknown("variable", "access", interner.resolve(identifier.0)?))?;
        if match *var.ty() {
            Ty::Function(_, _, ref uniq) => uniq.is_none(),
            _ => false,
        } {
            return Ok(var.clone());
        }
    }
    memory.replace(identifier, invalid(), interner)
        .ok_or(unknown("variable", "access", interner.resolve(identifier.0)?))?
}

fn add_not_closed(
    free: &mut HashSet<ast::Identifier>,
    closed: &Memory<()>,
    identifier: &ast::Identifier,
) {
    if closed.get(identifier).is_none() {
        free.insert(*identifier);
    }
}

fn find_free_variables<'a, I>(
    free: &mut HashSet<ast::Identifier>,
    closed: &mut Memory<()>,
    exprs: I,
) where
    I: IntoIterator<Item = &'a Expr<TypeKey>>,
{
    for e in exprs {
        match e.expression {
            Identifier(ref i) => add_not_closed(free, &*closed, i),
            Operation(ref op) => match *op {
                Assignment {
                    ref identifier,
                    ref value,
                } => {
                    add_not_closed(free, &*closed, identifier);
                    find_free_variables(free, closed, once(&**value));
                }
                Addition { ref parameters } => {
                    find_free_variables(free, closed, &parameters[..]);
                }
                Indexing {
                    ref target,
                    ref index,
                } => {
                    add_not_closed(free, &*closed, target);
                    find_free_variables(free, closed, once(&**index));
                }
                Calling {
                    ref name,
                    ref parameters,
                } => {
                    add_not_closed(free, &*closed, name);
                    find_free_variables(free, closed, &parameters[..]);
                }
            },
            Declaration {
                ref identifier,
                ref value,
                ..
            } => {
                if let Some(ref e) = *value {
                    find_free_variables(free, closed, once(&**e));
                }
                closed.create(*identifier, ());
            }
            Function {
                ref params,
                ref expressions,
                ..
            } => {
                closed.scope(|closed| {
                    for p in params {
                        closed.create(*p, ());
                    }
                    find_free_variables(free, closed, expressions);
                });
            }
            If(ref branch) => match *branch {
                Condition {
                    ref condition,
                    ref expressions,
                    ref otherwise,
                } => find_free_variables_if(&**condition, expressions, otherwise, free, closed),
                _ => panic!("else without if."),
            },
            Tuple { ref value } => find_free_variables(free, closed, &value[..]),
            Union(Some(ref u)) => find_free_variables(free, closed, once(&*u.value)),
            Scope { ref expressions } => {
                closed.scope(|closed| find_free_variables(free, closed, expressions))
            }
            _ => {}
        }
    }
}

fn find_free_variables_if(
    condition: &Expr<TypeKey>,
    expressions: &[Expr<TypeKey>],
    otherwise: &ast::If<TypeKey>,
    free: &mut HashSet<ast::Identifier>,
    closed: &mut Memory<()>,
) {
    find_free_variables(free, closed, once(condition));
    closed.scope(|closed| find_free_variables(free, closed, &expressions[..]));
    match *otherwise {
        Condition {
            ref condition,
            ref expressions,
            ref otherwise,
        } => {
            find_free_variables_if(condition, expressions, otherwise, free, closed);
        }
        Else(ref otherwise) => {
            closed.scope(|closed| find_free_variables(free, closed, &otherwise[..]));
        }
    }
}
