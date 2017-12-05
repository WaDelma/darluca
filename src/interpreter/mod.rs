use symtern::prelude::*;

use itertools::Itertools;
use itertools::process_results;

use std::collections::HashSet;
use std::mem::replace;
use std::iter::{once, repeat};
use std::fmt;

use parser::ast::{self, Ast, Expression, Type};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::{Interner, Symbol};

use self::Value::*;
use self::InterpreterError::*;
use self::mem::Memory;

#[cfg(test)]
mod test;
mod mem;

type Result<T> = ::std::result::Result<T, InterpreterError>;

#[derive(Debug, Fail, PartialEq)]
pub enum InterpreterError {
    #[fail(display = "Cannot assign non-existent variable {} with value {} of type {}", _0, _1, _2)]
    NonExistentAssign(String, String, String),
    #[fail(display = "Unknown variable: {}", _0)]
    UnknownVariable(String),
    #[fail(display = "Wrong value {} for type {}", _0, _1)]
    TypeMismatch(String, String),
    #[fail(display = "ICE: Interning failed: {:?}", _0)]
    InternFailure(#[cause] ::symtern::Error)
}

impl From<::symtern::Error> for InterpreterError {
    fn from(e: ::symtern::Error) -> Self {
        InternFailure(e)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypedValue {
    value: Value,
    ty: Ty,
}

impl TypedValue {
    fn unchecked(value: Value, ty: Ty) -> Self {
        TypedValue {
            value,
            ty
        }
    }
    fn new(value: Value, ty: Ty, interner: &Interner) -> Result<Self> {
        if type_matches(&value, &ty, interner) {
            Ok(TypedValue::unchecked(value, ty))
        } else {
            Err(TypeMismatch(format!("{}", value), format!("{:?}", ty)))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    Invalid,
    Tup(Vec<Value>),
    Uni(usize, Box<Value>, usize),
    Int(i32),
    Bool(bool),
    Fun(Vec<ast::Identifier>, Vec<Expression>, Vec<(ast::Identifier, TypedValue)>),
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

// TODO: Make primitives variants?
// Doesn't really help getting interner rid though
// as with custom types it's needed again.
// Or should the we store the actual string here?
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Ty {
    Unkown,
    Named(Symbol),
    Tuple(Vec<Ty>),
    Union(Vec<Ty>),
    Function(Box<Ty>, Box<Ty>)
}

impl Ty {
    fn display(&self, interner: &Interner) -> Result<String> {
        use self::Ty::*;
        let mut f = String::new();
        match *self {
            Unkown => f.push_str("?"),
            Tuple(ref v) => {
                f.push_str("[");
                f.push_str(
                    &process_results(v.iter()
                        .map(|v| v.display(interner)), |mut v| v.join(", "))?
                );
                f.push_str(",]");
            },
            Union(ref v) => {
                f.push_str("[");
                f.push_str(
                    &process_results(v.iter()
                        .map(|v| v.display(interner)), |mut v| v.join("|"))?
                );
                f.push_str(",]");
            },
            Named(ref i) => f.push_str(&format!("{}", interner.resolve(*i)?)),
            Function(ref param, ref ret) => {
                f.push_str(&format!("{} -> {}", param.display(interner)?, ret.display(interner)?))
            },
        }
        Ok(f)
    }
}

impl<'a> From<&'a Type> for Ty {
    fn from(t: &'a Type) -> Self {
        match *t {
            Type::Named(ref s) => Ty::Named(s.clone()),
            Type::Tuple(ref t) => Ty::Tuple(t.iter().map(Ty::from).collect()),
            Type::Union(ref t) => Ty::Union(t.iter().map(Ty::from).collect()),
            Type::Function(ref p, ref r) => Ty::Function(Box::new(Ty::from(&**p)), Box::new(Ty::from(&**r))),
        }
    }
}

fn terminal(interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Tup(vec![]),
        ty: Ty::Named(interner.intern("[,]")?),
    })
}

fn initial(interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Uni(0, Box::new(Invalid), 0),
        ty: Ty::Named(interner.intern("[|]")?),
    })
}

fn invalid() -> TypedValue {
    TypedValue {
        value: Invalid,
        ty: Ty::Unkown,
    }
}

fn bool_typed(value: Value, interner: &mut Interner) -> Result<TypedValue> {
    if let Bool(_) = value {
        Ok(TypedValue {
            value,
            ty: Ty::Named(interner.intern("Bool")?),
        })
    } else {
        panic!("ICE: Non-bool Bool");
    }
}

fn int_typed(value: Value, interner: &mut Interner) -> Result<TypedValue> {
    if let Int(_) = value {
        Ok(TypedValue {
            value,
            ty: Ty::Named(interner.intern("I32")?),
        })
    } else {
        panic!("ICE: Non-int int");
    }
}

fn union_typed(index: usize, value: TypedValue, size: usize) -> Result<TypedValue> {
    if let Uni(..) = value.value {
        let tys = repeat(Ty::Unkown).take(index)
            .chain(once(value.ty))
            .chain(repeat(Ty::Unkown).take(size - index - 1))
            .collect();
        Ok(TypedValue {
            value: value.value,
            ty: Ty::Union(tys),
        })
    } else {
        panic!("ICE: Non-union union");
    }
}

pub fn interpret(ast: Ast, interner: &mut Interner) -> Result<TypedValue> {
    interpret_scope(&ast.expressions, &mut Memory::new(), interner)
}

fn interpret_scope(expressions: &[Expression], memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
    memory.scope(|memory| {
        let mut value = terminal(interner)?;
        for e in expressions {
            value = execute(e, memory, interner)?;
        }
        Ok(value)
    })
}

fn execute(e: &Expression, memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
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
                    let c = memory.update(f, invalid())
                        .expect(&format!("Failed to close over variable: {:?} => {:?}", f, interner.resolve(f.0)));
                    (*f, c)
                }).collect();
            // TODO: Type
            let fun = Value::Fun(params.clone(), expressions.clone(), closed_over);
            TypedValue::new(fun, Ty::Unkown, interner)
        })?,
        Declaration { ref identifier, ref value, ref ty } => {
            // TODO: Shadowing
            let value = value
                .as_ref()
                .map(|v| execute(&*v, memory, interner))
                .unwrap_or(Ok(invalid()))?;
            if let Some(ref ty) = *ty {
                if !type_matches(&value.value, &Ty::from(ty), interner) {
                    panic!("Trying to assign wrong type of value. {:?} is not of type {:?}", value, ty);
                }
            }
            memory.create(*identifier, value);
            terminal(interner)?
        },
        Literal(ref l) => match *l {
            Integer(ref n) => {
                let val = {
                    let n = interner.resolve(*n).expect("No such literal");
                    str::parse(n).expect("Literal couldn't be parsed to integer")
                };
                int_typed(Int(val), interner)?
            },
            Boolean(ref b) => {
                bool_typed(Bool(*b), interner)?
            }
        },
        Identifier(ref i) => {
            memory.update(i, invalid()).ok_or(UnknownVariable(interner.resolve(i.0)?.into()))?
        },
        Tuple { ref value } => unimplemented!()/*Tup(
            value.iter()
                .map(|e| execute(e, memory, interner))
                .collect::<Result<_>>()?
        )*/,
        Union(ref u) => if let Some(ref u) = *u {
            let value = execute(&*u.value, memory, interner)?;
            union_typed(u.position, value, u.size)?
        } else {
            initial(interner)?
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
                let target = memory.get_mut(identifier)
                    .ok_or(NonExistentAssign(
                        interner.resolve(identifier.0)?.into(),
                        format!("{}", value.value),
                        value.ty.display(interner)?,
                    ))?;
                replace(target, value)
            },
            Addition { ref parameters } => unimplemented!()/*Int(
                parameters.iter()
                    .map(|e| execute(e, memory, interner))
                    .fold_results(0, |a, b|
                        if let Int(b) = b {
                            a + b
                        } else {
                            panic!("Cannot add: {:?} + {:?}", a, b);
                        }
                    )?
            )*/,
            Indexing { ref target, ref index, } => {
                let index = execute(index, memory, interner)?;
                let container = memory.get_mut(target).expect("Indexable should exists");
                match (&mut container.value, &mut container.ty) {
                    (&mut Tup(ref mut value), &mut Ty::Tuple(ref mut ty)) => {
                        let index = match index.value {
                            Int(n) => n,
                            _ => panic!("Cannot index with non-integer."),
                        };
                        let value = value.get_mut(index as usize)
                            .expect("Out of bounds access");
                        let ty = ty.get_mut(index as usize).expect("Type should be checked");
                        let ty = replace(ty, Ty::Unkown);
                        TypedValue::new(replace(value, Invalid), ty, interner)?
                    }
                    _ => panic!("Cannot index"),
                }
            },
            Calling { ref name, ref parameters } => {
                let fun = memory.update(name, invalid()).expect("Function should exists");
                match fun.value {
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

fn execute_if(condition: &Expression, expressions: &[Expression], otherwise: &ast::If, memory: &mut Memory<TypedValue>, interner: &mut Interner) -> Result<TypedValue> {
    let condition = execute(condition, memory, interner)?;
    let condition = match condition.value {
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

fn type_matches(val: &Value, ty: &Ty, interner: &Interner) -> bool {
    use self::Value::*;
    match *val {
        Invalid => panic!("Invalid value"),
        Tup(ref v) => if let Ty::Tuple(ref t) = *ty {
            v.iter().zip(t.iter())
                .all(|(v, t)| type_matches(v, t, interner))
        } else {
            false
        },
        Uni(ref index, ref v, ref size) => if let Ty::Union(ref t) = *ty {
            if t.len() == *size {
                type_matches(&v, &t[*index], interner)
            } else {
                false
            }
        } else {
            false
        },
        Int(_) => if let Ty::Named(ref n) = *ty {
            "I32" == interner.resolve(*n).unwrap()
        } else {
            false
        },
        Bool(_) => if let Ty::Named(ref n) = *ty {
            "Bool" == interner.resolve(*n).unwrap()
        } else {
            false
        },
        // TODO: Typecheck functions
        Fun(_, _, _) => if let Ty::Function(_, _) = *ty {
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