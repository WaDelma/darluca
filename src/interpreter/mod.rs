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

#[derive(Debug, Fail, PartialEq, Clone)]
pub enum InterpreterError {
    #[fail(display = "Cannot assign non-existent variable {} with value {} of type {}", _0, _1, _2)]
    NonExistentAssign(String, String, String),
    #[fail(display = "Unknown variable: {}", _0)]
    UnknownVariable(String),
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
    
    fn new(value: Value, ty: Ty, interner: &mut Interner) -> Result<Self> {
        let val_ty = Ty::from_value(&value, interner)?;
        if val_ty.is_subtype_of(&ty) {
            Ok(TypedValue::unchecked(value, ty))
        } else {
            Err(ValueTypeMismatch(format!("{}", value), val_ty.display(interner)?, ty.display(interner)?))
        }
    }

    fn assign(&mut self, other: TypedValue, interner: &Interner) -> Result<TypedValue> {
        if self.ty.is_subtype_of(&other.ty) {
            Ok(replace(self, other))
        } else {
            Err(ValueTypeMismatch(format!("{}", other.value), other.ty.display(interner)?, self.ty.display(interner)?))
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
    Unknown,
    Named(Symbol),
    Tuple(Vec<Ty>),
    Union(Vec<Ty>),
    // TODO: Make closures type unique
    Function(Box<Ty>, Box<Ty>)
}

impl Ty {
    fn from_value(value: &Value, interner: &mut Interner) -> Result<Self> {
        use self::Ty::*;
        Ok(match *value {
            Invalid => Unknown,
            Tup(ref v) => Tuple(v.iter().map(|v| Ty::from_value(v, interner)).collect::<Result<Vec<_>>>()?),
            Uni(ref index, ref value, ref size) => Union(
                repeat(Ok(Unknown)).take(*index)
                    .chain(once(Ty::from_value(&**value, interner)))
                    .chain(repeat(Ok(Unknown)).take(size - index - 1))
                    .collect::<Result<Vec<_>>>()?
            ),
            Int(_) => Ty::Named(interner.intern("I32")?),
            Bool(_) => Ty::Named(interner.intern("Bool")?),
            Fun(..) => Unknown, // TOOD: Function type
        })
    }

    fn is_subtype_of(&self, other: &Ty) -> bool {
        use self::Ty::*;
        match (self, other) {
            (&Named(ref a), &Named(ref b)) => a == b,
            (&Tuple(ref a), &Tuple(ref b)) => a.iter().zip(b.iter()).all(|(a, b)| a.is_subtype_of(b)),
            // TODO: Initial is subtype of all?
            (&Union(ref a), &Union(ref b)) => a.iter().zip(b.iter()).all(|(a, b)| a.is_subtype_of(b)),
            _ => false,
        }
    }

    fn display(&self, interner: &Interner) -> Result<String> {
        use self::Ty::*;
        let mut f = String::new();
        match *self {
            Unknown => f.push_str("?"),
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
                f.push_str("|]");
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
            Type::Unknown => Ty::Unknown,
            Type::Named(ref s) => Ty::Named(s.clone()),
            Type::Tuple(ref t) => Ty::Tuple(t.iter().map(Ty::from).collect()),
            Type::Union(ref t) => Ty::Union(t.iter().map(Ty::from).collect()),
            Type::Function(ref p, ref r) => Ty::Function(Box::new(Ty::from(&**p)), Box::new(Ty::from(&**r))),
        }
    }
}

fn terminal() -> TypedValue {
    TypedValue {
        value: Tup(vec![]),
        ty: Ty::Tuple(vec![]),
    }
}

fn initial() -> TypedValue {
    TypedValue {
        value: Uni(0, Box::new(Invalid), 0),
        ty: Ty::Union(vec![]),
    }
}

fn invalid() -> TypedValue {
    TypedValue {
        value: Invalid,
        ty: Ty::Unknown,
    }
}

fn bool_typed(value: bool, interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Bool(value),
        ty: Ty::Named(interner.intern("Bool")?),
    })
}

fn int_typed(value: i32, interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Int(value),
        ty: Ty::Named(interner.intern("I32")?),
    })
}

fn union_typed(index: usize, value: TypedValue, size: usize) -> TypedValue {
    let tys = repeat(Ty::Unknown).take(index)
        .chain(once(value.ty))
        .chain(repeat(Ty::Unknown).take(size - index - 1))
        .collect();
    TypedValue {
        value: Uni(index, Box::new(value.value), size),
        ty: Ty::Union(tys),
    }
}

fn tuple_typed(values: Vec<TypedValue>) -> TypedValue {
    let (mut vals, mut tys) = (Vec::with_capacity(values.len()), Vec::with_capacity(values.len()));
    for tv in values {
        vals.push(tv.value);
        tys.push(tv.ty);
    }
    TypedValue {
        value: Tup(vals),
        ty: Ty::Tuple(tys),
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
                    let c = memory.update(f, invalid())
                        .expect(&format!("Failed to close over variable: {:?} => {:?}", f, interner.resolve(f.0)));
                    (*f, c)
                }).collect();
            let fun = Value::Fun(params.clone(), expressions.clone(), closed_over);
            // TODO: Closure vs function
            TypedValue::new(fun, Ty::Function(Box::new(Ty::from(parameter_ty)), Box::new(Ty::from(return_ty))), interner)
        })?,
        Declaration { ref identifier, ref value, ref ty } => {
            // TODO: Shadowing
            let v = if let Some(ref v) = *value {
                let mut v = execute(&*v, memory, interner)?;
                let ty = Ty::from(ty);
                if v.ty.is_subtype_of(&ty) {
                    v.ty = ty;
                } else {
                    return Err(ValueTypeMismatch(format!("{}", v.value), v.ty.display(interner)?, ty.display(interner)?))
                }
                v
            } else {
                let mut v = invalid();
                v.ty = Ty::from(ty);
                v
            };
            memory.create(*identifier, v);
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
            memory.update(i, invalid()).ok_or(UnknownVariable(interner.resolve(i.0)?.into()))?
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
            // TODO: Initial cannot be constructed!
            initial()
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
                let target = memory.get_mut(identifier)
                    .ok_or(NonExistentAssign(
                        interner.resolve(identifier.0)?.into(),
                        format!("{}", value.value),
                        value.ty.display(interner)?,
                    ))?;
                target.assign(value, interner)?
            },
            Addition { ref parameters } => {
                int_typed(parameters.iter()
                    .map(|e| execute(e, memory, interner))
                    .fold_results(0, |a, b|
                        if let Int(b) = b.value {
                            a + b
                        } else {
                            panic!("Cannot add: {:?} + {:?}", a, b);
                        }
                    )?, interner)?
            },
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
                        let ty = ty.get_mut(index as usize).expect("Type should've been checked");
                        let ty = replace(ty, Ty::Unknown);
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
        },
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