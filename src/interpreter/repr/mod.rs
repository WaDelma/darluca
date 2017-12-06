use symtern::prelude::*;

use itertools::Itertools;
use itertools::process_results;

use std::iter::{once, repeat};
use std::mem::replace;

use parser::ast::{self, Expression, Type};
use interner::{Interner, Symbol};

use super::InterpreterError::*;
use super::Result;
use self::mem::Memory;
use self::Value::*;

pub mod mem;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypedValue {
    value: Value,
    ty: Ty,
}

impl TypedValue {
    pub fn unchecked(value: Value, ty: Ty) -> Self {
        TypedValue { value, ty }
    }

    pub fn new(value: Value, ty: Ty, interner: &mut Interner) -> Result<Self> {
        let val_ty = Ty::from_value(&value, interner)?;
        if val_ty.is_subtype_of(&ty) {
            Ok(TypedValue::unchecked(value, ty))
        } else {
            Err(ValueTypeMismatch(
                value.display(interner)?,
                val_ty.display(interner)?,
                ty.display(interner)?,
            ))
        }
    }

    pub fn assign(self, value: TypedValue, interner: &mut Interner) -> Result<Self> {
        if value.ty.is_subtype_of(&self.ty) {
            Ok(TypedValue::unchecked(value.value, self.ty))
        } else {
            Err(ValueTypeMismatch(
                value.value.display(interner)?,
                value.ty.display(interner)?,
                self.ty.display(interner)?,
            ))
        }
    }

    pub fn into_value(self) -> Value {
        self.value
    }

    pub fn split_mut_and_ref(&mut self) -> (&mut Value, &Ty) {
        (&mut self.value, &self.ty)
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn ty(&self) -> &Ty {
        &self.ty
    }
}

impl Memory<TypedValue> {
    pub fn replace(
        &mut self,
        i: &ast::Identifier,
        mut v: TypedValue,
        interner: &Interner,
    ) -> Option<Result<TypedValue>> {
        self.get_mut(i).map(|t| {
            if v.ty.is_subtype_of(&t.ty) {
                v.ty = t.ty.clone();
                Ok(replace(t, v))
            } else {
                Err(ValueTypeMismatch(
                    v.value.display(interner)?,
                    v.ty.display(interner)?,
                    t.ty.display(interner)?,
                ))
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Value {
    // TODO: Is there difference between invalid value and initial object?
    Invalid,
    Tup(Vec<Value>),
    Uni(usize, Box<Value>, usize),
    Int(i32),
    Bool(bool),
    // TODO: Should the function value carry it's type? The closed values already do...
    Fun(
        Vec<ast::Identifier>,
        Vec<Expression>,
        Vec<(ast::Identifier, TypedValue)>,
    ),
}

impl Value {
    pub fn display(&self, interner: &Interner) -> Result<String> {
        let mut f = String::new();
        match *self {
            Invalid => f.push_str("Invalid"),
            Tup(ref v) => {
                f.push_str("[");
                f.push_str(&process_results(
                    v.iter().map(|v| v.display(interner)),
                    |mut v| v.join(", "),
                )?);
                f.push_str(",]")
            }
            Uni(ref index, ref val, ref size) => {
                f.push_str("[");
                for _ in 0..*index {
                    f.push_str("_|");
                }
                f.push_str(&format!("{}|", val.display(interner)?));
                for _ in (index + 1)..*size {
                    f.push_str("_|");
                }
                f.push_str("]")
            }
            Int(ref i) => f.push_str(&format!("{}", i)),
            Bool(ref b) => f.push_str(&format!("{}", b)),
            Fun(ref params, ..) => {
                f.push_str("[");
                f.push_str(&process_results(
                    params.iter().map(|v| interner.resolve(v.0)),
                    |mut v| v.join(", "),
                )?);
                f.push_str(",] -> _");
            }
        }
        Ok(f)
    }
}

// TODO: Make primitives variants?
// Doesn't really help getting rid of interner though
// as with custom types it's needed again.
// Or should the we store the actual string here?
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Ty {
    Unknown,
    Named(Symbol),
    Tuple(Vec<Ty>),
    Union(Vec<Ty>),
    // TODO: Make closures type unique
    Function(Box<Ty>, Box<Ty>),
}

impl Ty {
    pub fn from_value(value: &Value, interner: &mut Interner) -> Result<Self> {
        use self::Ty::*;
        Ok(match *value {
            Invalid => Unknown,
            Tup(ref v) => Tuple(v.iter()
                .map(|v| Ty::from_value(v, interner))
                .collect::<Result<Vec<_>>>()?),
            Uni(ref index, ref value, ref size) => Union(repeat(Ok(Unknown))
                .take(*index)
                .chain(once(Ty::from_value(&**value, interner)))
                .chain(repeat(Ok(Unknown)).take(size - index - 1))
                .collect::<Result<Vec<_>>>()?),
            Int(_) => Ty::Named(interner.intern("I32")?),
            Bool(_) => Ty::Named(interner.intern("Bool")?),
            // TODO: Closures vs functions.
            Fun(..) => Ty::Function(Box::new(Unknown), Box::new(Unknown)),
        })
    }

    pub fn is_subtype_of(&self, other: &Ty) -> bool {
        use self::Ty::*;
        match (self, other) {
            (&Named(ref a), &Named(ref b)) => a == b,
            (&Tuple(ref a), &Tuple(ref b)) => {
                a.iter().zip(b.iter()).all(|(a, b)| a.is_subtype_of(b))
            }
            // TODO: Initial is subtype of all?
            (&Union(ref a), &Union(ref b)) => {
                a.iter().zip(b.iter()).all(|(a, b)| a.is_subtype_of(b))
            }
            // Contravariant on parameter and covariant on return type
            (&Function(ref ai, ref ar), &Function(ref bi, ref br)) => {
                bi.is_subtype_of(ai) && ar.is_subtype_of(br)
            }
            // TODO: These should impose subtype requirements.
            (&Unknown, _) | (_, &Unknown) => true,
            _ => false,
        }
    }

    pub fn display(&self, interner: &Interner) -> Result<String> {
        use self::Ty::*;
        let mut f = String::new();
        match *self {
            Unknown => f.push_str("?"),
            Tuple(ref v) => {
                f.push_str("[");
                f.push_str(&process_results(
                    v.iter().map(|v| v.display(interner)),
                    |mut v| v.join(", "),
                )?);
                f.push_str(",]");
            }
            Union(ref v) => {
                f.push_str("[");
                f.push_str(&process_results(
                    v.iter().map(|v| v.display(interner)),
                    |mut v| v.join("|"),
                )?);
                f.push_str("|]");
            }
            Named(ref i) => f.push_str(&format!("{}", interner.resolve(*i)?)),
            Function(ref param, ref ret) => f.push_str(&format!(
                "{} -> {}",
                param.display(interner)?,
                ret.display(interner)?
            )),
        }
        Ok(f)
    }
}

impl<'a> From<&'a Type> for Ty {
    fn from(t: &'a Type) -> Self {
        match *t {
            Type::Unknown => Ty::Unknown,
            Type::Named(ref s) => Ty::Named(*s),
            Type::Tuple(ref t) => Ty::Tuple(t.iter().map(Ty::from).collect()),
            Type::Union(ref t) => Ty::Union(t.iter().map(Ty::from).collect()),
            Type::Function(ref p, ref r) => {
                Ty::Function(Box::new(Ty::from(&**p)), Box::new(Ty::from(&**r)))
            }
        }
    }
}

pub fn terminal() -> TypedValue {
    TypedValue {
        value: Tup(vec![]),
        ty: Ty::Tuple(vec![]),
    }
}

pub fn invalid() -> TypedValue {
    TypedValue {
        value: Invalid,
        ty: Ty::Unknown,
    }
}

pub fn bool_typed(value: bool, interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Bool(value),
        ty: Ty::Named(interner.intern("Bool")?),
    })
}

pub fn int_typed(value: i32, interner: &mut Interner) -> Result<TypedValue> {
    Ok(TypedValue {
        value: Int(value),
        ty: Ty::Named(interner.intern("I32")?),
    })
}

pub fn union_typed(index: usize, value: TypedValue, size: usize) -> TypedValue {
    let tys = repeat(Ty::Unknown)
        .take(index)
        .chain(once(value.ty))
        .chain(repeat(Ty::Unknown).take(size - index - 1))
        .collect();
    TypedValue {
        value: Uni(index, Box::new(value.value), size),
        ty: Ty::Union(tys),
    }
}

pub fn tuple_typed(values: Vec<TypedValue>) -> TypedValue {
    let (mut vals, mut tys) = (
        Vec::with_capacity(values.len()),
        Vec::with_capacity(values.len()),
    );
    for tv in values {
        vals.push(tv.value);
        tys.push(tv.ty);
    }
    TypedValue {
        value: Tup(vals),
        ty: Ty::Tuple(tys),
    }
}
