use itertools::Itertools;
use itertools::process_results;

use std::iter::{once, repeat};
use std::mem::replace;
use std::fmt;

use parser::ast::{self, Expr};
use interner::{Interner, Symbol};
use typechecker::{TypeKey, Type};

use super::InterpreterError::*;
use super::Result;
use self::mem::Memory;
use self::Value::*;

pub mod mem;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypedValue<V> {
    value: Value<V>,
    ty: Type,
}

impl<T> TypedValue<T> {
    pub fn unchecked(value: Value<T>, ty: Type) -> Self {
        TypedValue { value, ty }
    }

    pub fn new(value: Value<T>, ty: Type, interner: &mut Interner) -> Result<Self> {
        // let val_ty = Ty::from_value(&value, interner)?;
        // if val_ty.is_subtype_of(&ty) {
            Ok(TypedValue::unchecked(value, ty))
        // } else {
        //     Err(ValueTypeMismatch(
        //         value.display(interner)?,
        //         val_ty.display(interner)?,
        //         ty.display(interner)?,
        //     ))
        // }
    }

    pub fn assign(self, value: TypedValue<T>, interner: &mut Interner) -> Result<Self> {
        // if value.ty.is_subtype_of(&self.ty) {
            Ok(TypedValue::unchecked(value.value, self.ty))
        // } else {
        //     Err(ValueTypeMismatch(
        //         value.value.display(interner)?,
        //         value.ty.display(interner)?,
        //         self.ty.display(interner)?,
        //     ))
        // }
    }

    pub fn into_value(self) -> Value<T> {
        self.value
    }

    pub fn split_mut_and_ref(&mut self) -> (&mut Value<T>, &Type) {
        (&mut self.value, &self.ty)
    }

    pub fn value(&self) -> &Value<T> {
        &self.value
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn cleanse(self) -> TypedValue<()> {
        TypedValue {
            value: self.value.cleanse(),
            ty: self.ty,
        }
    }
}

impl<T> Memory<TypedValue<T>> {
    pub fn replace(
        &mut self,
        i: &ast::Identifier,
        mut v: TypedValue<T>,
        interner: &Interner,
    ) -> Option<Result<TypedValue<T>>> {
        self.get_mut(i).map(|t| {
            // if v.ty.is_subtype_of(&t.ty) {
                v.ty = t.ty.clone();
                Ok(replace(t, v))
            // } else {
            //     Err(ValueTypeMismatch(
            //         v.value.display(interner)?,
            //         v.ty.display(interner)?,
            //         t.ty.display(interner)?,
            //     ))
            // }
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Value<V> {
    // TODO: Is there difference between invalid value and initial object?
    Invalid,
    Tup(Vec<Value<V>>),
    Uni(usize, Box<Value<V>>, usize),
    Int(i32),
    Bool(bool),
    // TODO: Should the function value carry it's type? The closed values already do...
    Fun(
        Vec<ast::Identifier>,
        Vec<Expr<V>>,
        Vec<(ast::Identifier, TypedValue<V>)>,
    ),
}

impl<V: fmt::Debug> fmt::Debug for Value<V> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Value::*;
        match *self {
            Invalid => fmt.write_str("Invalid")?,
            Tup(ref c) => c.fmt(fmt)?,
            Uni(ref index, ref val, ref size) => {
                fmt.write_str("[")?;
                for _ in 0..*index {
                    fmt.write_str("_|")?;
                }
                val.fmt(fmt)?;
                fmt.write_str("|")?;
                for _ in (index + 1)..*size {
                    fmt.write_str("_|")?;
                }
                fmt.write_str("]")?;
            },
            Int(ref i) => {
                fmt.write_str("Int(")?;
                i.fmt(fmt)?;
                fmt.write_str(")")?;
            },
            Bool(ref b) => {
                fmt.write_str("Bool(")?;
                b.fmt(fmt)?;
                fmt.write_str(")")?;
            },
            Fun(ref params, ref exprs, ref closed) => {
                params.fmt(fmt)?;
                fmt.write_str(" -> ")?;
                exprs.fmt(fmt)?;
                fmt.write_str("/")?;
                closed.fmt(fmt)?;
            },
        }
        Ok(())
    }
}

impl<V> Value<V> {
    pub fn cleanse(self) -> Value<()> {
        match self {
            Invalid => Invalid,
            Tup(vs) => Tup(vs.into_iter().map(|v| v.cleanse()).collect()),
            Uni(index, val, size) => Uni(index, Box::new(val.cleanse()), size),
            Int(i) => Int(i),
            Bool(b) => Bool(b),
            Fun(
                ids,
                exprs,
                closed,
            ) => Fun(
                ids,
                exprs.into_iter().map(|e| e.map_data(&mut |_| ())).collect(),
                closed.into_iter().map(|(k, v)| (k, v.cleanse())).collect(),
            ),
        }
    }

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
                    params.iter().map(|v| interner.resolve(v.0).ok_or_else(|| ResolvingSymbolFailed(format!("{:?}", v)))),
                    |mut v| v.join(", "),
                )?);
                f.push_str(",] -> _");
            }
        }
        Ok(f)
    }
}

/*impl Ty {
    pub fn from_value<V>(value: &Value<V>, interner: &mut Interner) -> Result<Self> {
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
            Int(_) => Ty::Named(interner.intern("I32")),
            Bool(_) => Ty::Named(interner.intern("Bool")),
            // TODO: Closures vs functions.
            Fun(_, _, ref c) => if c.is_empty() {
                Ty::Function(Box::new(Unknown), Box::new(Unknown), None)
            } else {
                Ty::Function(Box::new(Unknown), Box::new(Unknown), Some(interner.generate("fun")))
            }
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
            (&Function(ref ai, ref ar, ref ac), &Function(ref bi, ref br, ref bc)) => {
                ac.is_none() && bc.is_none() && bi.is_subtype_of(ai) && ar.is_subtype_of(br)
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
            Named(ref i) => f.push_str(&format!("{}", interner.resolve(*i).ok_or_else(|| ResolvingSymbolFailed(format!("{:?}", i)))?)),
            Function(ref param, ref ret, ref uniq) => if let Some(ref uniq) = *uniq {
                f.push_str(&format!(
                    "{} -> {}/{}",
                    param.display(interner)?,
                    ret.display(interner)?,
                    interner.resolve(*uniq).ok_or_else(|| ResolvingSymbolFailed(format!("{:?}", uniq)))?
                ))
            } else {
                f.push_str(&format!(
                    "{} -> {}",
                    param.display(interner)?,
                    ret.display(interner)?
                ))
            },
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
            Type::Function(ref p, ref r, ref closed) => {
                Ty::Function(Box::new(Ty::from(&**p)), Box::new(Ty::from(&**r)), closed.clone())
            }
        }
    }
}
*/

pub fn terminal() -> TypedValue<TypeKey> {
    TypedValue {
        value: Tup(vec![]),
        ty: Type::Tuple(vec![]),
    }
}

pub fn invalid() -> TypedValue<TypeKey> {
    TypedValue {
        value: Invalid,
        ty: Type::Unknown,
    }
}

pub fn bool_typed(value: bool, interner: &mut Interner) -> TypedValue<TypeKey> {
    TypedValue {
        value: Bool(value),
        ty: Type::Named(interner.intern("Bool")),
    }
}

pub fn int_typed(value: i32, interner: &mut Interner) -> TypedValue<TypeKey> {
    TypedValue {
        value: Int(value),
        ty: Type::Named(interner.intern("I32")),
    }
}

pub fn union_typed(index: usize, value: TypedValue<TypeKey>, size: usize) -> TypedValue<TypeKey> {
    let tys = repeat(Type::Unknown)
        .take(index)
        .chain(once(value.ty))
        .chain(repeat(Type::Unknown).take(size - index - 1))
        .collect();
    TypedValue {
        value: Uni(index, Box::new(value.value), size),
        ty: Type::Union(tys),
    }
}

pub fn tuple_typed(values: Vec<TypedValue<TypeKey>>) -> TypedValue<TypeKey> {
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
        ty: Type::Tuple(tys),
    }
}
