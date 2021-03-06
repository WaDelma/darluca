use ena::unify::{UnifyKey, UnifyValue, InPlaceUnificationTable};

use itertools::{Itertools, process_results};

use std::iter::{repeat, once};
use std::fmt;

use parser::ast::{self, Ast, Expr};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::{Symbol, Interner};
use interpreter::Memory;

use self::TypecheckerError::*;

type Result<T> = ::std::result::Result<T, TypecheckerError>;

// TODO: Better type errors...
#[derive(Debug, Fail, PartialEq, Clone)]
pub enum TypecheckerError {
    #[fail(display = "Unknown error")]
    UnknownError,
    #[fail(display = "ICE: Resolving interned string with symbol `{}` failed", _0)]
    ResolvingSymbolFailed(String),
}

pub struct TypedAst {
    pub expressions: Vec<Expr<TypeKey>>,
    pub ctx: Typechecker,
}

pub fn typecheck(ast: Ast, interner: &mut Interner) -> Result<TypedAst> {
    let mut ctx = Typechecker::new();
    let mut mem = Memory::new();
    mem.scope(move |mut mem| {
        let expressions = ast.expressions
            .into_iter()
            .map(|e| typecheck_expr(e, &mut ctx, interner, &mut mem))
            .collect::<Result<_>>()?;
        Ok(TypedAst {
            expressions,
            ctx
        })
    })
}

fn typecheck_expr(expr: Expr<()>, checker: &mut Typechecker, interner: &mut Interner, memory: &mut Memory<TypeKey>) -> Result<Expr<TypeKey>> {
    Ok(match expr.expression {
        Literal(Integer(i)) => Expr {
            expression: Literal(Integer(i)),
            data: checker.new_type(Type::Named(interner.intern("I32"))),
        },
        Literal(Boolean(b)) => Expr {
            expression: Literal(Boolean(b)),
            data: checker.new_type(Type::Named(interner.intern("Bool"))),
        },
        Identifier(i) =>  Expr {
            expression: Identifier(i),
            data: memory.get(&i).cloned().ok_or(UnknownError)?,
        },
        Operation(Assignment { identifier, value}) => {
            let ty = memory.get(&identifier).cloned().ok_or(UnknownError)?;
            let value = typecheck_expr(*value, checker, interner, memory)?;
            checker.unify(ty, value.data)?;
            Expr {
                expression: Operation(Assignment {
                    identifier,
                    value: Box::new(value),
                }),
                data: ty,
            }
        },
        Operation(Addition { parameters }) => {
            // TODO: More generic addition
            let ty = checker.new_type(Type::Named(interner.intern("I32")));
            let parameters = parameters.into_iter().map(|p| typecheck_expr(p, checker, interner, memory)).collect::<Result<Vec<_>>>()?;
            for p in &parameters {
                checker.unify(ty, p.data)?;
            }
            Expr {
                expression: Operation(Addition {
                    parameters
                }),
                data: ty,
            }
        },
        Operation(Calling {
            name,
            parameters
        }) => {
            let fun_ty = memory.get(&name).cloned().ok_or(UnknownError)?;
            match checker.ty(fun_ty) {
                Type::Function(ref parameter_ty, ref return_ty, ref closed) => {
                    let (parameters, parameter_ty) = match &**parameter_ty {
                        &Type::Tuple(ref ps) => {
                            // TODO: This doesn't work as tuple type created and it's contents are not associated with each other.
                            let parameters = parameters.iter().zip(ps.iter())
                                .map(|(p, t)| {
                                    let p = typecheck_expr(p.clone(), checker, interner, memory)?;
                                    let t = checker.new_type(t.clone());
                                    checker.unify(t, p.data)?;
                                    Ok(p)
                                }).collect::<Result<Vec<_>>>()?;
                            (parameters, checker.new_type(Type::Tuple(ps.clone())))
                        }
                        ty @ &Type::Named(_) => {
                            if parameters.len() != 1 {
                                Err(UnknownError)?
                            }
                            let ty = checker.new_type(ty.clone());
                            let param = typecheck_expr(parameters[0].clone(), checker, interner, memory)?;
                            checker.unify(ty, param.data)?;
                            (vec![param], ty)
                        },
                        &Type::Unknown => {
                            if parameters.len() != 1 {
                                Err(UnknownError)?
                            }
                            // TODO: Unknown parameter can be tuple
                            let ty = checker.new_unknown();
                            let param = typecheck_expr(parameters[0].clone(), checker, interner, memory)?;
                            checker.unify(ty, param.data)?;
                            (vec![param], ty)
                        },
                        _ => Err(UnknownError)?,
                    };
                    Expr {
                        expression: Operation(Calling {
                            name,
                            parameters,
                        }),
                        data: checker.new_type((&**return_ty).clone()),
                    } 
                },
                _ => Err(UnknownError)?,
            }
        },
        Function {
            params,
            expressions,
            parameter_ty,
            return_ty,
        } => memory.scope(|memory| {
            let parameter_ty = match parameter_ty {
                Type::Tuple(ps) => {
                    // TODO: This doesn't work as tuple type created and it's contents are not associated with each other.
                    for (p, t) in params.iter().zip(ps.iter()) {
                        let t = checker.new_type(t.clone());
                        memory.create(*p, t);
                    }
                    checker.new_type(Type::Tuple(ps))
                }
                ty @ Type::Named(_) => {
                    let ty = checker.new_type(ty);
                    if params.len() != 1 {
                        Err(UnknownError)?
                    }
                    memory.create(params[0], ty);
                    ty
                },
                Type::Unknown => {
                    // TODO: Unkown parameter can be tuple
                    let ty = checker.new_unknown();
                    if params.len() != 1 {
                        Err(UnknownError)?
                    }
                    memory.create(params[0], ty);
                    ty   
                },
                _ => Err(UnknownError)?,
            };

            let return_ty = checker.new_type(return_ty);
            let expressions = expressions.into_iter()
                .map(|e| typecheck_expr(e, checker, interner, memory))
                .collect::<Result<Vec<_>>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(return_ty, last_ty)?;
            let parameter_ty = checker.ty(parameter_ty);
            let return_ty = checker.ty(return_ty);
            // TODO: Find if there is any free variables to see the type is closure.
            let ty = checker.new_type(Type::Function(Box::new(parameter_ty.clone()), Box::new(return_ty.clone()), None));
            Ok(Expr {
                expression: Function {
                    params,
                    expressions,
                    parameter_ty: parameter_ty.clone(),
                    return_ty: return_ty.clone(),
                },
                data: ty,
            })
        })?,
        Declaration { identifier, ty, value } => {
            let ty = checker.new_type(ty);
            memory.create(identifier, ty);
            let value  = if let Some(v) = value {
                let value = typecheck_expr(*v, checker, interner, memory)?;
                checker.unify(ty, value.data)?;
                Some(Box::new(value))
            } else {
                None
            };
            Expr {
                expression: Declaration {
                    identifier,
                    ty: checker.ty(ty),
                    value: value,
                },
                data: checker.new_type(Type::Tuple(vec![])),
            }
        },
        Tuple { value } => {
            let value = value.into_iter().map(|v| typecheck_expr(v, checker, interner, memory)).collect::<Result<Vec<_>>>()?;
            let v = value.iter()
                .map(|v| checker.ty(v.data))
                .collect();
            let ty = checker.new_type(Type::Tuple(v));
            Expr {
                expression: Tuple {
                    value
                },
                data: ty,
            }
        },
        Union(Some(ast::Union { value, position, size } )) => {
            let value = typecheck_expr(*value, checker, interner, memory)?;
            let v = repeat(Type::Unknown).take(position)
                    .chain(once(checker.ty(value.data)))
                    .chain(repeat(Type::Unknown).take(size - position - 1))
                    .collect();
            let ty = checker.new_type(Type::Union(v));
            Expr {
                expression: Union(Some(ast::Union {
                    value: Box::new(value),
                    position,
                    size   
                })),
                data: ty,
            }
        },
        Union(None) => {
            Expr {
                expression: Union(None),
                // TODO: Does this give initial the right semantics?
                data: checker.new_unknown(),
            }
        },
        Scope { expressions } => {
            let ty = checker.new_unknown();
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner, memory)).collect::<Result<Vec<_>>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(ty, last_ty)?;
            Expr {
                expression: Scope {
                    expressions
                },
                data: ty,
            }
        },
        If(branch) => {
            let ty = checker.new_unknown();
            let branch = typecheck_if(branch, ty, checker, interner, memory)?;
            Expr {
                expression: If(branch),
                data: ty,
            }
        },
        _ => unimplemented!(),
    })
}

fn typecheck_if(branch: ast::If<()>, ty: TypeKey, checker: &mut Typechecker, interner: &mut Interner, memory: &mut Memory<TypeKey>) -> Result<ast::If<TypeKey>> {
    Ok(match branch {
        Condition {
            condition,
            expressions,
            otherwise,
        } => {
            let c_ty = checker.new_type(Type::Named(interner.intern("Bool")));
            let condition = typecheck_expr(*condition, checker, interner, memory)?;
            checker.unify(c_ty, condition.data)?;
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner, memory)).collect::<Result<Vec<_>>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(ty, last_ty)?;
            let otherwise = typecheck_if(*otherwise, ty, checker, interner, memory)?;
            Condition {
                condition: Box::new(condition),
                expressions,
                otherwise: Box::new(otherwise),
            }
        },
        Else(expressions) => {
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner, memory)).collect::<Result<Vec<_>>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(ty, last_ty)?;
            Else(expressions)
        },
    })
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Unknown,
    Named(Symbol),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Function(Box<Type>, Box<Type>, Option<Symbol>),
}

impl Type {
    pub fn display(&self, interner: &Interner) -> Result<String> {
        use self::Type::*;
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

impl fmt::Debug for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;
        match *self {
            Unknown => fmt.write_str("?")?,
            Named(ref s) => s.id().fmt(fmt)?,
            Union(ref c) | Tuple(ref c) => c.fmt(fmt)?,
            Function(ref p, ref r, ref c) => {
                fmt.write_str("[")?;
                p.fmt(fmt)?;
                fmt.write_str(",]")?;
                fmt.write_str(" -> ")?;
                r.fmt(fmt)?;
                if let Some(ref c) = *c {
                    fmt.write_str("/")?;
                    c.fmt(fmt)?;
                }
            },
        }
        Ok(())
    }
}

impl UnifyValue for Type {
    type Error = TypecheckerError;
    fn unify_values(v1: &Self, v2: &Self) -> Result<Self> {
        use self::Type::*;
        Ok(match (v1, v2) {
            (&Named(ref a), &Named(ref b)) if a == b => Named(*a),
            (&Tuple(ref az), &Tuple(ref bz)) if az.len() == bz.len() => {
                Tuple(az.iter().zip(bz.iter())
                    .map(|(a, b)| Self::unify_values(a, b))
                    .collect::<Result<_>>()?)
            },
            (&Union(ref az), &Union(ref bz)) if az.len() == bz.len() => {
                Union(az.iter().zip(bz.iter())
                    .map(|(a, b)| Self::unify_values(a, b))
                    .collect::<Result<_>>()?)
            },
            (&Function(ref ap, ref ar, None), &Function(ref bp, ref br, None)) => {
                Function(Box::new(Self::unify_values(ap, bp)?), Box::new(Self::unify_values(ar, br)?), None)
            },
            (&Unknown, t) => t.clone(),
            (t, &Unknown) => t.clone(),
            _ => Err(UnknownError)?,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeKey(u32);

impl fmt::Debug for TypeKey {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("TyKey(")?;
        self.0.fmt(fmt)?;
        fmt.write_str(")")
    }
}

impl UnifyKey for TypeKey {
    type Value = Type;
    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> Self {
        TypeKey(u)
    }
    fn tag() -> &'static str {
        "TypeKey"
    }
}

pub struct Typechecker {
    checker: InPlaceUnificationTable<TypeKey>,
}

impl Typechecker {
    pub fn new() -> Typechecker {
        Typechecker {
            checker: InPlaceUnificationTable::new(),
        }
    }

    pub fn new_type(&mut self, t: Type) -> TypeKey {
        self.checker.new_key(t)
    }

    pub fn new_unknown(&mut self) -> TypeKey {
        self.checker.new_key(Type::Unknown)
    }

    pub fn unify(&mut self, a: TypeKey, b: TypeKey) -> Result<()> {
        self.checker.unify_var_var(a, b)
    }

    pub fn ty(&mut self, key: TypeKey) -> Type {
        self.checker.probe_value(key)
    }
}


#[test]
fn can_unify_same_integers() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32");
    let intk1 = checker.new_type(Named(int));
    let intk2 = checker.new_type(Named(int));
    assert_eq!(Ok(()), checker.unify(intk1, intk2));
}

#[test]
fn can_unify_unknown_to_integer() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32");
    let intk1 = checker.new_type(Named(int));
    let unknown1 = checker.new_unknown();
    assert_eq!(Ok(()), checker.unify(intk1, unknown1));
}

#[test]
fn can_unify_same_tuples() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32");
    let boolean = interner.intern("Bool");
    let tuple1 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    let tuple2 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    assert_eq!(Ok(()), checker.unify(tuple1, tuple2));
}

#[test]
fn cannot_unify_different_tuples() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32");
    let boolean = interner.intern("Bool");
    let tuple1 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    let tuple2 = checker.new_type(Tuple(vec![Named(boolean), Named(boolean)]));
    assert_eq!(Err(UnknownError), checker.unify(tuple1, tuple2));
}