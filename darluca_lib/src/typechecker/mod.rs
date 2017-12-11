use ena::unify::{UnifyKey, UnifyValue, InPlaceUnificationTable};

use std::iter::{repeat, once};

use parser::ast::{self, Ast, Expr};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::{Symbol, Interner};

// TODO: Better type errors...
#[derive(Debug, Clone, PartialEq)]
pub struct TypecheckerError;

pub struct TypedAst {
    pub expressions: Vec<Expr<TypeKey>>,
    pub ctx: Typechecker,
}

pub fn typecheck(ast: Ast, interner: &mut Interner) -> Result<TypedAst, TypecheckerError> {
    let mut ctx = Typechecker::new();
    let expressions = ast.expressions
        .into_iter()
        .map(|e| typecheck_expr(e, &mut ctx, interner))
        .collect::<Result<_, _>>()?;
    Ok(TypedAst {
        expressions,
        ctx
    })
}

fn typecheck_expr(expr: Expr<()>, checker: &mut Typechecker, interner: &mut Interner) -> Result<Expr<TypeKey>, TypecheckerError> {
    Ok(match expr.expression {
        Literal(Integer(i)) => Expr {
            expression: Literal(Integer(i)),
            data: checker.new_type(Type::Named(interner.intern("I32").unwrap())),
        },
        Literal(Boolean(b)) => Expr {
            expression: Literal(Boolean(b)),
            data: checker.new_type(Type::Named(interner.intern("Bool").unwrap())),
        },
        Identifier(i) =>  Expr {
            expression: Identifier(i),
            // TODO: This is wrong. The type is constrained by it's previous mentions. Need to have Memory to track this.
            data: checker.new_unknown(),
        },
        Operation(Assignment { identifier, value}) => {
            // TODO: This is wrong. The type is constrained by it's previous mentions. Need to have Memory to track this.
            let ty = checker.new_unknown();
            let value = typecheck_expr(*value, checker, interner)?;
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
            let ty = checker.new_type(Type::Named(interner.intern("I32").unwrap()));
            let parameters = parameters.into_iter().map(|p| typecheck_expr(p, checker, interner)).collect::<Result<Vec<_>, _>>()?;
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
        // TODO: Need to keep variable to type mapping with Memory.
        // Function {
        //     params,
        //     expressions,
        //     parameter_ty,
        //     return_ty,
        // } => {
        //     let parameter_ty = checker.new_type(parameter_ty);
        //     let return_ty = checker.new_type(return_ty);
        //     let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner)).collect::<Result<Vec<_>, _>>()?;
        //     let last = expressions.last().map(|v| v.data);
        //     let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
        //     checker.unify(return_ty, last_ty)?;
        //     Expr {
        //         expression: Function {
        //             params,
        //             expressions,
        //             parameter_ty,
        //             return_ty,
        //         },
        //         data: ty,
        //     }
        // },
        Declaration { identifier, ty, value } => {
            let ty = checker.new_type(ty);
            let value  = if let Some(v) = value {
                let value = typecheck_expr(*v, checker, interner)?;
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
            let value = value.into_iter().map(|v| typecheck_expr(v, checker, interner)).collect::<Result<Vec<_>, _>>()?;
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
            let value = typecheck_expr(*value, checker, interner)?;
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
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner)).collect::<Result<Vec<_>, _>>()?;
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
            let branch = typecheck_if(branch, ty, checker, interner)?;
            Expr {
                expression: If(branch),
                data: ty,
            }
        },
        _ => unimplemented!(),
    })
}

fn typecheck_if(branch: ast::If<()>, ty: TypeKey, checker: &mut Typechecker, interner: &mut Interner) -> Result<ast::If<TypeKey>, TypecheckerError> {
    Ok(match branch {
        Condition {
            condition,
            expressions,
            otherwise,
        } => {
            let c_ty = checker.new_type(Type::Named(interner.intern("Bool").unwrap()));
            let condition = typecheck_expr(*condition, checker, interner)?;
            checker.unify(c_ty, condition.data)?;
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner)).collect::<Result<Vec<_>, _>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(ty, last_ty)?;
            let otherwise = typecheck_if(*otherwise, ty, checker, interner)?;
            Condition {
                condition: Box::new(condition),
                expressions,
                otherwise: Box::new(otherwise),
            }
        },
        Else(expressions) => {
            let expressions = expressions.into_iter().map(|e| typecheck_expr(e, checker, interner)).collect::<Result<Vec<_>, _>>()?;
            let last = expressions.last().map(|v| v.data);
            let last_ty = last.unwrap_or_else(|| checker.new_type(Type::Tuple(vec![])));
            checker.unify(ty, last_ty)?;
            Else(expressions)
        },
    })
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Unknown,
    Named(Symbol),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Function(Box<Type>, Box<Type>, Option<Symbol>),
}

impl UnifyValue for Type {
    type Error = TypecheckerError;
    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        use self::Type::*;
        Ok(match (v1, v2) {
            (&Named(ref a), &Named(ref b)) if a == b => Named(*a),
            (&Tuple(ref az), &Tuple(ref bz)) if az.len() == bz.len() => {
                Tuple(az.iter().zip(bz.iter())
                    .map(|(a, b)| Self::unify_values(a, b))
                    .collect::<Result<_, _>>()?)
            },
            (&Union(ref az), &Union(ref bz)) if az.len() == bz.len() => {
                Union(az.iter().zip(bz.iter())
                    .map(|(a, b)| Self::unify_values(a, b))
                    .collect::<Result<_, _>>()?)
            },
            (&Function(ref ap, ref ar, None), &Function(ref bp, ref br, None)) => {
                Function(Box::new(Self::unify_values(ap, bp)?), Box::new(Self::unify_values(ar, br)?), None)
            },
            (&Unknown, t) => t.clone(),
            (t, &Unknown) => t.clone(),
            _ => Err(TypecheckerError)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeKey(u32);

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

    pub fn unify(&mut self, a: TypeKey, b: TypeKey) -> Result<(), TypecheckerError> {
        self.checker.unify_var_var(a, b)
    }

    fn ty(&mut self, key: TypeKey) -> Type {
        self.checker.probe_value(key)
    }
}


#[test]
fn can_unify_same_integers() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32").unwrap();
    let intk1 = checker.new_type(Named(int));
    let intk2 = checker.new_type(Named(int));
    assert_eq!(Ok(()), checker.unify(intk1, intk2));
}

#[test]
fn can_unify_unknown_to_integer() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32").unwrap();
    let intk1 = checker.new_type(Named(int));
    let unknown1 = checker.new_unknown();
    assert_eq!(Ok(()), checker.unify(intk1, unknown1));
}

#[test]
fn can_unify_same_tuples() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32").unwrap();
    let boolean = interner.intern("Bool").unwrap();
    let tuple1 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    let tuple2 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    assert_eq!(Ok(()), checker.unify(tuple1, tuple2));
}

#[test]
fn cannot_unify_different_tuples() {
    use self::Type::*;
    let mut interner = Interner::new();
    let mut checker = Typechecker::new();
    let int = interner.intern("I32").unwrap();
    let boolean = interner.intern("Bool").unwrap();
    let tuple1 = checker.new_type(Tuple(vec![Named(int), Named(boolean)]));
    let tuple2 = checker.new_type(Tuple(vec![Named(boolean), Named(boolean)]));
    assert_eq!(Err(TypecheckerError), checker.unify(tuple1, tuple2));
}