use ena::unify::{UnifyKey, UnifyValue, InPlaceUnificationTable};

use parser::ast::{self, Ast, Expr, Expression};
use parser::ast::Expression::*;
use parser::ast::Literal::*;
use parser::ast::Operation::*;
use parser::ast::If::*;
use interner::{Symbol, Interner};

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
        _ => unimplemented!(),
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
                Tuple(az.iter().zip(bz.iter())
                    .map(|(a, b)| Self::unify_values(a, b))
                    .collect::<Result<_, _>>()?)
            },
            (&Function(ref ap, ref ar, None), &Function(ref bp, ref br, None)) => {
                Function(Box::new(Self::unify_values(ap, bp)?), Box::new(Self::unify_values(ar, br)?), None)
            },
            _ => Err(TypecheckerError)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeKey(u32);

impl UnifyKey for TypeKey {
    type Value = Option<Type>;
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
        self.checker.new_key(Some(t))
    }

    pub fn new_unknown(&mut self) -> TypeKey {
        self.checker.new_key(None)
    }

    pub fn unify(&mut self, a: TypeKey, b: TypeKey) -> Result<(), TypecheckerError> {
        self.checker.unify_var_var(a, b)
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