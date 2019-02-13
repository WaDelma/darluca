use std::fmt;

#[derive(PartialEq)]
pub struct Ast {
    pub modules: Vec<Module>,
}

#[derive(PartialEq)]
pub enum Expr {
    Mod(Module),
    Cond(Conditional),
    Lit(Literal),
    Assign(Assignment),
    Sign(Signature),
    Fun(Function),
    App(Application),
    Ident(Ident),
    Prod(Vec<Expr>),
    Sum(Box<Expr>),
    Sub(Box<Expr>),
    Comment(String),
}

pub struct Oper {
    lhs: Box<Expr>,
    op: String,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub value: Box<Expr>,
    pub condition: Condition,
    pub otherwise: Option<Vec<Expr>>,
}

#[derive(Debug, PartialEq)]
pub enum Condition {
    Multi(Vec<(Pattern, Vec<Expr>)>),
    Single(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct Pattern {}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(String),
    Str(String),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: Ident,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub lhs: Ident,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub params: Vec<Ident>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Signature {
    pub publicity: Publicity,
    pub mutability: Mutability,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, PartialEq)]
pub enum Publicity {
    Public,
    Private,
}

#[derive(Debug, PartialEq)]
pub struct FunTy {
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug, PartialEq)]
pub struct Application {
    pub fun: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Ty(Ident),
    Fun(Box<FunTy>),
    Prod(Vec<Type>),
    Sum(Vec<Type>),
    Paren(Box<Type>),
    Placeholder,
}

#[derive(PartialEq)]
pub struct Ident(pub String);

impl fmt::Debug for Ast {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if fmt.alternate() {
            write!(fmt, "Ast {:#?}", self.modules)
        } else {
            write!(fmt, "Ast {:?}", self.modules)
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        if fmt.alternate() {
            match self {
                Cond(c) => write!(fmt, "{:#?}", c),
                Assign(a) => write!(fmt, "{:#?}", a),
                Sum(s) => write!(fmt, "{:#?}", s),
                Prod(p) => write!(fmt, "{:#?}", p),
                Lit(l) => write!(fmt, "{:#?}", l),
                Sign(s) => write!(fmt, "{:#?}", s),
                Mod(m) => write!(fmt, "{:#?}", m),
                Sub(e) => write!(fmt, "({:#?})", e),
                Fun(f) => write!(fmt, "{:#?}", f),
                App(a) => write!(fmt, "{:#?}", a),
                Ident(i) => write!(fmt, "{:#?}", i),
                Comment(c) => write!(fmt, "Comment({:#?})", c),
            }
        } else {
            match self {
                Cond(c) => write!(fmt, "{:?}", c),
                Assign(a) => write!(fmt, "{:?}", a),
                Sum(s) => write!(fmt, "{:?}", s),
                Prod(p) => write!(fmt, "{:?}", p),
                Lit(l) => write!(fmt, "{:?}", l),
                Sign(s) => write!(fmt, "{:?}", s),
                Mod(m) => write!(fmt, "{:?}", m),
                Sub(e) => write!(fmt, "({:?})", e),
                Fun(f) => write!(fmt, "{:?}", f),
                App(a) => write!(fmt, "{:?}", a),
                Ident(i) => write!(fmt, "{:?}", i),
                Comment(c) => write!(fmt, "Comment({:?})", c),
            }
        }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "`{}`", self.0)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(f: &'a str) -> Self {
        Ident(f.into())
    }
}
