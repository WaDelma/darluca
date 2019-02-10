use std::fmt;

#[derive(PartialEq)]
pub struct Ast {
    pub modules: Vec<Module>,
}

#[derive(PartialEq)]
pub enum Expr {
    Mod(Module),
    Assign(Assignment),
    Sign(Signature),
    Fun(Function),
    App(Application),
    Ident(Ident),
    Sub(Box<Expr>),
    Comment(String),
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
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub enum Publicity {
    Pub,
    Priv,
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
        }else {
            write!(fmt, "Ast {:?}", self.modules)
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        if fmt.alternate() {
            match self {
                Assign(a) => write!(fmt, "{:#?}", a),
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
                Assign(a) => write!(fmt, "{:?}", a),
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