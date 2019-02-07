pub struct Ast {
    modules: Vec<Module>,
}

pub struct Module {
    name: Option<Ident>,
    exprs: Vec<Expr>,
}

pub enum Expr {
    Fun(Fun),
    App(Expr, Vec<Expr>),
    Comment(String),
}

pub struct Fun {
    signature: Option<Signature>,
    name: Ident,
    body: Vec<Expr>,
}

pub struct Signature {
    publicity: Publicity,
    ty: FunTy,
}

pub enum Publicity {
    Pub,
    Priv,
}

pub struct FunTy {
    param: Type,
    ret: Type,
}

pub enum Type {
    Fun(FunTy),
    Prod(Vec<Type>),
    Sum(Vec<Type>),
    Placeholder,
}

pub struct Ident(String);
