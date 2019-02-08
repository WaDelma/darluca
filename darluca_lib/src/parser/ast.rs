#[derive(Debug)]
pub struct Ast {
    pub modules: Vec<Module>,
}

#[derive(Debug)]
pub enum Expr {
    Mod(Module),
    Fun(Function),
    App(Application),
    Ident(Ident),
    Comment(String),
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Function {
    pub signature: Option<Signature>,
    pub name: Ident,
    pub params: Vec<Ident>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Signature {
    pub publicity: Publicity,
    pub ty: FunTy,
}

#[derive(Debug)]
pub enum Publicity {
    Pub,
    Priv,
}

#[derive(Debug)]
pub struct FunTy {
    pub param: Type,
    pub ret: Type,
}

#[derive(Debug)]
pub struct Application {
    pub fun: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Debug)]
pub enum Type {
    Fun(Box<FunTy>),
    Prod(Vec<Type>),
    Sum(Vec<Type>),
    Placeholder,
}

#[derive(Debug)]
pub struct Ident(pub String);
