use interner::Symbol;
use typechecker::Type;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast {
    pub expressions: Vec<Expr<()>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Expr<T> {
    pub expression: Expression<T>,
    pub data: T,
}

impl<T> Expr<T> {
    pub fn new(expression: Expression<T>, data: T) -> Self {
        Expr {
            expression,
            data
        }
    }
}

impl From<Expression<()>> for Expr<()> {
    fn from(expression: Expression<()>) -> Self {
        Expr {
            expression,
            data: (),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression<T> {
    Literal(Literal),
    Identifier(Identifier),
    Operation(Operation<T>),
    If(If<T>),
    Declaration {
        identifier: Identifier,
        ty: Type,
        value: Option<Box<Expr<T>>>,
    },
    Function {
        params: Vec<Identifier>,
        expressions: Vec<Expr<T>>,
        parameter_ty: Type,
        return_ty: Type,
    },
    Tuple {
        value: Vec<Expr<T>>,
    },
    Union(Option<Union<T>>),
    Scope {
        expressions: Vec<Expr<T>>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum If<T> {
    Condition {
        condition: Box<Expr<T>>,
        expressions: Vec<Expr<T>>,
        otherwise: Box<If<T>>,
    },
    Else(Vec<Expr<T>>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Operation<T> {
    Assignment {
        identifier: Identifier,
        value: Box<Expr<T>>,
    },
    Addition {
        parameters: Vec<Expr<T>>,
    },
    Indexing {
        target: Identifier,
        index: Box<Expr<T>>,
    },
    Calling {
        name: Identifier,
        parameters: Vec<Expr<T>>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Union<T> {
    pub value: Box<Expr<T>>,
    pub position: usize,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Identifier(pub Symbol);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Literal {
    Integer(Symbol),
    Boolean(bool),
}
