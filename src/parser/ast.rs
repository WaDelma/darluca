use interner::{Interner, Symbol};

#[derive(Debug, PartialEq, Eq)]
pub struct Ast {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Operation(Operation),
    Declaration {
        identifier: Identifier,
        ty: Option<Type>,
        value: Option<Box<Expression>>,
    },
    FunctionCall {
        name: Identifier,
        parameters: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        expressions: Vec<Expression>,
        elses: Vec<Expression>,
    },
    Tuple {
        value: Vec<Expression>,
    },
    Union(Option<Union>),
    Scope {
        expressions: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Assignment {
        identifier: Identifier,
        value: Box<Expression>,
    },
    Addition {
        parameters: Vec<Expression>,
    },
    Indexing {
        target: Identifier,
        index: Box<Expression>,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Union {
    pub value: Box<Expression>,
    pub position: usize,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Identifier(pub Symbol);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Type(pub Symbol);

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(Symbol),
    Boolean(bool)
}
