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
    FunctionCall {
        name: Identifier,
        parameters: Vec<Expression>,
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

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(Symbol)
}
