use interner::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Operation(Operation),
    If(If),
    Declaration {
        identifier: Identifier,
        ty: Option<Type>,
        value: Option<Box<Expression>>,
    },
    Function {
        params: Vec<Identifier>,
        expressions: Vec<Expression>,
    },
    Tuple {
        value: Vec<Expression>,
    },
    Union(Option<Union>),
    Scope {
        expressions: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum If {
    Condition {
        condition: Box<Expression>,
        expressions: Vec<Expression>,
        otherwise: Box<If>,
    },
    Else(Vec<Expression>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
    },
    Calling {
        name: Identifier,
        parameters: Vec<Expression>,
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Union {
    pub value: Box<Expression>,
    pub position: usize,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Identifier(pub Symbol);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Named(Symbol),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Function(Box<Type>, Box<Type>)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Literal {
    Integer(Symbol),
    Boolean(bool)
}
