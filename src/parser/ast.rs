#[derive(Debug, PartialEq, Eq)]
pub struct Ast<'ctx> {
    pub expressions: Vec<Expression<'ctx>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression<'ctx> {
    Literal(Literal<'ctx>),
    Identifier(Identifier<'ctx>),
    Operation(Operation<'ctx>),
    FunctionCall {
        name: Identifier<'ctx>,
        parameters: Vec<Expression<'ctx>>,
    },
    Tuple {
        value: Vec<Expression<'ctx>>,
    },
    Union(Option<Union<'ctx>>),
    Scope {
        expressions: Vec<Expression<'ctx>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operation<'ctx> {
    Assignment {
        identifier: Identifier<'ctx>,
        value: Box<Expression<'ctx>>,
    },
    Addition {
        parameters: Vec<Expression<'ctx>>,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Union<'ctx> {
    pub value: Box<Expression<'ctx>>,
    pub position: usize,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier<'ctx>(pub &'ctx str);

impl<'ctx> From<&'ctx str> for Identifier<'ctx> {
    fn from(f: &'ctx str) -> Self {
        Identifier(f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal<'ctx> {
    Integer(&'ctx str)
}
