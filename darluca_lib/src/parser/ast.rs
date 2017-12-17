use std::fmt;

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

    pub fn map_data<I, F>(self, f: &mut F) -> Expr<I>
        where F: FnMut(T) -> I
    {
        use self::Expression::*;
        let Expr { expression, data } = self;
        let expression = match expression {
            Operation(o) => Operation(o.map_data(f)),
            If(i) => If(i.map_data(f)),
            Declaration { identifier, ty, value, ..} => Declaration {
                identifier,
                ty,
                value: value.map(|v| v.map_data(f)).map(Box::new),
            },
            Function {
                params,
                expressions,
                parameter_ty,
                return_ty,
            } => Function {
                params,
                expressions: expressions.into_iter().map(|e| e.map_data(f)).collect(),
                parameter_ty,
                return_ty,
            },
            Tuple {
                value,
            } => Tuple {
                value: value.into_iter().map(|e| e.map_data(f)).collect(),
            },
            Union(Some(u)) => Union(Some(u.map_data(f))),
            Union(None) => Union(None),
            Scope {
                expressions,
            } => Scope {
                expressions: expressions.into_iter().map(|e| e.map_data(f)).collect(),
            },
            Literal(l) => Literal(l),
            Identifier(i) => Identifier(i),
        };
        Expr {
            expression: expression,
            data: f(data)
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
    // TODO: Ensure that invalid ifs are impossible
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

impl<T> If<T> {
    pub fn map_data<I, F>(self, f: &mut F) -> If<I>
        where F: FnMut(T) -> I
    {
        use self::If::*;
        match self {
            Condition {
                condition,
                expressions,
                otherwise,
            } => Condition {
                condition: Box::new(condition.map_data(f)),
                expressions: expressions.into_iter().map(|e| e.map_data(f)).collect(),
                otherwise: Box::new(otherwise.map_data(f)),
            },
            Else(expressions) => Else(expressions.into_iter().map(|e| e.map_data(f)).collect()),
        }
    }
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

impl<T> Operation<T> {
    pub fn map_data<I, F>(self, f: &mut F) -> Operation<I>
        where F: FnMut(T) -> I
    {
        use self::Operation::*;
        match self {
            Assignment {
                identifier,
                value,
            } => Assignment {
                identifier,
                value: Box::new(value.map_data(f)),
            },
            Addition {
                parameters,
            } => Addition {
                parameters: parameters.into_iter().map(|e| e.map_data(f)).collect(),
            },
            Indexing {
                target,
                index,
            } => Indexing {
                target,
                index: Box::new(index.map_data(f)),
            },
            Calling {
                name,
                parameters,
            } => Calling {
                name,
                parameters: parameters.into_iter().map(|e| e.map_data(f)).collect(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Union<T> {
    pub value: Box<Expr<T>>,
    pub position: usize,
    pub size: usize,
}

impl<T> Union<T> {
    fn map_data<I, F>(self, f: &mut F) -> Union<I>
        where F: FnMut(T) -> I
    {
        Union {
            value: Box::new(self.value.map_data(f)),
            position: self.position,
            size: self.position,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Identifier(pub Symbol);

impl fmt::Debug for Identifier {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("Ident(")?;
        (self.0).id().fmt(fmt)?;
        fmt.write_str(")")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Literal {
    Integer(Symbol),
    Boolean(bool),
}
