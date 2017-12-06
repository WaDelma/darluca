use nom::{ErrorKind, IResult, Needed};

use lexer::tokens::{self, Tks, Token};
use lexer::tokens::Punctuation::*;
use lexer::tokens::Reserved::*;
use lexer::tokens::Balanced::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Direction::*;
use self::ast::*;

pub mod ast;
#[cfg(test)]
mod test;

macro_rules! tag_token (
  ($i: expr, $tag: expr) => (
    {
        (|| {
            let (i1, t1) = try_parse!($i, take!(1));
            if t1.tokens.is_empty() {
                IResult::Incomplete::<_, _, u32>(Needed::Size(1))
            } else {
                if t1.tokens[0] == Token::from($tag) {
                    IResult::Done(i1, t1)
                } else {
                    IResult::Error(error_position!(ErrorKind::Count, $i))
                }
            }
        })()
    }
  );
);

macro_rules! identifier (
  ($i: expr,) => (
    {
        (|| {
            let (i1, t1) = try_parse!($i, take!(1));
            if t1.tokens.is_empty() {
                IResult::Error(error_position!(ErrorKind::Tag, $i))
            } else {
                match t1.tokens[0] {
                    Token::Identifier(name) => IResult::Done(i1, Identifier(name)),
                    _ => IResult::Error(error_position!(ErrorKind::Tag, $i)),
                }
            }
        })()
    }
  );
);

macro_rules! ty_identifier (
  ($i: expr,) => (
    {
        (|| {
            let (i1, t1) = try_parse!($i, take!(1));
            if t1.tokens.is_empty() {
                IResult::Error(error_position!(ErrorKind::Tag, $i))
            } else {
                match t1.tokens[0] {
                    Token::Type(name) => IResult::Done(i1, Type::Named(name)),
                    _ => IResult::Error(error_position!(ErrorKind::Tag, $i)),
                }
            }
        })()
    }
  );
);

macro_rules! literal_integer (
  ($i: expr,) => (
    {
        (|| {
            let (i1, t1) = try_parse!($i, take!(1));
            if t1.tokens.is_empty() {
                IResult::Error(error_position!(ErrorKind::Tag, $i))
            } else {
                match t1.tokens[0] {
                    Token::Literal(tokens::Literal::Integer(data)) => IResult::Done(i1,
                        Literal::Integer(data)
                    ),
                    _ => IResult::Error(error_position!(ErrorKind::Tag, $i)),
                }
            }
        })()
    }
  );
);

named!(literal_boolean(Tks) -> Literal,
    alt_complete!(
        tag_token!(True) => {|_| Literal::Boolean(true)} |
        tag_token!(False) => {|_| Literal::Boolean(false)}
    )
);

named!(literal(Tks) -> Expression,
    map!(
        alt_complete!(
                literal_integer!() |
                literal_boolean
        ),
        |literal| {
            Expression::Literal(literal)
        }
    )
);

named!(ty_union(Tks) -> Type,
    do_parse!(
        tag_token!(Square(Open)) >>
        tys: separated_list!(
            tag_token!(Bar),
            ty
        ) >>
        tag_token!(Bar) >>
        tag_token!(Square(Close)) >>
        (Type::Union(tys))
    )
);

named!(ty_tuple(Tks) -> Type,
    do_parse!(
        tag_token!(Square(Open)) >>
        tys: separated_list!(
            tag_token!(Comma),
            ty
        ) >>
        tag_token!(Comma) >>
        tag_token!(Square(Close)) >>
        (Type::Tuple(tys))
    )
);

named!(ty_function(Tks) -> Type,
    do_parse!(
        tag_token!(Parenthesis(Open)) >>
        param: ty >>
        tag_token!(Arrow(Right)) >>
        result: ty >>
        tag_token!(Parenthesis(Close)) >>
        (Type::Function(Box::new(param), Box::new(result)))
    )
);

named!(ty(Tks) -> Type,
    alt!(
        ty_function |
        ty_union |
        ty_tuple |
        ty_identifier!()
    )
);

named!(assignment(Tks) -> Operation,
    map!(
        do_parse!(
            identifier: identifier!() >>
            tag_token!(Assignment) >>
            value: expression >>
            (identifier, Box::new(value))
        ),
        |(identifier, value)| {
            Operation::Assignment {
                identifier,
                value,
            }
        }
    )
);

named!(addition(Tks) -> Operation,
    map!(
        do_parse!(
            tag_token!(Parenthesis(Open)) >>
            first: expression >>
            tag_token!(Addition) >>
            second: expression >>
            tag_token!(Parenthesis(Close)) >>
            (first, second)
        ),
        |(first, second)| {
            Operation::Addition {
                parameters: vec![first, second],
            }
        }
    )
);

// TODO: Remove indexing and make tuples functions?
named!(indexing(Tks) -> Operation,
    map!(
        do_parse!(
            target: identifier!() >>
            tag_token!(Square(Open)) >>
            index: expression >>
            tag_token!(Square(Close)) >>
            (target, Box::new(index))
        ),
        |(target, index)| {
            Operation::Indexing {
                target,
                index,
            }
        }
    )
);

named!(calling(Tks) -> Operation,
    map!(
        do_parse!(
            name: identifier!() >>
            params: tuple >>
            (name, params)
        ),
        |(name, params)| {
            if let Expression::Tuple { value: parameters } = params {
                Operation::Calling {
                    name,
                    parameters,
                }
            } else {
                // TODO: If enum variants will become types...
                unreachable!("Parsing tuple should yield tuple");
            }
        }
    )
);

named!(operation(Tks) -> Expression,
    map!(
        alt_complete!(
            calling |
            assignment |
            addition |
            indexing
        ),
        |operation| {
            Expression::Operation(operation)
        }
    )
);


named!(tuple(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Square(Open)) >>
            expressions: separated_list!(
                tag_token!(Comma),
                expression
            ) >>
            tag_token!(Comma) >>
            tag_token!(Square(Close)) >>
            (expressions)
        ),
        |value| {
            Expression::Tuple {
                value,
            }
        }
    )
);

named!(union(Tks) -> Expression,
    alt!(
        do_parse!(
            tag_token!(Square(Open)) >>
            tag_token!(Bar) >>
            tag_token!(Square(Close)) >>
            (Expression::Union(None))
        ) |
        map!(
            do_parse!(
                tag_token!(Square(Open)) >>
                before: opt!(
                    terminated!(
                        separated_nonempty_list!(
                            tag_token!(Bar),
                            tag_token!(Placeholder)
                        ),
                        tag_token!(Bar)
                    )
                ) >>
                value: expression >>
                after: opt!(
                    preceded!(
                        tag_token!(Bar),
                        separated_nonempty_list!(
                            tag_token!(Bar),
                            tag_token!(Placeholder)
                        )
                    )
                ) >>
                tag_token!(Bar) >>
                tag_token!(Square(Close)) >>
                (before, value, after)
            ),
            |(before, value, after)| {
                let before = before.map(|b| b.len()).unwrap_or_else(|| 0);
                let after = after.map(|b| b.len()).unwrap_or_else(|| 0);
                Expression::Union(Some(Union {
                    value: Box::new(value),
                    position: before,
                    size: before + after + 1
                }))
            }
        )
    )
);

named!(scope(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Curly(Open)) >>
            expressions: many0!(expression) >>
            tag_token!(Curly(Close)) >>
            (expressions)
        ),
        |expressions| {
            Expression::Scope {
                expressions
            }
        }
    )
);

named!(branch(Tks) -> ast::If,
    map!(
        do_parse!(
            tag_token!(If) >>
            condition: expression >>
            tag_token!(Curly(Open)) >>
            expressions: many0!(expression) >>
            tag_token!(Curly(Close)) >>
            otherwise: opt!(
                preceded!(
                    tag_token!(Else),
                    alt!(
                        branch |
                        do_parse!(
                            tag_token!(Curly(Open)) >>
                            elses: many0!(expression) >>
                            tag_token!(Curly(Close)) >>
                            (ast::If::Else(elses))
                        )
                    )
                )
            ) >>
            (Box::new(condition),expressions, Box::new(
                otherwise.unwrap_or_else(|| ast::If::Else(vec![]))
            ))
        ),
        |(condition, expressions, otherwise)| {
            ast::If::Condition {
                condition,
                expressions,
                otherwise,
            }
        }
    )
);

named!(declaration(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Let) >>
            identifier: identifier!() >>
            ty: opt!(complete!(do_parse!(
                tag_token!(Colon) >>
                ty: ty >>
                (ty)
            ))) >>
            value: opt!(complete!(do_parse!(
                tag_token!(Assignment) >>
                value: expression >>
                (value)
            ))) >>
            (identifier, ty.unwrap_or_else(|| Type::Unknown), value.map(Box::new))
        ),
        |(identifier, ty, value)| {
            Expression::Declaration {
                identifier,
                ty,
                value,
            }
        }
    )
);

named!(function(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Square(Open)) >>
            params: separated_list!(
                tag_token!(Comma),
                do_parse!(
                    ident: identifier!() >>
                    param_ty: opt!(preceded!(
                        tag_token!(Colon),
                        ty
                    )) >>
                    (ident, param_ty)
                )
            ) >>
            tag_token!(Comma) >>
            tag_token!(Square(Close)) >>
            tag_token!(Arrow(Right)) >>
            return_ty: opt!(ty) >>
            scope: scope >>
            (params, scope, return_ty.unwrap_or_else(|| Type::Unknown))
        ),
        |(parameters, scope, return_ty)| {
            let mut params = Vec::with_capacity(parameters.len());
            let mut parameter_ty = Vec::with_capacity(parameters.len());
            for (p, t) in parameters {
                params.push(p);
                parameter_ty.push(t.unwrap_or_else(|| Type::Unknown));
            }
            let parameter_ty = if parameter_ty.len() > 1 || parameter_ty.is_empty() {
                Type::Tuple(parameter_ty)
            } else {
                parameter_ty.remove(0)
            };
            if let Expression::Scope { expressions } = scope {
                Expression::Function {
                    params,
                    expressions,
                    parameter_ty,
                    return_ty,
                }
            } else {
                // TODO: If enum variants will become types...
                unreachable!("Parsing scope should yield scope");
            }
        }
    )
);

named!(expression(Tks) -> Expression,
    alt_complete!(
        operation |
        function |
        scope |
        branch => {Expression::If} |
        tuple |
        union |
        declaration |
        literal |
        identifier!() => {Expression::Identifier}
    )
);

named!(pub parse(Tks) -> Ast,
    map!(
        many0!(expression),
        |expressions| {
            Ast {
                expressions
            }
        }
    )
);
