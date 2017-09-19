use nom::{IResult, ErrorKind, Needed, Err};

use lexer::tokens::{self, Token, Tokens, Tks, Punctuation, Operator, Reserved};
use lexer::tokens::Punctuation::*;
use lexer::tokens::Reserved::*;
use lexer::tokens::Balanced::*;
use lexer::tokens::Operator::*;
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

macro_rules! literal (
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

named!(assignment(Tks) -> Operation,
    map!(
        do_parse!(
            identifier: identifier!() >>
            tag_token!(Assignment) >>
            expression: expression >>
            (identifier, expression)
        ),
        |(identifier, expression)| {
            Operation::Assignment {
                identifier,
                value: Box::new(expression),
            }
        }
    )
);

named!(addition(Tks) -> Operation,
    map!(
        do_parse!(
            tag_token!(Addition) >>
            parameters: tuple >>
            (parameters)
        ),
        |parameters| {
            if let Expression::Tuple { value } = parameters {
                Operation::Addition {
                    parameters: value,
                }
            } else {
                panic!("Parsed tuple wasn't tuple.");
            }
        }
    )
);

named!(operation(Tks) -> Expression,
    map!(
        alt_complete!(
            assignment |
            addition
        ),
        |operation| {
            Expression::Operation(operation)
        }
    )
);


named!(tuple(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Parenthesis(Open)) >>
            expressions: many0!(expression) >>
            tag_token!(Parenthesis(Close)) >>
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
            tag_token!(Bar) >>
            tag_token!(Bar) >>
            (Expression::Union(None))
        ) |
        map!(
            do_parse!(
                tag_token!(Bar) >>
                before: many0!(tag_token!(Placeholder)) >>
                value: expression >>
                after: many0!(tag_token!(Placeholder)) >>
                tag_token!(Bar) >>
                (before, value, after)
            ),
            |(before, value, after)| {
                Expression::Union(Some(Union {
                    value: Box::new(value),
                    position: before.len(),
                    size: before.len() + after.len() + 1
                }))
            }
        )
    )
);

named!(expression(Tks) -> Expression,
    alt_complete!(
        tuple |
        union |
        operation |
        literal!() => {Expression::Literal} |
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
