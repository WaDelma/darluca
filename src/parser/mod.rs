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

named!(operation(Tks) -> Expression,
    map!(
        alt_complete!(
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
            tag_token!(Parenthesis(Open)) >>
            expressions: separated_list!(
                tag_token!(Colon),
                expression
            ) >>
            opt!(tag_token!(Colon)) >>
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
            tag_token!(Parenthesis(Open)) >>
            tag_token!(Bar) >>
            tag_token!(Parenthesis(Close)) >>
            (Expression::Union(None))
        ) |
        map!(
            do_parse!(
                tag_token!(Parenthesis(Open)) >>
                before: opt!(
                    terminated!(
                        separated_list!(
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
                        separated_list!(
                            tag_token!(Bar),
                            tag_token!(Placeholder)
                        )
                    )
                ) >>
                opt!(tag_token!(Bar)) >>
                tag_token!(Parenthesis(Close)) >>
                (before, value, after)
            ),
            |(before, value, after)| {
                let before = before.map(|b| b.len()).unwrap_or(0);
                let after = after.map(|b| b.len()).unwrap_or(0);
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

named!(branch(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(If) >>
            condition: expression >>
            tag_token!(Curly(Open)) >>
            expressions: many0!(expression) >>
            tag_token!(Curly(Close)) >>
            elses: opt!(do_parse!(
                tag_token!(Else) >>
                tag_token!(Curly(Open)) >>
                elses: many0!(expression) >>
                tag_token!(Curly(Close)) >>
                (elses)
            )) >>
            (Box::new(condition), expressions, elses.unwrap_or(vec![]))
        ),
        |(condition, expressions, elses)| {
            Expression::If {
                condition,
                expressions,
                elses,
            }
        }
    )
);

named!(declaration(Tks) -> Expression,
    map!(
        do_parse!(
            tag_token!(Let) >>
            identifier: identifier!() >>
            value: opt!(complete!(do_parse!(
                tag_token!(Assignment) >>
                value: expression >>
                (value)
            ))) >>
            (identifier, value.map(Box::new))
        ),
        |(identifier, value)| {
            Expression::Declaration {
                identifier,
                value,
            }
        }
    )
);

named!(expression(Tks) -> Expression,
    alt_complete!(
        scope |
        branch |
        tuple |
        union |
        declaration |
        operation |
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
