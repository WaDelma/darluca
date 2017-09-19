use std::str;

use nom::{is_digit, is_alphabetic};

use self::tokens::{Token, Tokens};
use self::tokens::Token::*;
use self::tokens::Punctuation::*;
use self::tokens::Balanced::*;
use self::tokens::Operator::*;
use self::tokens::Reserved::*;
use self::tokens::Literal::*;

pub mod tokens;
#[cfg(test)]
pub mod test;

// TODO: Full UTF-8 support
// fn is_valid_for_ident(c: char) -> bool {
//     c.is_alphabetic() || c == '_'
// }

named!(identifier(&[u8]) -> Token,
    map!(
        take_while1!(is_alphabetic),
        |ident| {
            str::from_utf8(ident)
                .map(Identifier)
                .expect("is_alphabetic should ensure that ident is valid utf-8.")
        }
    )
);

named!(literal(&[u8]) -> tokens::Literal,
    alt!(
        map!(
            take_while1!(is_digit),
            |integer| {
                str::from_utf8(integer)
                    .map(Integer)
                    .expect("is_digit should ensure that integer is valid utf-8.")
            }
        )
    )
);

named!(reserved(&[u8]) -> tokens::Reserved,
    alt!(
        tag!("let") => {|_| Let}
    )
);

named!(operator(&[u8]) -> tokens::Operator,
    alt!(
        tag!("=") => {|_| Assignment} |
        tag!("+") => {|_| Addition}
    )
);

named!(punctuation(&[u8]) -> tokens::Punctuation,
    alt!(
        tag!("{") => {|_| Curly(Open)} |
        tag!("}") => {|_| Curly(Close)} |
        tag!("(") => {|_| Parenthesis(Open)} |
        tag!(")") => {|_| Parenthesis(Close)} |
        tag!(";") => {|_| SemiColon} |
        tag!(",") => {|_| Colon} |
        tag!("|") => {|_| Bar} |
        tag!("_") => {|_| Placeholder}
    )
);

named!(token(&[u8]) -> Token,
    alt!(
        punctuation => {Punctuation} |
        operator => {Operator} |
        reserved => {Reserved} |
        literal => {Literal} |
        identifier
    )
);

named!(pub tokenize(&[u8]) -> Tokens,
    map!(
        many0!(ws!(token)),
        |tokens| {
            Tokens {
                tokens
            }
        }
    )
);
