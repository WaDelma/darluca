use std::str;

use nom::{is_alphanumeric, is_digit};

use interner::Interner;

use self::tokens::{Token, Tokens};
use self::tokens::Token::*;
use self::tokens::Punctuation::*;
use self::tokens::Balanced::*;
use self::tokens::Operator::*;
use self::tokens::Reserved::*;
use self::tokens::Literal::*;
use self::tokens::Direction::*;

pub mod tokens;
#[cfg(test)]
mod test;

// TODO: Full UTF-8 support
// fn is_valid_for_ident(c: char) -> bool {
//     c.is_alphabetic() || c == '_'
// }

#[derive(Debug)]
pub struct Lexer<'ctx> {
    interner: &'ctx mut Interner,
}

impl<'ctx> Lexer<'ctx> {
    pub fn new(interner: &'ctx mut Interner) -> Self {
        Lexer { interner }
    }

    method!(identifier<Lexer<'ctx>>(&[u8]) -> Token, self,
        map!(
            verify!(
                take_while1!(is_alphanumeric),
                |val: &[u8]| {
                    let fst = char::from(val[0]);
                    fst.is_alphabetic() && fst.is_lowercase()
                }
            ),
            |ident| {
                str::from_utf8(ident)
                    .map(|i| self.interner.intern(i))
                    .map(Identifier)
                    .expect("is_alphabetic should ensure that ident is valid utf-8.")
            }
        )
    );

    method!(parse_type<Lexer<'ctx>>(&[u8]) -> Token, self,
        map!(
            verify!(
                take_while1!(is_alphanumeric),
                |val: &[u8]| {
                    let fst = char::from(val[0]);
                    fst.is_alphabetic() && fst.is_uppercase()
                }
            ),
            |ident| {
                str::from_utf8(ident)
                    .map(|i| self.interner.intern(i))
                    .map(Type)
                    .expect("is_alphabetic should ensure that ident is valid utf-8.")
            }
        )
    );

    method!(literal<Lexer<'ctx>>(&[u8]) -> tokens::Literal, self,
        alt!(
            map!(
                take_while1!(is_digit),
                |integer| {
                    str::from_utf8(integer)
                        .map(|i| self.interner.intern(i))
                        .map(Integer)
                        .expect("is_digit should ensure that integer is valid utf-8.")
                }
            )
        )
    );

    method!(reserved<Lexer<'ctx>>(&[u8]) -> tokens::Reserved, self,
        alt!(
            tag!("let") => {|_| Let} |
            tag!("true") => {|_| True} |
            tag!("false") => {|_| False} |
            tag!("if") => {|_| If} |
            tag!("else") => {|_| Else}
        )
    );

    method!(operator<Lexer<'ctx>>(&[u8]) -> tokens::Operator, self,
        alt!(
            tag!("=") => {|_| Assignment} |
            tag!("+") => {|_| Addition}
        )
    );

    method!(punctuation<Lexer<'ctx>>(&[u8]) -> tokens::Punctuation, self,
        alt!(
            tag!("{") => {|_| Curly(Open)} |
            tag!("}") => {|_| Curly(Close)} |
            tag!("[") => {|_| Square(Open)} |
            tag!("]") => {|_| Square(Close)} |
            tag!("<") => {|_| Angle(Open)} |
            tag!(">") => {|_| Angle(Close)} |
            tag!("(") => {|_| Parenthesis(Open)} |
            tag!(")") => {|_| Parenthesis(Close)} |
            tag!("->") => {|_| Arrow(Right)} |
            tag!("<-") => {|_| Arrow(Left)} |
            tag!(";") => {|_| SemiColon} |
            tag!(":") => {|_| Colon} |
            tag!(",") => {|_| Comma} |
            tag!("|") => {|_| Bar} |
            tag!("_") => {|_| Placeholder}
        )
    );

    method!(token<Lexer<'ctx>>(&[u8]) -> Token, mut self,
        alt!(
            call_m!(self.punctuation) => {Punctuation} |
            call_m!(self.operator) => {Operator} |
            call_m!(self.reserved) => {Reserved} |
            call_m!(self.literal) => {Literal} |
            call_m!(self.identifier) |
            call_m!(self.parse_type)
        )
    );

    method!(pub tokenize<Lexer<'ctx>>(&[u8]) -> Tokens, mut self,
        map!(
            many0!(ws!(call_m!(self.token))),
            |tokens| {
                Tokens {
                    tokens
                }
            }
        )
    );
}
