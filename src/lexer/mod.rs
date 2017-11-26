use std::str;

use nom::{is_digit, is_alphabetic};
use symtern::prelude::*;

use interner::Interner;

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

#[derive(Debug)]
pub struct Lexer<'ctx> {
    interner: &'ctx mut Interner,
}

impl<'ctx> Lexer<'ctx> {
    pub fn new(interner: &'ctx mut Interner) -> Self {
        Lexer {
            interner,
        }
    }

    method!(identifier<Lexer<'ctx>>(&[u8]) -> Token, self,
        map!(
            take_while1!(is_alphabetic),
            |ident| {
                str::from_utf8(ident)
                    .map(|i| self.interner.intern(i).unwrap())
                    .map(Identifier)
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
                        .map(|i| self.interner.intern(i).unwrap())
                        .map(Integer)
                        .expect("is_digit should ensure that integer is valid utf-8.")
                }
            )
        )
    );

    method!(reserved<Lexer<'ctx>>(&[u8]) -> tokens::Reserved, self,
        alt!(
            tag!("let") => {|_| Let}
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
            tag!(";") => {|_| SemiColon} |
            tag!(",") => {|_| Colon} |
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
            call_m!(self.identifier)
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