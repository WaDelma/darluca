use nom::named;

use ast::Ast;

mod ast;

named!(parse<&str, Ast>,
    
);

fn parse(code: &str) -> Ast {}
