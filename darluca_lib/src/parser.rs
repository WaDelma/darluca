use nom::{
    alt, do_parse, many0, map, named, named_args, separated_list, tag, take_while1, take_until, ws, alt_complete,
};

use ast::{Application, Ast, Expr, Function, Ident, Module};

mod ast;
#[cfg(test)]
mod test;

named_args!(pub parse(name: Ident)<&str, Ast>,
    map!(
        ws!(many0!(expr)),
        |exprs| Ast {
            modules: vec![Module {
                name,
                exprs
            }]
        }
    )
);

named!(expr<&str, Expr>,
    alt_complete!(
        comment => {Expr::Comment} |
        module => {Expr::Mod} |
        function => {Expr::Fun} |
        application => {Expr::App} | 
        ident => {Expr::Ident}
    )
);

named!(module<&str, Module>,
    do_parse!(
        tag!("mod") >>
        name: ident >>
        tag!("{") >>
        exprs: many0!(expr) >>
        tag!("}") >>
        (Module { name,  exprs })
    )
);

named!(function<&str, Function>,
    do_parse!(
        name: ident >>
        tag!(":=") >>
        tag!("(") >>
        params: separated_list!(
            tag!(","),
            ident
        ) >>
        tag!(")") >>
        tag!("{") >>
        exprs: many0!(expr) >>
        tag!("}") >>
        (Function { signature: None, name, params, exprs })
    )
);

named!(application<&str, Application>,
    do_parse!(
        fun: expr >>
        tag!("(") >>
        params: many0!(expr) >>
        tag!(")") >>
        (Application { fun: Box::new(fun),  params })
    )
);

named!(comment<&str, String>,
    do_parse!(
        tag!("//") >>
        comment: take_until!("\n") >>
        (comment.to_owned())
    )
);

named!(ident<&str, Ident>,
    map!(
        take_while1!(
            |c: char| c.is_alphanumeric()
        ),
        |s| Ident(s.to_owned())
    )
);
