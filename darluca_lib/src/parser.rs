use nom::{
    alt, dbg, delimited, digit, do_parse, eat_separator, many0, many1, map, named, named_args, opt,
    sep, separated_list, tag, take_until, take_while, take_while1, take_while_m_n,
    types::CompleteStr,
};

use ast::{
    Application, Assignment, Ast, Condition, Conditional, Expr, Function, Ident, Literal, Module,
    Mutability, Pattern, Publicity, Signature,
};

use types::ty;

mod ast;
#[cfg(test)]
mod test;
mod types;

named!(pub space<CompleteStr, CompleteStr>, eat_separator!(" \r\n"));

#[macro_export]
macro_rules! ws (
    ($i:expr, $($args:tt)*) => (
        {
            use nom::{Convert, Err};

            match sep!($i, space, $($args)*) {
                Err(e) => Err(e),
                Ok((i1, o)) => {
                    match space(i1) {
                        Err(e) => Err(Err::convert(e)),
                        Ok((i2, _)) => Ok((i2, o))
                    }
                }
            }
        }
    )
);

named_args!(pub parse(name: Ident)<CompleteStr, Ast>,
    map!(
        many0!(expr),
        |exprs| Ast {
            modules: vec![Module {
                name,
                exprs
            }]
        }
    )
);

named!(expr<CompleteStr, Expr>,
    ws!(alt!(
        comment => {Expr::Comment} |
        conditional => {Expr::Cond} |
        assignment => {Expr::Assign} |
        module => {Expr::Mod} |
        function => {Expr::Fun} |
        application => {Expr::App} |
        signature => {Expr::Sign} |
        parenthesed_expr => {|e| Expr::Sub(Box::new(e))} |
        literal => {Expr::Lit} |
        sum |
        product |
        ident => {Expr::Ident}
    ))
);

named!(conditional<CompleteStr, Conditional>,
    do_parse!(
        tag!("if") >>
        value: expr >>
        tag!("{") >>
        condition: ws!(condition) >>
        tag!("}") >>
        otherwise: opt!(do_parse!(
            ws!(tag!("else")) >>
            tag!("{") >>
            exprs: many0!(expr) >>
            tag!("}") >>
            (exprs)
        )) >>
        (Conditional {
            value: Box::new(value),
            condition,
            otherwise
        })
    )
);

named!(condition<CompleteStr, Condition>,
    alt!(
        multi_condition |
        single_condition
    )
);

named!(multi_condition<CompleteStr, Condition>,
    map!(
        many1!(do_parse!(
            ws!(tag!("is")) >>
            pattern: pattern >>
            tag!("{") >>
            exprs: many0!(expr) >>
            tag!("}") >>
            ((pattern, exprs))
        )),
        |conds| Condition::Multi(conds)
    )
);

named!(single_condition<CompleteStr, Condition>,
    map!(
        many0!(expr),
        |exprs| Condition::Single(exprs)
    )
);

named!(pattern<CompleteStr, Pattern>,
    map!(
        tag!("NOT IMPLEMENTED... THERE REALLY NEEDS TO BE A UNIMPLEMENTED COMBINATOR. WHY AM I YELLING?"),
        |_| Pattern {}
    )
);

named!(sum<CompleteStr, Expr>,
    do_parse!(
        tag!("|") >>
        expr: expr >>
        tag!("|") >>
        (Expr::Sum(Box::new(expr)))
    )
);

named!(product<CompleteStr, Expr>,
    do_parse!(
        tag!("[") >>
        params: separated_list!(
            ws!(tag!(",")),
            expr
        ) >>
        opt!(ws!(tag!(","))) >>
        tag!("]") >>
        (Expr::Prod(params))
    )
);

named!(literal<CompleteStr, Literal>,
    alt!(
        integer
    )
);

named!(integer<CompleteStr, Literal>,
    map!(
        digit,
        |s| Literal::Integer(s.to_string())
    )
);

named!(signature<CompleteStr, Signature>,
    do_parse!(
        ws!(tag!(":")) >>
        is_pub: opt!(ws!(tag!("pub"))) >>
        is_mut: opt!(ws!(tag!("mut"))) >>
        ty: ty >>
        (Signature {
            mutability: is_mut.map(|_| Mutability::Mutable).unwrap_or(Mutability::Immutable),
            publicity: is_pub.map(|_| Publicity::Public).unwrap_or(Publicity::Private),
            ty
        })
    )
);

named!(parenthesed_expr<CompleteStr, Expr>,
    delimited!(
        ws!(tag!("(")),
        ws!(expr),
        ws!(tag!(")"))
    )
);

named!(module<CompleteStr, Module>,
    do_parse!(
        tag!("mod") >>
        name: ws!(ident) >>
        tag!("{") >>
        exprs: many0!(expr) >>
        tag!("}") >>
        (Module { name,  exprs })
    )
);

named!(assignment<CompleteStr, Assignment>,
    do_parse!(
        lhs: ident >>
        ws!(tag!(":=")) >>
        rhs: expr >>
        (Assignment { lhs, rhs: Box::new(rhs) })
    )
);

named!(function<CompleteStr, Function>,
    do_parse!(
        tag!("(") >>
        params: separated_list!(
            ws!(tag!(",")),
            ident
        ) >>
        opt!(ws!(tag!(","))) >>
        tag!(")") >>
        ws!(tag!("{")) >>
        exprs: many0!(expr) >>
        tag!("}") >>
        (Function { params, exprs })
    )
);

named!(application<CompleteStr, Application>,
    do_parse!(
        fun: alt!(
            parenthesed_expr => {|e| Expr::Sub(Box::new(e))} |
            ident => {Expr::Ident}
        ) >>
        ws!(tag!("(")) >>
        params: separated_list!(
            ws!(tag!(",")),
            expr
        ) >>
        opt!(ws!(tag!(","))) >>
        tag!(")") >>
        (Application { fun: Box::new(fun),  params })
    )
);

named!(comment<CompleteStr, String>,
    do_parse!(
        tag!("//") >>
        comment: take_until!("\n") >>
        (comment.to_string())
    )
);

named!(ident<CompleteStr, Ident>,
    do_parse!(
        fst: take_while_m_n!(1, 1, |c| unic_ucd_ident::is_xid_start(c) && char::is_lowercase(c)) >>
        rest: take_while!(|c| unic_ucd_ident::is_xid_continue(c) || c == ':') >>
        (Ident(fst.to_string() + &*rest))
    )
);
