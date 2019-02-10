use nom::{
    do_parse, map, named, named_args, tag, take_while1, take_until, ws, dbg, types::CompleteStr, delimited, opt, take_while_m_n, alt, many0, separated_list,
};

use ast::{Application, Ast, Expr, Function, Ident, Module, Assignment, Signature, Publicity, FunTy, Type};

mod ast;
#[cfg(test)]
mod test;

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
        dbg!(comment) => {Expr::Comment} |
        dbg!(assignment) => {Expr::Assign} |
        dbg!(module) => {Expr::Mod} |
        dbg!(function) => {Expr::Fun} |
        dbg!(application) => {Expr::App} | 
        dbg!(signature) => {Expr::Sign} |
        dbg!(parenthesed_expr) => {|e| Expr::Sub(Box::new(e))} |
        dbg!(ident) => {Expr::Ident}
    ))
);

named!(signature<CompleteStr, Signature>,
    do_parse!(
        is_pub: opt!(tag!("pub")) >>
        ty: ty >>
        (Signature { publicity: is_pub.map(|_| Publicity::Pub).unwrap_or(Publicity::Priv), ty })
    )
);

named!(ty<CompleteStr, Type>,
    alt!(
        function_type => {|t| Type::Fun(Box::new(t))} |
        product_type |
        sum_type |
        tag!("_") => {|_| Type::Placeholder } |
        parenthesed_type => {|t| Type::Paren(Box::new(t))} |
        type_ident => {Type::Ty}
    )
);

named!(parenthesed_type<CompleteStr, Type>,
    delimited!(tag!("("), ty, tag!(")"))
);

named!(function_type<CompleteStr, FunTy>,
    do_parse!(
        lhs: type_ident >>
        tag!("->") >>
        rhs: ty >>
        (FunTy { lhs: Type::Ty(lhs), rhs })
    )
);

named!(product_type<CompleteStr, Type>,
    do_parse!(
        tag!("[") >>

        tag!("]") >>
        (Type::Prod(vec![]))
    )
);

named!(sum_type<CompleteStr, Type>,
    do_parse!(
        tag!("|") >>
        
        tag!("|") >>
        (Type::Sum(vec![]))
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
        ws!(tag!("mod")) >>
        name: ws!(ident) >>
        ws!(tag!("{")) >>
        exprs: ws!(many0!(expr)) >>
        ws!(tag!("}")) >>
        (Module { name,  exprs })
    )
);

named!(assignment<CompleteStr, Assignment>,
    do_parse!(
        lhs: ws!(ident) >>
        ws!(tag!(":=")) >>
        rhs: ws!(expr) >>
        (Assignment { lhs, rhs: Box::new(rhs) })
    )
);

named!(function<CompleteStr, Function>,
    do_parse!(
        ws!(tag!("(")) >>
        params: ws!(separated_list!(
            ws!(tag!(",")),
            ws!(ident)
        )) >>
        opt!(ws!(tag!(","))) >>
        ws!(tag!(")")) >>
        ws!(tag!("{")) >>
        exprs: ws!(many0!(expr)) >>
        ws!(tag!("}")) >>
        (Function { params, exprs })
    )
);

named!(application<CompleteStr, Application>,
    do_parse!(
        fun: ws!(alt!(
            ws!(parenthesed_expr) => {|e| Expr::Sub(Box::new(e))} |
            ws!(ident) => {Expr::Ident}
        )) >>
        ws!(tag!("(")) >>
        params: ws!(separated_list!(
            ws!(tag!(",")),
            ws!(expr)
        )) >>
        opt!(ws!(tag!(","))) >>
        ws!(tag!(")")) >>
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

named!(type_ident<CompleteStr, Ident>,
    do_parse!(
        fst: take_while_m_n!(1, 1, |c| unic_ucd_ident::is_xid_start(c) && char::is_uppercase(c)) >>
        rest: take_while1!(|c| unic_ucd_ident::is_xid_continue(c) || c == ':') >>
        (Ident(fst.to_string() + &*rest))
    )
);

named!(ident<CompleteStr, Ident>,
    do_parse!(
        fst: take_while_m_n!(1, 1, |c| unic_ucd_ident::is_xid_start(c) && char::is_lowercase(c)) >>
        rest: take_while1!(|c| unic_ucd_ident::is_xid_continue(c) || c == ':') >>
        (Ident(fst.to_string() + &*rest))
    )
);