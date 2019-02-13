use nom::{
    alt, delimited, do_parse, named, opt, tag, take_while, take_while_m_n, types::CompleteStr, ws,
};

use crate::parser::ast::{FunTy, Ident, Type};

named!(pub ty<CompleteStr, Type>,
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
        params: ws!(separated_list!(
            ws!(tag!(",")),
            ty
        )) >>
        opt!(ws!(tag!(","))) >>
        tag!("]") >>
        (Type::Prod(params))
    )
);

named!(sum_type<CompleteStr, Type>,
    do_parse!(
        tag!("|") >>
        tag!("|") >>
        (Type::Sum(vec![]))
    )
);

named!(type_ident<CompleteStr, Ident>,
    do_parse!(
        fst: take_while_m_n!(1, 1, |c| unic_ucd_ident::is_xid_start(c) && char::is_uppercase(c)) >>
        rest: take_while!(|c| unic_ucd_ident::is_xid_continue(c) || c == ':') >>
        (Ident(fst.to_string() + &*rest))
    )
);
