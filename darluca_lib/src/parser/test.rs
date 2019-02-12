use super::ast::{Ast, Ident, Expr, Module, Assignment, Literal, Function, Application};
use super::parse;
use nom::{types::CompleteStr, Err};

fn ast(name: &str, exprs: Vec<Expr>) -> Result<(CompleteStr, Ast), Err<CompleteStr>> {
    Ok((
        "".into(),
        Ast {
            modules: vec![
                Module {
                    name: Ident(name.into()),
                    exprs
                }
            ]
        }
    ))
}

#[test]
fn comment() {
    assert_eq!(
        parse(
            r#"
            //Hello, World!
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Comment("Hello, World!".into())
        ])
    );
}

#[test]
fn assignment() {
    assert_eq!(
        parse(
            r#"
            x := 10
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Assign(Assignment {
                lhs: "x".into(),
                rhs: Box::new(Expr::Lit(Literal::Integer("10".into())))
            })
        ])
    );
}

#[test]
fn function() {
    assert_eq!(
        parse(
            r#"
            (x, y) {
                x
            }
            z(k,)
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Fun(Function {
                params: vec!["x".into(), "y".into()],
                exprs: vec![Expr::Ident("x".into())]
            }),
            Expr::App(Application {
                fun: Box::new(Expr::Ident("z".into())),
                params: vec![Expr::Ident("k".into())],
            })
        ])
    );
}

#[test]
fn product() {
    assert_eq!(
        parse(
            r#"
            []
            [x, y]
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Prod(vec![]),
            Expr::Prod(vec![
                Expr::Ident("x".into()),
                Expr::Ident("y".into())
            ])
        ])
    );
}

#[test]
fn sum() {
    assert_eq!(
        parse(
            r#"
            |x|
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Sum(Box::new(Expr::Ident("x".into()))),
        ])
    );
}


#[test]
fn idents_starting_with_reserved_words() {
    assert_eq!(
        parse(
            r#"
            iffy puberty elsebeths modulin isis
            "#.into(),
            "main".into()
        ),
        ast("main", vec![
            Expr::Ident("iffy".into()),
            Expr::Ident("puberty".into()),
            Expr::Ident("elsebeths".into()),
            Expr::Ident("modulin".into()),
            Expr::Ident("isis".into()),
        ])
    );
}

#[test]
fn test() {
    println!(
        "{:#?}",
        parse(
            r#"
                // Comment
                : I32 -> I32 -> I32 -> (I32 -> I32 -> I32)
                x := (a, b, c) {
                    (inner:y(a))(b, c)
                }
                mod inner {
                    y := (d) {
                        (z, w) {
                            d
                        }
                    }
                }
            "#.into(),
            Ident("main".to_owned()),
        )
    );
}
