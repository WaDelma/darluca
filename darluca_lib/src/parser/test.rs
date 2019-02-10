use super::ast::{Ast, Ident, Expr, Module};
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
            Ident("main".into())
        ),
        ast("main", vec![
            Expr::Comment("Hello, World!".into())
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
                // I32 -> I32 -> I32 -> (I32 -> I32 -> I32)
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
