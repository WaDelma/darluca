use super::ast::Ident;
use super::parse;

#[test]
fn test() {
    println!(
        "{:?}",
        parse(
            r#""#,
            // r#"
            //     // Comment
            //     x := (a, b, c) {
            //         x(a, b, c)
            //     }
            //     mod inner {
            //         y := () {}
            //     }
            // "#,
            Ident("main".to_owned()),
        )
    );
}
