use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use nom::{named_args, types::CompleteStr};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Associativity {
    Left,
    Right,
    Non,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Fixity {
    Prefix,
    Infix(Associativity),
    Postfix,
    Closed,
}

#[derive(Clone, Debug)]
pub struct Operator {
    names: Vec<String>,
    fixity: Fixity,
}

pub struct Precedence {
    ops: HashMap<Fixity, Vec<Operator>>,
    succs: Vec<Rc<Precedence>>,
}

impl Precedence {
    fn ops(&self, fix: Fixity) -> &[Operator] {
        &self.ops.get(&fix).map(|v| &v[..]).unwrap_or(&[][..])
    }

    fn succs(&self) -> &[Rc<Precedence>] {
        &self.succs[..]
    }
}

pub struct PrecedenceGraph(Vec<Precedence>);

pub type Res<'a, T> = Result<(&'a str, T), ()>;

pub struct Oper {
    op: Operator,
    params: Vec<Oper>,
}

impl fmt::Debug for Oper {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut n = self.op.names.join("_");
        match self.op.fixity {
            Fixity::Infix(Associativity::Left) => {
                n = format!("_{}_ <-", n);
            }
            Fixity::Infix(Associativity::Non) => {
                n = format!("_{}_", n);
            }
            Fixity::Infix(Associativity::Right) => {
                n = format!("_{}_ ->", n);
            }
            Fixity::Postfix => {
                n = format!("_{}", n);
            }
            Fixity::Prefix => {
                n = format!("{}_", n);
            }
            Fixity::Closed => {}
        }
        write!(fmt, "{}", n)?;
        if !self.params.is_empty() {
            write!(fmt, "{:#?}", self.params)?;
        }
        Ok(())
    }
}

#[test]
fn oper() {
    panic!(
        "{:#?}",
        parse_expr(
            "a+b*(c+d+e^a^b)",
            &Precedence {
                ops: {
                    let mut ops = HashMap::new();
                    let fixity = Fixity::Infix(Associativity::Left);
                    ops.insert(
                        fixity.clone(),
                        vec![Operator {
                            names: vec!["+".into()],
                            fixity,
                        }],
                    );
                    ops
                },
                succs: vec![Rc::new(Precedence {
                    ops: {
                        let mut ops = HashMap::new();
                        let fixity = Fixity::Infix(Associativity::Left);
                        ops.insert(
                            fixity.clone(),
                            vec![Operator {
                                names: vec!["*".into()],
                                fixity,
                            }],
                        );
                        ops
                    },
                    succs: vec![Rc::new(Precedence {
                        ops: {
                            let mut ops = HashMap::new();
                            let fixity = Fixity::Infix(Associativity::Right);
                            ops.insert(
                                fixity.clone(),
                                vec![Operator {
                                    names: vec!["^".into()],
                                    fixity,
                                }],
                            );
                            ops
                        },
                        succs: vec![Rc::new(Precedence {
                            ops: {
                                let mut ops = HashMap::new();
                                let fixity = Fixity::Closed;
                                ops.insert(
                                    fixity.clone(),
                                    vec![
                                        Operator {
                                            names: vec!["a".into()],
                                            fixity: fixity.clone(),
                                        },
                                        Operator {
                                            names: vec!["b".into()],
                                            fixity: fixity.clone(),
                                        },
                                        Operator {
                                            names: vec!["c".into()],
                                            fixity: fixity.clone(),
                                        },
                                        Operator {
                                            names: vec!["d".into()],
                                            fixity: fixity.clone(),
                                        },
                                        Operator {
                                            names: vec!["e".into()],
                                            fixity: fixity.clone(),
                                        },
                                        Operator {
                                            names: vec!["(".into(), ")".into()],
                                            fixity,
                                        },
                                    ],
                                );
                                ops
                            },
                            succs: vec![]
                        })]
                    })]
                })]
            }
        )
    );
}

pub fn parse_expr<'a>(src: &'a str, prec: &Precedence) -> Res<'a, Oper> {
    println!("parse_expr: {}", src);
    let mut stack = vec![prec];
    while let Some(p) = stack.pop() {
        match parse_oper(src, p, prec) {
            Ok((src, oper)) => {
                return Ok((src, oper));
            }
            _ => {}
        }
        for s in p.succs() {
            stack.push(s);
        }
    }
    Err(())
}

fn parse_oper<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
    println!("parse_oper: {}", src);
    parse_op(src, prec.ops(Fixity::Closed), root)
        .or_else(|_| {
            parse_stronger(src, prec, root).and_then(|(src, oper)| {
                parse_op(src, prec.ops(Fixity::Infix(Associativity::Non)), root).and_then(
                    |(src, mut oper2)| {
                        let (src, oper3) = parse_stronger(src, prec, root)?;
                        oper2.params.insert(0, oper);
                        oper2.params.push(oper3);
                        Ok((src, oper2))
                    },
                )
            })
        })
        .or_else(|_| {
            let (mut src, mut oper) = parse_right(src, prec, root)?;
            while let Ok((src2, oper2)) = parse_right(src, prec, root) {
                oper.params.push(oper2);
                src = src2;
            }
            let (src, oper2) = parse_stronger(src, prec, root)?;
            oper.params.push(oper2);
            Ok((src, oper))
        })
        .or_else(|()| {
            parse_stronger(src, prec, root).and_then(|(src, oper2)| {
                let (mut src, mut oper) = parse_left(src, prec, root)?;
                oper.params.insert(0, oper2);
                while let Ok((src2, mut oper2)) = parse_left(src, prec, root) {
                    oper2.params.insert(0, oper);
                    oper = oper2;
                    src = src2;
                }
                Ok((src, oper))
            })
        })
}

fn parse_stronger<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
    println!("parse_stronger: {}", src);
    let mut stack: Vec<_> = prec.succs().iter().collect();
    while let Some(p) = stack.pop() {
        match parse_oper(src, p, root) {
            Ok((src, oper)) => {
                return Ok((src, oper));
            }
            _ => {}
        }
        for s in p.succs() {
            stack.push(s);
        }
    }
    Err(())
}

fn parse_left<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
    println!("parse_left: {}", src);
    parse_op(src, prec.ops(Fixity::Postfix), root).or_else(|_| {
        parse_op(src, prec.ops(Fixity::Infix(Associativity::Left)), root).and_then(
            |(src, mut oper)| {
                let (src, oper2) = parse_stronger(src, prec, root)?;
                oper.params.push(oper2);
                Ok((src, oper))
            },
        )
    })
}

fn parse_right<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
    println!("parse_right: {}", src);
    parse_op(src, prec.ops(Fixity::Prefix), root).or_else(|_| {
        parse_stronger(src, prec, root).and_then(|(src, oper)| {
            let (src, mut oper2) =
                parse_op(src, prec.ops(Fixity::Infix(Associativity::Right)), root)?;
            oper2.params.insert(0, oper);
            Ok((src, oper2))
        })
    })
}

// named_args!(parse_op2(ops: &[Operator], root: &Precedence)<CompleteString, Oper>,

// );

fn parse_op<'a>(mut src: &'a str, ops: &[Operator], root: &Precedence) -> Res<'a, Oper> {
    println!("parse_op: {}", src);
    if src.is_empty() {
        return Err(());
    }
    let orig_src = src;
    'ops: for op in ops {
        let mut params = Vec::with_capacity(op.names.len() - 1);
        for (i, n) in op.names.iter().enumerate() {
            if &src[0..n.len()] == &*n {
                src = &src[n.len()..];
                if i < op.names.len() - 1 {
                    match parse_expr(src, root) {
                        Ok((src2, expr)) => {
                            params.push(expr);
                            src = src2;
                        }
                        Err(()) => {
                            src = orig_src;
                            continue 'ops;
                        }
                    }
                }
            } else {
                src = orig_src;
                continue 'ops;
            }
        }
        return Ok((
            src,
            Oper {
                op: op.clone(),
                params,
            },
        ));
    }
    return Err(());
}
