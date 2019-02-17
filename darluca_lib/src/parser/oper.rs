use nom::lib::std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::once;
use std::rc::Rc;

use nom::{
    alt, apply, cond, do_parse, error_position, many1, map, named_args, preceded, tag, tuple,
    types::CompleteStr, IResult, ws, dbg
};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Associativity {
    Left,
    Right,
    Non,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Fixity {
    Prefix,
    Infix(Associativity),
    Postfix,
    Closed,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Operator {
    names: Vec<String>,
    fixity: Fixity,
}

impl fmt::Debug for Operator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let n = self.names.join("_");
        match self.fixity {
            Fixity::Infix(Associativity::Left) => {
                write!(fmt, "_{}_ <-", n)
            }
            Fixity::Infix(Associativity::Non) => {
                write!(fmt, "_{}_", n)
            }
            Fixity::Infix(Associativity::Right) => {
                write!(fmt, "_{}_ ->", n)
            }
            Fixity::Postfix => {
                write!(fmt, "_{}", n)
            }
            Fixity::Prefix => {
                write!(fmt, "{}_", n)
            }
            Fixity::Closed => write!(fmt, "{}", n)
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct Precedence {
    ops: Vec<(Fixity, Vec<Operator>)>,
    succs: Vec<Rc<Precedence>>,
}

impl Precedence {
    pub fn new(ops: Vec<Operator>) -> Self {
        let mut ops_: Vec<(Fixity, Vec<Operator>)> = vec![];
        for o in ops {
            if let Some((_, ops)) = ops_.iter_mut().filter(|(f, _)| f == &o.fixity).next() {
                ops.push(o);
            } else {
                ops_.push((o.fixity, vec![o]));
            }
        }
        Precedence { ops: ops_, succs: vec![] }
    }

    pub fn with_successors(ops: Vec<Operator>, succs: Vec<Rc<Precedence>>) -> Self {
        let mut ops_: Vec<(Fixity, Vec<Operator>)> = vec![];
        for o in ops {
            if let Some((_, ops)) = ops_.iter_mut().filter(|(f, _)| f == &o.fixity).next() {
                ops.push(o);
            } else {
                ops_.push((o.fixity, vec![o]));
            }
        }
        Precedence { ops: ops_, succs }
    }

    fn ops(&self, fix: Fixity) -> &[Operator] {
        self.ops
            .iter()
            .filter(|(f, _)| f == &fix)
            .next()
            .map(|(_, v)| &v[..])
            .unwrap_or(&[][..])
    }

    fn succs(&self) -> &[Rc<Precedence>] {
        &self.succs[..]
    }
}

pub struct Bfs<'a> {
    queue: VecDeque<&'a Precedence>,
    visited: HashSet<&'a Precedence>,
}

impl<'a> Bfs<'a> {
    fn new<I: IntoIterator<Item = &'a Precedence>>(start: I) -> Self {
        let mut queue = VecDeque::new();
        queue.extend(start);
        Bfs {
            queue,
            visited: HashSet::new(),
        }
    }
}

impl<'a> Iterator for Bfs<'a> {
    type Item = &'a Precedence;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(p) = self.queue.pop_front() {
            if self.visited.contains(&p) {
                continue;
            }
            self.visited.insert(p);
            for p in p.succs() {
                self.queue.push_back(p);
            }
            return Some(p);
        }
        None
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
        write!(fmt, "{:?}", self.op)?;
        if !self.params.is_empty() {
            write!(fmt, "{:#?}", self.params)?;
        }
        Ok(())
    }
}

#[test]
fn oper1() {
    panic!(
        "{:#?}",
        parse_expr(
            "a+b+c*(a^b^c^d^e)".into(),
            &PrecedenceGraph(vec![Precedence::with_successors(
                vec![Operator {
                    names: vec!["+".into()],
                    fixity: Fixity::Infix(Associativity::Left)
                }],
                vec![Rc::new(Precedence::with_successors(
                    vec![Operator {
                        names: vec!["*".into()],
                        fixity: Fixity::Infix(Associativity::Left)
                    }],
                    vec![Rc::new(Precedence::with_successors(
                        vec![Operator {
                            names: vec!["^".into()],
                            fixity: Fixity::Infix(Associativity::Right),
                        }],
                        vec![Rc::new(Precedence::with_successors(
                            vec![
                                Operator {
                                    names: vec!["a".into()],
                                    fixity: Fixity::Closed,
                                },
                                Operator {
                                    names: vec!["b".into()],
                                    fixity: Fixity::Closed,
                                },
                                Operator {
                                    names: vec!["c".into()],
                                    fixity: Fixity::Closed,
                                },
                                Operator {
                                    names: vec!["d".into()],
                                    fixity: Fixity::Closed,
                                },
                                Operator {
                                    names: vec!["e".into()],
                                    fixity: Fixity::Closed,
                                },
                                Operator {
                                    names: vec!["(".into(), ")".into()],
                                    fixity: Fixity::Closed,
                                },
                            ],
                            vec![]
                        ))]
                    ))]
                ))]
            )
        ]))
    );
}

#[test]
fn oper2() {
    let oper = Rc::new(Precedence::new(vec![
        Operator {
            names: vec!["b".into()],
            fixity: Fixity::Closed,
        },
        Operator {
            names: vec!["n".into()],
            fixity: Fixity::Closed,
        },
        Operator {
            names: vec!["(".into(), ")".into()],
            fixity: Fixity::Closed,
        },
    ]));
    let mut ifthenelse = Precedence::new(vec![
        Operator {
            names: vec!["if".into(), "then".into(), "else".into()],
            fixity: Fixity::Prefix,
        }
    ]);
    ifthenelse.succs.push(oper.clone());
    let mut bang = Precedence::new(vec![
        Operator {
            names: vec!["!".into()],
            fixity: Fixity::Postfix,
        }
    ]);
    bang.succs.push(oper.clone());
    let mut arith = Precedence::new(vec![
        Operator {
            names: vec!["+".into()],
            fixity: Fixity::Infix(Associativity::Left)
        },
        Operator {
            names: vec!["-".into()],
            fixity: Fixity::Infix(Associativity::Left)
        }
    ]);
    arith.succs.push(oper.clone());
    let mut comp = Precedence::new(vec![
        Operator {
            names: vec!["==".into()],
            fixity: Fixity::Infix(Associativity::Non)
        }
    ]);
    comp.succs.push(oper.clone());
    comp.succs.push(Rc::new(bang));
    comp.succs.push(Rc::new(arith));
    let mut and = Precedence::new(vec![
        Operator {
            names: vec!["∧".into()],
            fixity: Fixity::Infix(Associativity::Right)
        }
    ]);
    and.succs.push(oper);
    and.succs.push(Rc::new(comp));

    // TODO: Becuase the order precendence dag is visited it parses `n` and thus doesn't realise that there is still + coming...
    // This is not explained in the paper.. It just says that it's choice from all stronger precedences (which `n` is...)
    // Maybe topological ordering?
    panic!(
        "{:#?}",
        parse_expr(
            "b ∧ n + n".into(),
            // "if b ∧ n + n == n ! then n else ( n + n - n )".into(),
            &PrecedenceGraph(vec![and, ifthenelse])
        )
    );
}

macro_rules! one_of {
    ($i:expr, $elem:ident in $iter:expr, $submac:ident!( $($args:tt)* )) => {
        {
            let mut _ok = None;
            let mut _err = None;
            let mut _i = $i.clone();
            for $elem in IntoIterator::into_iter($iter) {
                let _input = _i.clone();
                match $submac!(_input, $($args)*) {
                    Ok((j, o)) => {
                        _i = j;
                        _ok = Some(o);
                        break;
                    }
                    err => _err = Some(err),
                }
            }
            if let Some(o) = _ok {
                Ok((_i, o))
            } else if let Some(e) = _err {
                e
            } else {
                // TODO: what to do?
                Err(nom::Err::Error(error_position!(_i, nom::ErrorKind::Many0)))
            }
        }
    };
}

macro_rules! for_each {
    ($i:expr, $elem:ident in $iter:expr, $submac:ident!( $($args:tt)* )) => {
        {
            let mut _i = $i;
            let mut _v = vec![];
            let mut _err = None;
            for $elem in IntoIterator::into_iter($iter) {
                let _input = Clone::clone(&_i);
                match $submac!(_input, $($args)*) {
                    Ok((j, e)) => {
                        _i = j;
                        _v.push(e);
                    }
                    Err(e) => {
                        _err = Some(e);
                        break;
                    }
                }
            }
            if let Some(e) = _err {
                Err(e)
            } else {
                Ok((_i, _v))
            }
        }
    };
}

macro_rules! trace {
    ($i:expr, $submac:ident!( $($args:tt)* ), $m:expr) => {
        {
            println!("{}: `{}`", $m, $i);
            $submac!($i, $($args)*)
        }
        
    };
}

named_args!(pub parse_expr<'a>(prec: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(one_of!(
        p in Bfs::new(&prec.0),
        apply!(parse_oper, p, prec)
    ), "parse_expr")
);

named_args!(parse_oper<'a>(prec: &Precedence, root: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(alt!(
        apply!(parse_op, prec.ops(Fixity::Closed), root) |
        map!(
            tuple!(
                apply!(parse_stronger, prec, root),
                apply!(parse_op, prec.ops(Fixity::Infix(Associativity::Non)), root),
                apply!(parse_stronger, prec, root)
            ),
            |(oper, mut oper2, oper3)| {
                oper2.params.insert(0, oper);
                oper2.params.push(oper3);
                oper2
            }
        ) |
        map!(
            tuple!(
                many1!(trace!(apply!(parse_right, prec, root), "right")),
                trace!(apply!(parse_stronger, prec, root), format!("no {:#?}", prec))
            ),
            |(opers, mut oper)| {
                for mut op in opers.into_iter().rev() {
                    op.params.push(oper);
                    oper = op;
                }
                oper
            }
        ) |
        map!(
            tuple!(
                apply!(parse_stronger, prec, root),
                many1!(apply!(parse_left, prec, root))
            ),
            |(mut oper, opers)| {
                for mut op in opers.into_iter() {
                    op.params.insert(0, oper);
                    oper = op;
                }
                oper
            }
        )
    ), format!("parse_oper {:#?}", prec))
);

named_args!(parse_stronger<'a>(prec: &Precedence, root: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(one_of!(
        p in Bfs::new(prec.succs().iter().map(|p| &**p)),
        apply!(parse_oper, p, root)
    ), format!("parse_stronger"))
);

named_args!(parse_left<'a>(prec: &Precedence, root: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(alt!(
        apply!(parse_op, prec.ops(Fixity::Postfix), root) |
        map!(
            tuple!(
                apply!(parse_op, prec.ops(Fixity::Infix(Associativity::Left)), root),
                apply!(parse_stronger, prec, root)
            ),
            |(mut oper, oper2)| {
                oper.params.insert(0, oper2);
                oper
            }
        )
    ), "parse_left")
);

named_args!(parse_right<'a>(prec: &Precedence, root: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(alt!(
        apply!(parse_op, prec.ops(Fixity::Prefix), root) |
        map!(
            tuple!(
                apply!(parse_stronger, prec, root),
                apply!(parse_op, prec.ops(Fixity::Infix(Associativity::Right)), root)
            ),
            |(oper, mut oper2)| {
                oper2.params.insert(0, oper);
                oper2
            }
        )
    ), "parse_right")
);

named_args!(parse_op<'a>(ops: &[Operator], root: &PrecedenceGraph)<CompleteStr<'a>, Oper>,
    trace!(one_of!(
        op in ops,
        trace!(map!(
            for_each!(
                n in op.names.iter().enumerate(),
                preceded!(
                    ws!(tag!(&**n.1)),
                    cond!(
                        n.0 < op.names.len() - 1,
                        apply!(parse_expr, root)
                    )
                )
            ),
            |params| {
                Oper {
                    op: op.clone(),
                    params: params.into_iter().filter_map(|x| x).collect()
                }
            }
        ), format!("parse_op({:?})", op))
    ), "parse_op")
);

// fn parse_stronger<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
//     println!("parse_stronger: {}", src);
//     let mut stack: Vec<_> = prec.succs().iter().collect();
//     while let Some(p) = stack.pop() {
//         match parse_oper(src, p, root) {
//             Ok((src, oper)) => {
//                 return Ok((src, oper));
//             }
//             _ => {}
//         }
//         for s in p.succs() {
//             stack.push(s);
//         }
//     }
//     Err(())
// }
// pub fn parse_expr<'a>(src: &'a str, prec: &Precedence) -> Res<'a, Oper> {
//     println!("parse_expr: {}", src);
//     let mut stack = vec![prec];
//     while let Some(p) = stack.pop() {
//         match parse_oper(src, p, prec) {
//             Ok((src, oper)) => {
//                 return Ok((src, oper));
//             }
//             _ => {}
//         }
//         for s in p.succs() {
//             stack.push(s);
//         }
//     }
//     Err(())
// }
// fn parse_oper<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
//     println!("parse_oper: {}", src);
//     parse_op(src, prec.ops(Fixity::Closed), root)
//         .or_else(|_| {
//             parse_stronger(src, prec, root).and_then(|(src, oper)| {
//                 parse_op(src, prec.ops(Fixity::Infix(Associativity::Non)), root).and_then(
//                     |(src, mut oper2)| {
//                         let (src, oper3) = parse_stronger(src, prec, root)?;
//                         oper2.params.insert(0, oper);
//                         oper2.params.push(oper3);
//                         Ok((src, oper2))
//                     },
//                 )
//             })
//         })
//         .or_else(|_| {
//             let (mut src, mut oper) = parse_right(src, prec, root)?;
//             while let Ok((src2, oper2)) = parse_right(src, prec, root) {
//                 oper.params.push(oper2);
//                 src = src2;
//             }
//             let (src, oper2) = parse_stronger(src, prec, root)?;
//             oper.params.push(oper2);
//             Ok((src, oper))
//         })
//         .or_else(|()| {
//             parse_stronger(src, prec, root).and_then(|(src, oper2)| {
//                 let (mut src, mut oper) = parse_left(src, prec, root)?;
//                 oper.params.insert(0, oper2);
//                 while let Ok((src2, mut oper2)) = parse_left(src, prec, root) {
//                     oper2.params.insert(0, oper);
//                     oper = oper2;
//                     src = src2;
//                 }
//                 Ok((src, oper))
//             })
//         })
// }
// fn parse_left<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
//     println!("parse_left: {}", src);
//     parse_op(src, prec.ops(Fixity::Postfix), root).or_else(|_| {
//         parse_op(src, prec.ops(Fixity::Infix(Associativity::Left)), root).and_then(
//             |(src, mut oper)| {
//                 let (src, oper2) = parse_stronger(src, prec, root)?;
//                 oper.params.push(oper2);
//                 Ok((src, oper))
//             },
//         )
//     })
// }
// fn parse_right<'a>(src: &'a str, prec: &Precedence, root: &Precedence) -> Res<'a, Oper> {
//     println!("parse_right: {}", src);
//     parse_op(src, prec.ops(Fixity::Prefix), root).or_else(|_| {
//         parse_stronger(src, prec, root).and_then(|(src, oper)| {
//             let (src, mut oper2) =
//                 parse_op(src, prec.ops(Fixity::Infix(Associativity::Right)), root)?;
//             oper2.params.insert(0, oper);
//             Ok((src, oper2))
//         })
//     })
// }
// fn parse_op<'a>(mut src: &'a str, ops: &[Operator], root: &Precedence) -> Res<'a, Oper> {
//     if src.is_empty() {
//         return Err(());
//     }
//     let orig_src = src;
//     'ops: for op in ops {
//         let mut params = Vec::with_capacity(op.names.len() - 1);
//         for (i, n) in op.names.iter().enumerate() {
//             if &src[0..n.len()] == &*n {
//                 src = &src[n.len()..];
//                 if i < op.names.len() - 1 {
//                     match parse_expr(src, root) {
//                         Ok((src2, expr)) => {
//                             params.push(expr);
//                             src = src2;
//                         }
//                         Err(()) => {
//                             src = orig_src;
//                             continue 'ops;
//                         }
//                     }
//                 }
//             } else {
//                 src = orig_src;
//                 continue 'ops;
//             }
//         }
//         return Ok((
//             src,
//             Oper {
//                 op: op.clone(),
//                 params,
//             },
//         ));
//     }
//     return Err(());
// }
