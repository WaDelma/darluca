use std::rc::Rc;

use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash)]
enum Associativity {
    Left,
    Right,
    Non,
}

#[derive(PartialEq, Eq, Hash)]
enum Fixity {
    Prefix,
    Infix(Associativity),
    Postfix,
    Closed,
}

struct Operator {
    names: Vec<String>,
    fixity: Fixity,
}

struct Precedence {
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

struct PrecedenceGraph(Vec<Precedence>);
