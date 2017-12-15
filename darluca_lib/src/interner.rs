use string_interner::DefaultStringInterner;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub struct Interner {
    pool: DefaultStringInterner,
    gens: HashMap<usize, usize>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            pool: DefaultStringInterner::default(),
            gens: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        Symbol(self.pool.get_or_intern(s))
    }

    pub fn resolve(&self, s: Symbol) -> Option<&str> {
        self.pool.resolve(s.0)
    }

    pub fn generate(&mut self, base: &str) -> Symbol {
        let base_s = self.pool.get_or_intern(format!("{}¤", base));
        let cur = self.gens.entry(base_s).or_insert(0);
        let result = self.pool.get_or_intern(format!("{}¤{}", base, cur));
        *cur += 1;
        Symbol(result)
    }
}

#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct Symbol(usize);

impl Symbol {
    pub fn id(&self) -> usize {
        self.0
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("Sym(")?;
        self.0.fmt(fmt)?;
        fmt.write_str(")")
    }
}
