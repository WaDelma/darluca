use symtern::{Pool, Sym, Error};
use symtern::prelude::*;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Interner {
    pool: Pool<str, usize>,
    gens: HashMap<Sym<usize>, usize>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            pool: Pool::new(),
            gens: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Result<Symbol, Error> {
        self.pool.intern(s).map(Symbol)
    }

    pub fn resolve(&self, s: Symbol) -> Result<&str, Error> {
        self.pool.resolve(s.0)
    }

    pub fn generate(&mut self, base: &str) -> Result<Symbol, Error> {
        let base_s = self.pool.intern(&format!("{}¤", base))?;
        let cur = self.gens.entry(base_s).or_insert(0);
        let result = self.pool.intern(&format!("{}¤{}", base, cur))?;
        *cur += 1;
        Ok(Symbol(result))
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Symbol(Sym<usize>);
