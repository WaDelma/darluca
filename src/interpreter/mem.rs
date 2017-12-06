use std::collections::HashMap;
use std::iter::FromIterator;
use std::mem::replace;

use parser::ast::Identifier;

pub struct Memory<D> {
    scopes: Vec<HashMap<Identifier, D>>,
    #[cfg(test)]
    pub used_scopes: Vec<HashMap<Identifier, D>>,
}

impl<D> Memory<D> {
    pub fn new() -> Self {
        Memory {
            scopes: vec![],
            #[cfg(test)]
            used_scopes: vec![],
        }
    }

    pub fn get(&self, i: &Identifier) -> Option<&D> {
        for scope in self.scopes.iter().rev() {
            if let r @ Some(_) = scope.get(i) {
                return r;
            }
        }
        None
    }

    pub fn get_mut(&mut self, i: &Identifier) -> Option<&mut D> {
        for scope in self.scopes.iter_mut().rev() {
            if let r @ Some(_) = scope.get_mut(i) {
                return r;
            }
        }
        None
    }

    pub fn create(&mut self, i: Identifier, v: D) -> Option<D> {
        self.scopes.last_mut().and_then(|l| l.insert(i, v))
    }

    pub fn scope<F, T>(&mut self, f: F) -> T
        where F: FnOnce(&mut Self) -> T
    {
        self.scopes.push(HashMap::new());
        let result = f(self);
        let _used_scope = self.scopes.pop();
        #[cfg(test)]
        self.used_scopes.push(
            _used_scope.expect("We pushed scope so this should be always valid.")
        );
        result
    }
}

impl<D> FromIterator<(Identifier, D)> for Memory<D> {
    fn from_iter<I>(iter: I) -> Memory<D>
        where I: IntoIterator<Item=(Identifier, D)>
    {
        let mut mem = Memory::new();
        mem.scopes.push(iter.into_iter().collect());
        mem
    }
}