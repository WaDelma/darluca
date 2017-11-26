use symtern::Pool;
use symtern::Sym;

pub type Interner = Pool<str, usize>;
pub type Symbol = Sym<usize>;