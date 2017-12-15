//#![feature(trace_macros)]
//#![feature(log_syntax)]

extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate itertools;
#[macro_use]
extern crate nom;
extern crate string_interner;
extern crate ena;

pub mod lexer;
pub mod parser;
pub mod interpreter;
pub mod typechecker;
pub mod interner;
