//#![feature(trace_macros)]
//#![feature(log_syntax)]

#[macro_use]
extern crate nom;
extern crate symtern;
extern crate itertools;
extern crate failure;
#[macro_use]
extern crate failure_derive;

pub mod lexer;
pub mod parser;
pub mod interpreter;
mod interner;