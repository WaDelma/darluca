//#![feature(trace_macros)]
//#![feature(log_syntax)]

#[macro_use]
extern crate nom;
extern crate symtern;

pub mod lexer;
pub mod parser;
pub mod interpreter;
mod interner;