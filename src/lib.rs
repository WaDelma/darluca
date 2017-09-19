#![feature(trace_macros)]
#![feature(log_syntax)]

#[macro_use]
extern crate nom;

pub mod lexer;
pub mod parser;
pub mod interpreter;
