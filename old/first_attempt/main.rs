#![feature(let_chains)]
#![feature(inherent_associated_types)]
#![allow(incomplete_features)]
#![allow(dead_code)]
#![allow(unused_imports)]

use std::env;
use std::error::Error;

use scanner::parser::{Expr, LiteralValue};
use scanner::{Token, TokenKind};

mod scanner;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let count = args.len();
    match count {
        1 => scanner::run_prompt()?,
        2 => scanner::run_file(&args.nth(1).unwrap())?,
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }
    Ok(())
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(thiserror::Error, Debug)]
pub struct ParserError {
    message: String,
    line: usize,
}

impl ParserError {
    pub fn wrong_token(token: &Token, message: String) -> Self {
        Self {
            message,
            line: token.line,
        }
    }
    pub fn interpreter_error(message: String) -> Self {
        // TODO: add line number
        Self { message, line: 0 }
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}
