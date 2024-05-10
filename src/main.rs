#![feature(let_chains)]

use std::{env, error::Error};

pub mod compiler;
pub mod environment;
pub mod interpreter;
pub mod parser;
pub mod tokenizer;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let count = args.len();
    match count {
        1 => interpreter::run_prompt()?,
        2 => interpreter::run_file(&args.nth(1).unwrap())?,
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }
    Ok(())
}
