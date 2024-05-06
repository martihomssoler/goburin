#![feature(let_chains)]

use std::env;
use std::error::Error;

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
