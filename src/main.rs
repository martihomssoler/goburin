#![feature(let_chains)]

use std::error::Error;

mod parser;
mod scanner;

fn main() -> Result<(), Box<dyn Error>> {
    scanner::run_file("./example.gobo")?;
    Ok(())
}
