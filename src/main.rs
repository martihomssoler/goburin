#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(variant_count)]
#![allow(dead_code, unused)]

pub mod backend;
pub mod frontend;
pub mod middleend;
pub mod shared;

use backend::*;
use frontend::*;
use middleend::*;
use shared::*;

use std::{
    env,
    error::Error,
    fmt::Display,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
};

type CompilerResult<T> = Result<T, CompilerError>;

fn main() -> CompilerResult<()> {
    let mut args = env::args();
    let count = args.len();
    match count {
        // 1 => interpreter::run_prompt()?,
        2 => compile_file(&args.nth(1).unwrap().into())?,
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }
    Ok(())
}

pub fn compile_file(file_path: &PathBuf) -> CompilerResult<()> {
    let file_content = std::fs::read_to_string(file_path)?;
    // generate AST
    let ast = frontend::generate_ast(&file_content)?;
    // generate IR
    let ir = middleend::generate_ir(ast)?;
    // generate ASM
    let qbe = backend::generate_qbe(ir)?;

    compile_qbe(file_path, qbe)?;

    Ok(())
}

fn compile_qbe(file_path: &Path, qbe: String) -> CompilerResult<()> {
    let output_path = file_path.with_extension("ssa");
    let mut output = File::create(output_path.clone())?;

    output.write_all(qbe.as_bytes())?;

    let file_dir = output_path.parent().unwrap().to_str().unwrap();
    let basename = output_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
        .unwrap()
        .0
        .to_string();
    let mut qbe_file = basename.clone();
    qbe_file.push_str(".ssa");
    let mut asm_file = basename.clone();
    asm_file.push_str(".s");

    // qbe -o file.s file.ssa
    Command::new("qbe")
        .arg("-o")
        .arg(asm_file.clone())
        .arg(qbe_file)
        .current_dir(file_dir)
        .spawn()?
        .wait()?;

    // cc -o file file.s
    Command::new("cc")
        .arg("-o")
        .arg(basename)
        .arg(asm_file)
        .current_dir(file_dir)
        .spawn()?
        .wait()?;

    Ok(())
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CompilerError {
    LexerError,
    ParserError,
    SemanticError,
    IRError,
    QBEError,
    Generic(String),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<io::Error> for CompilerError {
    fn from(value: io::Error) -> Self {
        CompilerError::Generic(format!("{value}"))
    }
}

#[cfg(test)]
pub mod tests {
    use std::{
        fs::File,
        io::{self, Write},
        path::PathBuf,
        process::{Command, ExitStatus},
    };

    use crate::compile_file;

    #[test]
    fn goburin_compiler() -> io::Result<()> {
        let mut tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        tests_dir.push("tests/");
        for entry in tests_dir.read_dir()? {
            let test = entry?;
            let test_path = test.path();
            if !test_path.is_dir()
                && let Some(ext) = test_path.extension()
                && ext.eq("gobo")
            {
                let filename = test_path.file_stem().unwrap().to_str().unwrap();

                let output_file = test_path.clone().with_extension("output");
                let Ok(expected_output_content) = std::fs::read_to_string(output_file) else {
                    println!(
                        "ERROR --> Skipping {:?} : No '.output' file found for it",
                        filename
                    );
                    return Err(std::io::Error::last_os_error());
                };

                println!("[[ Compiling '{:}' ]]", filename);
                let res = compile_file(&test_path);
                assert_eq!(Ok(()), res);

                println!("--> [ Executing '{:}' ]", filename);
                let actual_output = Command::new(format!("./{}", filename))
                    // .arg(format!(" > {}.test_output", filename))
                    .current_dir(tests_dir.clone())
                    .output()?;
                // .spawn()?
                // .wait()?;
                assert_eq!(
                    actual_output.status,
                    <ExitStatus as std::default::Default>::default()
                );

                println!(
                    "--> [ Comparing execution of '{:}' with '.output' file ]",
                    filename
                );
                let actual_output_content =
                    std::str::from_utf8(&actual_output.stdout).expect("Output should be UTF-8");
                assert_eq!(actual_output_content, expected_output_content);

                println!("--> [ Removing files '{:}' ]", filename);
                let output = Command::new("rm")
                    .arg(filename)
                    .arg(format!("{filename}.s"))
                    .arg(format!("{filename}.ssa"))
                    .current_dir(tests_dir.clone())
                    .status()
                    .unwrap()
                    .code();

                println!();
            }
        }

        Ok(())
    }
}
