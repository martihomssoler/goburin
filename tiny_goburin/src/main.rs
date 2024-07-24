#![allow(
    dead_code,
    unused,
    clippy::if_same_then_else,
    clippy::needless_range_loop,
    clippy::needless_bool
)]

#[path = "compiler/compiler.rs"]
mod compiler;

use compiler::c_compile_file;
use std::{env, path::PathBuf};

fn main() -> Result<(), String> {
    let mut args: Vec<_> = env::args().collect();
    let count = args.len();
    match count {
        c if c < 2 || args[1] == FLAG_HELP_ABBR || args[1] == FLAG_HELP => {
            println!("Goburin's Compiler");
            println!();
            usage();
        }
        4 if ACCEPTED_COMMANDS.contains(&args[1].as_str()) => {
            let stage = match args[1].as_str() {
                // I know u32 is overkill and that u8 would pbbly be enough but YOLO
                // I've used 3 bytes more than necessary, sue me
                FLAG_ALL | FLAG_ALL_ABBR => u32::MAX,
                FLAG_TOKEN | FLAG_TOKEN_ABBR => 0,
                FLAG_PARSE | FLAG_PARSE_ABBR => 1,
                FLAG_CODEGEN | FLAG_CODEGEN_ABBR => 2,
                f => panic!("Wrong flag {f:?}."),
            };
            let input = args[2].clone().into();
            let output = args[3].clone().into();
            c_compile_file(stage, input, output)?;
        }
        _ => {
            println!("[ ERROR ]: Wrong number of arguments provided.");
            println!();
            usage();
        }
    }

    Ok(())
}

const FLAG_ALL_ABBR: &str = "-a";
const FLAG_CODEGEN_ABBR: &str = "-c";
const FLAG_PARSE_ABBR: &str = "-p";
const FLAG_TOKEN_ABBR: &str = "-t";
const FLAG_HELP_ABBR: &str = "-h";
const FLAG_ALL: &str = "--all";
const FLAG_CODEGEN: &str = "--codegen";
const FLAG_PARSE: &str = "--parse";
const FLAG_TOKEN: &str = "--token";
const FLAG_HELP: &str = "--help";

const ACCEPTED_COMMANDS: [&str; 8] = [
    FLAG_ALL_ABBR,
    FLAG_CODEGEN_ABBR,
    FLAG_PARSE_ABBR,
    FLAG_TOKEN_ABBR,
    FLAG_ALL,
    FLAG_CODEGEN,
    FLAG_PARSE,
    FLAG_TOKEN,
];

fn usage() {
    println!("Usage: gbc [COMMAND] <INPUT> <OUTPUT>");
    println!();
    println!("Commands:");
    println!("  {FLAG_ALL_ABBR}, {FLAG_ALL}\t\tRuns the entire compiler on <INPUT> and saves the emitted code into <OUTPUT>.");
    println!("  {FLAG_CODEGEN_ABBR}, {FLAG_CODEGEN}\t\tRuns the lexer, parser and asm generation on <INPUT> and saves the assembly code into <OUTPUT>.");
    println!("  {FLAG_PARSE_ABBR}, {FLAG_PARSE}\t\tRuns the lexer and parser on <INPUT> and saves the AST representation into <OUTPUT>.");
    println!("  {FLAG_TOKEN_ABBR}, {FLAG_TOKEN}\t\tRuns the lexer on <INPUT> and saves the token representation into <OUTPUT>.");
    println!("  {FLAG_HELP_ABBR}, {FLAG_HELP}\t\tPrints this help.");
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use core::panic;
    use std::{
        io::{Read, Stdout, Write},
        path::PathBuf,
        process::{Command, ExitStatus, Stdio},
    };

    /// Output files are organized in the following way:
    /// The first line of the file must be parsed as an integer and is the expected StatusCode of the program
    /// The rest of the file is the StdOut of the program
    #[test]
    fn goburin_compiler() -> std::io::Result<()> {
        let mut tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        tests_dir.push("tests/");
        for entry in tests_dir.read_dir()? {
            let test = entry?;
            let test_path = test.path();
            if !test_path.is_dir() && test_path.extension().is_some_and(|ext| ext.eq("gobo")) {
                let filename = test_path.file_stem().unwrap().to_str().unwrap();

                let output_file = test_path.clone().with_extension("output");
                let Ok(output_file_content) = std::fs::read_to_string(output_file) else {
                    println!("ERROR --> Skipping '{filename}.gobo' : No '.output' file found for it",);
                    return Err(std::io::Error::last_os_error());
                };
                let expected_status_code = output_file_content
                    .lines()
                    .next()
                    .expect("The output file should have as first line the status code of the program.")
                    .parse::<i32>()
                    .map_err(|err| format!("{err} : the status code of the program is expected to be an integer."))
                    .unwrap();
                let expected_output_content: String = output_file_content.lines().skip(1).collect();

                println!("[[ COMPILING ]] => '{:}.gobo'", filename);
                if let Err(err) = c_compile_file(u32::MAX, test_path.clone(), filename.to_owned().into()) {
                    println!("{err}");
                    panic!();
                }

                println!("--> [ Executing '{:}' ]", filename);
                let mut command_output = Command::new(format!("./{filename}"))
                    .current_dir(tests_dir.clone())
                    .output()?;
                if !command_output.status.success() {
                    let command_error = std::str::from_utf8(&command_output.stderr)
                        .expect("Output should be UTF-8")
                        .to_string();
                    println!("--> [ EXECUTING ERROR ] {command_error:?}");
                }
                assert_eq!(command_output.status.code(), Some(expected_status_code));

                println!("--> [ Comparing execution with '{filename}.output' ]",);
                let actual_output_content =
                    std::str::from_utf8(&command_output.stdout).expect("Output should be UTF-8");
                assert_eq!(actual_output_content, expected_output_content);

                println!("--> [ Removing files '{:}' ]", filename);
                let output = Command::new("rm")
                    .arg(filename)
                    // .arg(format!("{filename}.asm"))
                    .arg(format!("{filename}.o"))
                    .current_dir(tests_dir.clone())
                    .status()
                    .unwrap()
                    .code();
                assert_eq!(output, Some(0));

                println!();
            }
        }

        Ok(())
    }
}
