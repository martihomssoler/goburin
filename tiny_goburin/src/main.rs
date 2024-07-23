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
    let mut args = env::args();
    let count = args.len();
    match count {
        2 => {
            c_compile_file(args.nth(1).unwrap().into())?;
        }
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }

    Ok(())
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
                let Ok(expected_output_content) = std::fs::read_to_string(output_file) else {
                    println!("ERROR --> Skipping '{filename}.gobo' : No '.output' file found for it",);
                    return Err(std::io::Error::last_os_error());
                };

                println!("[[ COMPILING ]] => '{:}.gobo'", filename);
                if let Err(err) = c_compile_file(test_path.clone()) {
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
                assert_eq!(command_output.status, <ExitStatus as std::default::Default>::default());

                println!("--> [ Comparing execution with '{filename}.output' ]",);
                let actual_output_content =
                    std::str::from_utf8(&command_output.stdout).expect("Output should be UTF-8");
                assert_eq!(actual_output_content, expected_output_content);

                println!("--> [ Removing files '{:}' ]", filename);
                let output = Command::new("rm")
                    .arg(filename)
                    .arg(format!("{filename}.asm"))
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
