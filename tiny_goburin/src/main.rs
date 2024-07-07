use std::{env, path::PathBuf};

fn main() -> Result<(), String> {
    let mut args = env::args();
    let count = args.len();
    match count {
        2 => compile_file(&args.nth(1).unwrap().into())?,
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }

    Ok(())
}

pub fn compile_file(file_path: &PathBuf) -> Result<(), String> {
    let source = std::fs::read_to_string(file_path).unwrap();
    let result_frontend = frontend::frontend_pass(&source)?;
    // let result_middleend = middleend::middleend_pass(result_frontend);
    // let result_backend = backend::backend_pass(result_middleend);
    Ok(())
}

#[path = "backend/backend.rs"]
mod backend;
#[path = "frontend/frontend.rs"]
mod frontend;
#[path = "middleend/middleend.rs"]
mod middleend;

#[cfg(test)]
pub mod tests {
    use std::{
        path::PathBuf,
        process::{Command, ExitStatus},
    };

    use crate::compile_file;

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
                    println!(
                        "ERROR --> Skipping '{:}.gobo' : No '.output' file found for it",
                        filename
                    );
                    return Err(std::io::Error::last_os_error());
                };

                println!("[[ Compiling '{:}.gobo' ]]", filename);
                let res = compile_file(&test_path);
                assert_eq!(Ok(()), res);

                // println!("--> [ Executing '{:}.gobo' ]", filename);
                // let actual_output = Command::new(format!("./{}", filename))
                //     .current_dir(tests_dir.clone())
                //     .output()?;

                // assert_eq!(
                //     actual_output.status,
                //     <ExitStatus as std::default::Default>::default()
                // );

                // println!(
                //     "--> [ Comparing execution of '{:}' with '.output' file ]",
                //     filename
                // );
                // let actual_output_content =
                //     std::str::from_utf8(&actual_output.stdout).expect("Output should be UTF-8");
                // assert_eq!(actual_output_content, expected_output_content);

                // println!("--> [ Removing files '{:}' ]", filename);
                // let output = Command::new("rm")
                //     .arg(filename)
                //     .arg(format!("{filename}.s"))
                //     .arg(format!("{filename}.ssa"))
                //     .current_dir(tests_dir.clone())
                //     .status()
                //     .unwrap()
                //     .code();
                // assert_eq!(output, Some(0));

                println!();
            }
        }

        Ok(())
    }
}
