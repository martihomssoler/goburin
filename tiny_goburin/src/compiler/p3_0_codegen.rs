use super::*;
use std::{
    fmt::Write,
    fs::File,
    path::Path,
    process::{Command, Stdio},
};

impl Ir {
    pub fn p3_0_codegen(self, file_path: &Path, target: &mut dyn Target) -> Result<(), String> {
        let code = target.codegen(self)?;
        code_fasm_compile(file_path, code)?;
        Ok(())
    }
}

fn code_fasm_compile(file_path: &Path, code: String) -> Result<(), String> {
    let asm_file = file_path.with_extension("asm");
    let obj_file = asm_file.clone().with_extension("o");
    let exec_file = asm_file.clone().with_extension("");
    let mut output = File::create(asm_file.clone()).map_err(|err| err.to_string())?;

    std::io::Write::write_all(&mut output, code.as_bytes()).map_err(|err| err.to_string())?;

    let file_dir = asm_file.parent().unwrap().to_str().unwrap().to_string();

    // fasm file.asm
    let output_fasm = Command::new("fasm")
        .arg(asm_file)
        .current_dir(file_dir.clone())
        .output()
        .map_err(|err| err.to_string())?;
    if !output_fasm.status.success() {
        let fasm_error = std::str::from_utf8(&output_fasm.stderr).expect("Output should be UTF-8");
        return Err(fasm_error.to_string());
    }

    // ld file.o -lc
    let output_ld = Command::new("ld")
        .arg("-o")
        .arg(exec_file)
        .arg(obj_file)
        .arg("-lc")
        .current_dir(file_dir)
        .output()
        .map_err(|err| err.to_string())?;
    if !output_ld.status.success() {
        let fasm_error = std::str::from_utf8(&output_ld.stderr).expect("Output should be UTF-8");
        return Err(fasm_error.to_string());
    }

    Ok(())
}
