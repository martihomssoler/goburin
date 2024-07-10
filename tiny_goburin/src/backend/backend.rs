/// For now we use FASM as our assembly target language
use crate::frontend::Ast;
use std::{fmt::Write, fs::File, path::Path, process::Command};

pub fn backend_pass(file_path: &Path, ir: Ast) -> Result<(), String> {
    let be = X86_64::default();
    let code = be.codegen(ir)?;
    code_fasm_compile(file_path, code)?;
    Ok(())
}

fn code_fasm_compile(file_path: &Path, code: String) -> Result<(), String> {
    let asm_file = file_path.with_extension("asm");
    let obj_file = asm_file.clone().with_extension("o");
    let exec_file = asm_file.clone().with_extension("");
    let mut output = File::create(asm_file.clone()).map_err(|err| err.to_string())?;

    std::io::Write::write_all(&mut output, code.as_bytes()).map_err(|err| err.to_string())?;

    let file_dir = asm_file.parent().unwrap().to_str().unwrap().to_string();

    // fasm file.asm
    Command::new("fasm")
        .arg(asm_file)
        .current_dir(file_dir.clone())
        .spawn()
        .map_err(|err| err.to_string())?
        .wait()
        .map_err(|err| err.to_string())?;

    // ld file.o -dynamic-linker -lc
    Command::new("ld")
        .arg("-o")
        .arg(exec_file)
        .arg(obj_file)
        .arg("-lc")
        .current_dir(file_dir)
        .spawn()
        .map_err(|err| err.to_string())?
        .wait()
        .map_err(|err| err.to_string())?;

    Ok(())
}

// --- TARGET ---
pub trait Target {
    fn codegen(self, ir: Ast) -> Result<String, String>;

    fn scratch_alloc(&mut self) -> u32;
    fn scratch_free(&mut self, r: u32);
    fn scratch_name(&self, r: u32) -> String;

    fn label_create(&mut self) -> u32;
    fn label_name(&self, l: u32) -> String;
}

#[derive(Default)]
pub struct X86_64 {
    pub regs: [bool; X86_64::SCRATCH_REGS.len()],
}

impl X86_64 {
    const SCRATCH_REGS: [&'static str; 7] =
        ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"];
}

impl Target for X86_64 {
    fn codegen(self, ir: Ast) -> Result<String, String> {
        let mut errors: Vec<String> = Vec::new();
        let mut code = String::new();

        // --- PRELUDE ---
        // file format
        writeln!(code, r#"format ELF64"#);
        writeln!(code);

        // code section
        writeln!(code, r#"section '.text' executable"#);
        writeln!(code);
        // make start visible
        writeln!(code, r#"public _start"#);
        writeln!(code);
        // declare external functions
        writeln!(code, r#"extrn printf"#);
        writeln!(code, r#"extrn _exit"#);
        writeln!(code);
        // code starts
        writeln!(code, r#"_start:"#);
        writeln!(code, r#"mov rdi, msg"#);
        writeln!(code, r#"call printf"#);
        writeln!(code, r#"mov rdi, 0"#);
        writeln!(code, r#"call _exit"#);
        writeln!(code);

        // data section
        writeln!(code, r#"section '.data' writeable"#);
        writeln!(code);
        writeln!(code, r#"msg: db "Hello, World", 10, 0"#);

        if !errors.is_empty() {
            return Err(errors.join("\n"));
        }

        Ok(code)
    }

    fn scratch_alloc(&mut self) -> u32 {
        for i in 0..self.regs.len() {
            if !self.regs[i] {
                self.regs[i] = true;
                return i as u32;
            }
        }
        u32::MAX
    }

    fn scratch_free(&mut self, r: u32) {
        if (r as usize) < self.regs.len() {
            self.regs[r as usize] = false;
        }
    }

    fn scratch_name(&self, r: u32) -> String {
        if (r as usize) < self.regs.len() {
            return X86_64::SCRATCH_REGS[r as usize].to_string();
        }
        println!("[PANIC ERROR] Register_Overflow");
        panic!()
    }

    fn label_create(&mut self) -> u32 {
        todo!()
    }

    fn label_name(&self, _l: u32) -> String {
        todo!()
    }
}
// --- ---
