/// For now we use FASM as our assembly target language
use crate::{
    frontend::{self, Ast, Identifier, Stmt},
    middleend::Ir,
};
use std::{
    fmt::Write,
    fs::File,
    path::Path,
    process::{Command, Stdio},
};

pub fn backend_pass(file_path: &Path, ir: Ir) -> Result<(), String> {
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

// --- TARGET ---
pub trait Target {
    fn codegen(self, ir: Ir) -> Result<String, String>;

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
    fn codegen(self, mut ir: Ir) -> Result<String, String> {
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
        writeln!(code, r#"extrn exit"#);
        writeln!(code);
        // code starts
        writeln!(code, r#"_start:"#);

        for stmt in ir.instructions() {
            match stmt {
                Stmt::Decl(decl) => {
                    let frontend::Decl { id, val, .. } = decl;

                    match val.kind {
                        frontend::Value::Constant(c) => match c {
                            frontend::Constant::Int(i) => {
                                let Identifier(id) = id.kind;
                                ir.var_insert(&id, &format!("{i}"));
                            }
                            frontend::Constant::Bool(b) => todo!(),
                            frontend::Constant::Char(c) => {
                                format!("{c:?}");
                            }
                            frontend::Constant::String(s) => {
                                let str_label = format!("str_{}", ir.strs.len());
                                ir.str_insert(&s, &str_label);
                            }
                        },
                        frontend::Value::Identifier(id) => todo!(),
                    }
                }
                Stmt::Print(print) => {
                    let frontend::Print { vals } = print;
                    for val in vals {
                        match val.kind {
                            frontend::Value::Constant(c) => match c {
                                frontend::Constant::Int(i) => todo!(),
                                frontend::Constant::Bool(b) => todo!(),
                                frontend::Constant::Char(c) => todo!(),
                                frontend::Constant::String(s) => {
                                    let str_label = format!("str_{}", ir.strs.len());
                                    ir.str_insert(&s, &str_label);

                                    writeln!(code, r#"mov rdi, fmt_string"#);
                                    writeln!(code, r#"mov rsi, {}"#, str_label);
                                    writeln!(code, r#"call printf"#);
                                }
                            },
                            frontend::Value::Identifier(id) => {
                                let Some(s) = ir.var_get(&id.0) else {
                                    panic!("Symbol should be resolved by now");
                                };

                                writeln!(code, r#"mov rdi, fmt_int"#);
                                writeln!(code, r#"mov rsi, {s}"#);
                                writeln!(code, r#"call printf"#);
                            }
                        }
                    }
                }
            }
        }
        writeln!(code, r#"mov rdi, 0"#);
        writeln!(code, r#"call exit"#);
        writeln!(code);

        // data section
        writeln!(code, r#"section '.data' writeable"#);
        writeln!(code);
        writeln!(code, r#"fmt_int: db "%d", 0"#);
        writeln!(code, r#"fmt_char: db "%c", 0"#);
        writeln!(code, r#"fmt_string: db "%s", 0"#);

        for (str_content, str_label) in ir.strs {
            let str_content = str_content.replace('\n', "\", 10, \"");
            writeln!(code, r#"{str_label}: db "{str_content}", 0"#);
        }

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
