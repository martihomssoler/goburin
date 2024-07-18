/// For now we use FASM as our assembly target language
use crate::{
    frontend::{self, Ast, Declaration, Expression, Identifier, Statement},
    middleend::{Ir, IrState},
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
    const SCRATCH_REGS: [&'static str; 7] = ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"];
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

        codegen_definition(&mut ir, &mut code);

        // data section
        writeln!(code, r#"section '.data' writeable"#);
        writeln!(code);
        writeln!(code, r#"fmt_int: db "%d", 0"#);
        writeln!(code, r#"fmt_char: db "%c", 0"#);
        writeln!(code, r#"fmt_string: db "%s", 0"#);

        for (str_content, str_label) in ir.state.strs {
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

fn codegen_definition(ir: &mut Ir, code: &mut String) {
    for def in &mut ir.program {
        match &mut def.val {
            frontend::DefinitionValue::FunctionBody(stmts) => {
                for stmt in stmts {
                    codegen_stmt(&mut ir.state, stmt, code);
                }
            }
        }
    }
}

fn codegen_stmt(state: &mut IrState, stmt: &mut Statement, code: &mut String) {
    match stmt {
        Statement::Declaration(d) => codegen_decl(state, d, code),
        Statement::Expression(e) => codegen_expr(state, e, code),
        Statement::Print(p) => codegen_print(state, p, code),
        Statement::Return(r) => codegen_return(state, r, code),
    }
}

fn codegen_decl(state: &mut IrState, decl: &mut Declaration, code: &mut String) {
    match &mut decl.val {
        frontend::DeclarationValue::Uninitialized => (),
        frontend::DeclarationValue::Expression(expr) => codegen_expr(state, expr, code),
        frontend::DeclarationValue::FunctionBody(stmts) => {
            for stmt in stmts {
                codegen_stmt(state, stmt, code);
            }
        }
    }
}

fn codegen_expr(state: &mut IrState, expr: &mut Expression, code: &mut String) {
    println!("{expr}");
    match &mut expr.kind {
        frontend::ExpressionKind::LiteralInteger(i) => {
            state.var_insert(&expr.name, &format!("{i}"));
        }
        frontend::ExpressionKind::LiteralString(s) => {
            let str_label = format!("str_{}", state.strs.len());
            state.str_insert(s, &str_label);
        }
        frontend::ExpressionKind::Identifier(_) => todo!(),
    }
}

fn codegen_print(state: &mut IrState, exprs: &[Expression], code: &mut String) {
    for expr in exprs {
        match &expr.kind {
            frontend::ExpressionKind::LiteralInteger(i) => {
                state.var_insert(&expr.name, &format!("{i}"));
            }
            frontend::ExpressionKind::LiteralString(s) => {
                let str_label = format!("str_{}", state.strs.len());
                state.str_insert(s, &str_label);

                writeln!(code, r#"mov rdi, fmt_string"#);
                writeln!(code, r#"mov rsi, {}"#, str_label);
                writeln!(code, r#"call printf"#);
            }
            frontend::ExpressionKind::Identifier(id) => {
                let Some(s) = state.var_get(id) else {
                    println!("[ERROR]");
                    println!("VARS:\n{:?}", state.vars);
                    println!("STRS:\n{:?}", state.strs);
                    panic!("Symbol {id:?} should be resolved by now");
                };

                writeln!(code, r#"mov rdi, fmt_int"#);
                writeln!(code, r#"mov rsi, {s}"#);
                writeln!(code, r#"call printf"#);
            }
        }
    }
}

fn codegen_return(state: &mut IrState, r: &mut Expression, code: &mut String) {
    codegen_expr(state, r, code);

    writeln!(code, r#"mov rdi, 0"#);
    writeln!(code, r#"call exit"#);
    writeln!(code);
}
// --- ---
