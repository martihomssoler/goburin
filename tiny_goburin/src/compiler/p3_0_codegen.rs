//! For now we use FASM as our assembly target language
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

// --- TARGET ---
pub trait Target {
    fn codegen(&mut self, ir: Ir) -> Result<String, String>;

    fn scratch_alloc(&mut self) -> u32;
    fn scratch_free(&mut self, r: u32);
    fn scratch_name(&self, r: u32) -> String;

    fn label_create(&mut self) -> u32;
    fn label_name(&self, l: u32) -> String;
}

#[derive(Default)]
pub struct X86_64 {
    pub regs: [bool; X86_64::SCRATCH_REGS.len()],
    pub labels: u32,
}

impl X86_64 {
    /// | Register | Conventional use                |
    /// | -------- | ------------------------------- |
    /// | %rax     | Return value, caller-saved      |
    /// | %rdi     | 1st argument, caller-saved      |
    /// | %rsi     | 2nd argument, caller-saved      |
    /// | %rdx     | 3rd argument, caller-saved      |
    /// | %rcx     | 4th argument, caller-saved      |
    /// | %r8      | 5th argument, caller-saved      |
    /// | %r9      | 6th argument, caller-saved      |
    /// | %r10     | Scratch/temporary, caller-saved |
    /// | %r11     | Scratch/temporary, caller-saved |
    /// | %rsp     | Stack pointer, callee-saved     |
    /// | %rbx     | Local variable, callee-saved    |
    /// | %rbp     | Local variable, callee-saved    |
    /// | %r12     | Local variable, callee-saved    |
    /// | %r13     | Local variable, callee-saved    |
    /// | %r14     | Local variable, callee-saved    |
    /// | %r15     | Local variable, callee-saved    |
    ///
    /// [link](https://web.stanford.edu/class/archive/cs/cs107/cs107.1174/guide_x86-64.html)
    const SCRATCH_REGS: [&'static str; 10] = ["rcx", "rdx", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"];
}

impl Target for X86_64 {
    fn codegen(&mut self, mut ir: Ir) -> Result<String, String> {
        let mut errors: Vec<String> = Vec::new();
        let mut code = String::new();

        // --- PRELUDE ---
        // file format
        writeln!(code, r#"format ELF64"#);
        writeln!(code);

        // code section
        writeln!(code, r#"section '.text' executable"#);
        // make start visible
        writeln!(code, r#"public _start"#);
        // declare external functions
        writeln!(code, r#"extrn printf"#);
        writeln!(code, r#"extrn exit"#);
        writeln!(code);
        // code starts
        writeln!(code, r#"_start:"#);

        self.codegen_definition(&mut ir, &mut code);

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
        println!("[PANIC ERROR] Register_Overflow: {r}");
        println!("REGS: {:?}", self.regs);
        panic!()
    }

    fn label_create(&mut self) -> u32 {
        self.labels += 1;
        self.labels
    }

    fn label_name(&self, l: u32) -> String {
        format!(".L{l}")
    }
}

impl X86_64 {
    fn codegen_definition(&mut self, ir: &mut Ir, code: &mut String) {
        for def in &mut ir.program {
            match &mut def.val {
                DefinitionValue::FunctionBody(stmts) => {
                    for stmt in stmts {
                        self.codegen_stmt(&mut ir.state, stmt, code);
                    }
                }
            }
        }
    }

    fn codegen_stmt(&mut self, state: &mut IrState, stmt: &mut Statement, code: &mut String) {
        match stmt {
            Statement::Declaration(d) => self.codegen_decl(state, d, code),
            Statement::Expression(e) => {
                self.codegen_expr(state, e, code);
            }
            Statement::Print(p) => self.codegen_print(state, p, code),
            Statement::Return(r) => self.codegen_return(state, r, code),
            Statement::Block(_) => todo!(),
            Statement::Conditional(_) => todo!(),
            Statement::Loop(l) => self.codegen_loop(state, l, code),
            Statement::Assignment(a) => self.codegen_assign(state, a, code),
        }
    }

    fn codegen_decl(&mut self, state: &mut IrState, decl: &mut Declaration, code: &mut String) {
        match &mut decl.val {
            DeclarationValue::Uninitialized => {
                state.var_insert(&decl.name, "0");
            }
            // Declaration w/ form => name = value;
            DeclarationValue::Expression(expr) => {
                // generate the code for the "value"
                self.codegen_expr(state, expr, code);
                // the result is in the nodes register
                let r = expr.node.reg;
                state.var_insert(&decl.name, &self.scratch_name(r));
            }
            DeclarationValue::FunctionBody(stmts) => {
                for stmt in stmts {
                    self.codegen_stmt(state, stmt, code);
                }
            }
        }
    }

    fn codegen_expr(&mut self, state: &mut IrState, expr: &mut Expression, code: &mut String) {
        match &mut expr.kind {
            ExpressionKind::LiteralInteger(i) => {
                expr.node.reg = self.scratch_alloc();
                writeln!(code, r#"mov {}, {i}"#, self.scratch_name(expr.node.reg));
            }
            ExpressionKind::LiteralString(s) => {
                let str_label = format!("str_{}", state.strs.len());
                state.str_insert(s, &str_label);
                expr.node.reg = self.scratch_alloc();
                writeln!(code, r#"mov {}, {str_label}"#, self.scratch_name(expr.node.reg));
            }
            ExpressionKind::Identifier(id) => {
                if let Some(res) = state.var_get(id) {
                    if let Some(idx) = Self::SCRATCH_REGS.iter().position(|reg| res.eq(reg)) {
                        expr.node.reg = idx as u32;
                        return;
                    }
                }
                expr.node.reg = self.scratch_alloc();
                let symbol = state.var_get(id).unwrap();
                writeln!(code, r#"mov {}, {symbol}"#, self.scratch_name(expr.node.reg));
            }
            ExpressionKind::LiteralBoolean(_) => todo!(),
            ExpressionKind::LiteralCharacter(_) => todo!(),
            ExpressionKind::BinaryOpAdd => {
                let left = expr.left.as_mut().unwrap().as_mut();
                let right = expr.right.as_mut().unwrap().as_mut();
                self.codegen_expr(state, left, code);
                self.codegen_expr(state, right, code);
                let l = left.node.reg;
                let r = right.node.reg;

                writeln!(code, r#"add {}, {}"#, self.scratch_name(l), self.scratch_name(r));
                expr.node.reg = l;
            }
            ExpressionKind::BinaryOpLower => {
                let left = expr.left.as_mut().unwrap().as_mut();
                let right = expr.right.as_mut().unwrap().as_mut();
                self.codegen_expr(state, left, code);
                self.codegen_expr(state, right, code);
                let l = left.node.reg;
                let r = right.node.reg;

                writeln!(code, r#"cmp {}, {}"#, self.scratch_name(l), self.scratch_name(r));
            }
            ExpressionKind::BinaryOpSub => todo!(),
            ExpressionKind::BinaryOpMul => todo!(),
            ExpressionKind::BinaryOpDiv => todo!(),
            ExpressionKind::UnaryOpNeg => todo!(),
            ExpressionKind::UnaryOpNot => todo!(),
            ExpressionKind::BinaryOpArrayAccess => todo!(),
            ExpressionKind::FuncCall => todo!(),
            ExpressionKind::FuncArg => todo!(),
            ExpressionKind::BinaryOpAssignment => {
                let left = expr.left.as_mut().unwrap().as_mut();
                let right = expr.right.as_mut().unwrap().as_mut();
                self.codegen_expr(state, right, code);
                let r = right.node.reg;
                writeln!(
                    code,
                    r#"mov {}, {}"#,
                    &self.scratch_name(r),
                    state.var_get(&left.name).unwrap(),
                );
                state.var_insert(&left.name, &self.scratch_name(r));
            }
        }
    }

    fn codegen_print(&mut self, state: &mut IrState, exprs: &mut Vec<Expression>, code: &mut String) {
        writeln!(code, r#";; print start"#);
        for expr in exprs {
            // save "CALLER" registers
            writeln!(code, r#"push rcx"#);
            writeln!(code, r#"push rdx"#);
            writeln!(code, r#"push r8"#);
            writeln!(code, r#"push r9"#);
            writeln!(code, r#"push r10"#);
            writeln!(code, r#"push r11"#);

            match &expr.kind {
                ExpressionKind::LiteralInteger(i) => {}
                ExpressionKind::LiteralString(s) => {
                    let str_label = format!("str_{}", state.strs.len());
                    state.str_insert(s, &str_label);

                    writeln!(code, r#"mov rdi, fmt_string"#);
                    writeln!(code, r#"mov rsi, {}"#, str_label);
                    writeln!(code, r#"call printf"#);
                }
                ExpressionKind::Identifier(id) => {
                    let s = state.var_get(id).unwrap();
                    writeln!(code, r#"mov rdi, fmt_int"#);
                    writeln!(code, r#"mov rsi, {s}"#);
                    writeln!(code, r#"call printf"#);
                }
                ExpressionKind::LiteralBoolean(_) => todo!(),
                ExpressionKind::LiteralCharacter(_) => todo!(),
                ExpressionKind::BinaryOpAdd => todo!(),
                ExpressionKind::BinaryOpSub => todo!(),
                ExpressionKind::BinaryOpMul => todo!(),
                ExpressionKind::BinaryOpDiv => todo!(),
                ExpressionKind::UnaryOpNeg => todo!(),
                ExpressionKind::UnaryOpNot => todo!(),
                ExpressionKind::BinaryOpArrayAccess => todo!(),
                ExpressionKind::FuncCall => todo!(),
                ExpressionKind::FuncArg => todo!(),
                ExpressionKind::BinaryOpAssignment => todo!(),
                ExpressionKind::BinaryOpLower => todo!(),
            }

            // restore registers
            writeln!(code, r#"pop r11"#);
            writeln!(code, r#"pop r10"#);
            writeln!(code, r#"pop r9"#);
            writeln!(code, r#"pop r8"#);
            writeln!(code, r#"pop rdx"#);
            writeln!(code, r#"pop rcx"#);
            writeln!(code, r#";; print end"#);
        }
    }

    fn codegen_return(&mut self, state: &mut IrState, r: &mut Expression, code: &mut String) {
        self.codegen_expr(state, r, code);

        writeln!(code, r#"mov rdi, {}"#, self.scratch_name(r.node.reg));
        writeln!(code, r#"call exit"#);
        writeln!(code);
    }

    // for now loops only support for-like loops where the condition is checked ad the begining of
    // every loop and there is an inconditional jump to the begining of the loop
    fn codegen_loop(&mut self, state: &mut IrState, l: &mut Loop, code: &mut String) {
        writeln!(code, r#";; LOOP "#);
        let Loop {
            init_expr_opt,
            control_expr_opt,
            next_expr_opt,
            loop_body,
        } = l;
        let label_begin = self.label_create();
        let label_end = self.label_create();

        // initialize variables if an init expression exists
        writeln!(code, r#";; init"#);
        if let Some(expr) = init_expr_opt.as_mut() {
            self.codegen_expr(state, expr, code);
        }
        // add the jump label at the beginning of the loop
        writeln!(code, r#"{}:"#, self.label_name(label_begin));
        writeln!(code, r#";; condition"#);
        if let Some(expr) = control_expr_opt.as_mut() {
            self.codegen_expr(state, expr, code);
            // conditional jump to break the loop, for now only if not equal to zero
            writeln!(code, r#"jz {}"#, self.label_name(label_end));
        }
        writeln!(code, r#";; statements"#);
        for stmt in loop_body {
            self.codegen_stmt(state, stmt, code)
        }
        writeln!(code, r#";; next"#);
        if let Some(expr) = next_expr_opt.as_mut() {
            self.codegen_expr(state, expr, code);
        }
        // inconditional jump to the beginning of the loop
        writeln!(code, r#"jmp {}"#, self.label_name(label_begin));
        writeln!(code, r#"{}:"#, self.label_name(label_end));
    }

    fn codegen_assign(&mut self, state: &mut IrState, a: &mut Assignment, code: &mut String) {
        let Assignment { name, val } = a;
        self.codegen_expr(state, val, code);
        let val_reg = val.node.reg;
        let assign_dest = state.var_get(name).unwrap();
    }
}
// --- ---
