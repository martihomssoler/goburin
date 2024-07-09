use crate::frontend::{Ast, Decl, Print, Stmt, SymbolTable, Value};

pub fn backend_pass(ir: Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();

    let symbol_table = ir.symbol_table;

    for stmt in ir.stmts {
        codegen_stmt(&symbol_table, stmt, &mut errors);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn codegen_stmt(symbol_table: &SymbolTable, stmt: Stmt, errors: &mut Vec<String>) {
    match stmt {
        Stmt::Decl(decl) => codegen_decl(symbol_table, decl, errors),
        Stmt::Print(print) => codegen_print(symbol_table, print, errors),
    }
}

fn codegen_decl(symbol_table: &SymbolTable, decl: Decl, errors: &mut Vec<String>) {}

fn codegen_print(symbol_table: &SymbolTable, print: Print, errors: &mut Vec<String>) {
    for val in print.vals {
        match val.kind {
            Value::Constant(c) => match c {
                crate::frontend::Constant::Int(i) => todo!(),
                crate::frontend::Constant::Bool(b) => todo!(),
                crate::frontend::Constant::Char(c) => todo!(),
                crate::frontend::Constant::String(s) => todo!(),
            },
            Value::Identifier(id) => {
                let Some(var) = symbol_table.scope_symbol_lookup(&id.0) else {
                    unreachable!(
                        "Non-declared variables should be caught in the semantic analisis"
                    );
                };

                match var.kind {
                    crate::frontend::SymbolKind::Local { pos } => todo!(),
                    crate::frontend::SymbolKind::Parameter { pos } => todo!(),
                    crate::frontend::SymbolKind::Global => todo!(),
                }
            }
        }
    }
}

// --- TARGET ---
pub trait Target {
    fn scratch_alloc(&mut self) -> u32;
    fn scratch_free(&mut self, r: u32);
    fn scratch_name(&self, r: u32) -> String;
}

pub struct X86_64 {
    pub regs: [bool; X86_64::SCRATCH_REGS.len()],
}

impl X86_64 {
    const SCRATCH_REGS: [&'static str; 7] =
        ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"];
}

impl Target for X86_64 {
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
}
// --- ---
