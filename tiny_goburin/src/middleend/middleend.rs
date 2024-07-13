use std::collections::HashMap;

use crate::frontend::{Ast, Stmt, SymbolTable};

pub fn middleend_pass(ast: Ast) -> Result<Ir, String> {
    let ir = Ir {
        stmts: ast.stmts,
        symbol_table: ast.symbol_table,
        vars: Vec::new(),
        strs: Vec::new(),
    };
    Ok(ir)
}

pub struct Ir {
    pub stmts: Vec<Stmt>,
    pub symbol_table: SymbolTable,
    pub vars: Vec<(String, String)>,
    pub strs: Vec<(String, String)>,
}

impl Ir {
    pub fn var_insert(&mut self, name: &str, val: &str) {
        self.vars.push((name.to_owned(), val.to_owned()));
    }

    pub fn var_get(&mut self, name: &str) -> Option<String> {
        for i in (0..self.vars.len()).rev() {
            let curr_var = &self.vars[i];

            if curr_var.0.eq(name) {
                return Some(curr_var.1.clone());
            }
        }

        None
    }

    pub fn str_insert(&mut self, name: &str, val: &str) {
        self.strs.push((name.to_owned(), val.to_owned()));
    }

    pub fn str_get(&mut self, name: &str) -> Option<String> {
        for i in (0..self.strs.len()).rev() {
            let curr_var = &self.strs[i];

            if curr_var.0.eq(name) {
                return Some(curr_var.1.clone());
            }
        }

        None
    }

    pub fn instructions(&self) -> Vec<Stmt> {
        self.stmts.clone()
    }
}
