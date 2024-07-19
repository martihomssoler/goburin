use super::*;

impl Ast {
    pub fn p1_1_name_resolution(mut self) -> Result<Ast, String> {
        n_name_resolution(&mut self)?;
        Ok(self)
    }
}

// --- NAME RESOLUTION ---
pub fn n_name_resolution(ast: &mut Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let mut st = SymbolTable::default();

    // name resolution
    for i in 0..ast.program.len() {
        resolve_def(&mut st, &mut ast.program[i], &mut errors);
    }

    ast.symbol_table = st;

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn resolve_def(st: &mut SymbolTable, def: &mut Definition, errors: &mut Vec<String>) {
    match &mut def.val {
        DefinitionValue::FunctionBody(stmts) => {
            for stmt in stmts {
                resolve_stmt(st, stmt, errors);
            }
        }
    }
}

fn resolve_stmt(st: &mut SymbolTable, stmt: &mut Statement, errors: &mut Vec<String>) {
    match stmt {
        Statement::Declaration(decl) => resolve_decl(st, decl, errors),
        Statement::Expression(expr) => resolve_expr(st, expr, errors),
        Statement::Print(exprs) => {
            for expr in exprs {
                resolve_expr(st, expr, errors);
            }
        }
        Statement::Return(ret) => resolve_expr(st, ret, errors),
    }
}

fn resolve_decl(st: &mut SymbolTable, decl: &mut Declaration, errors: &mut Vec<String>) {
    let kind = if st.scope_level() > 1 {
        SymbolKind::Local { pos: 0 }
    } else {
        SymbolKind::Global
    };

    let symbol = Symbol {
        name: decl.name.clone(),
        kind,
        typ: decl.typ.clone(),
    };

    match &mut decl.val {
        DeclarationValue::Uninitialized => return,
        DeclarationValue::Expression(expr) => {
            expr.name.clone_from(&symbol.name);
            expr.node.symbol = Some(symbol.clone());
            resolve_expr(st, expr, errors);
        }
        DeclarationValue::FunctionBody(stmts) => {
            for stmt in stmts {
                resolve_stmt(st, stmt, errors);
            }
        }
    }
    let DeclarationValue::Expression(expr) = &mut decl.val else {
        return;
    };

    st.scope_symbol_bind(symbol);
}

fn resolve_expr(st: &SymbolTable, expr: &mut Expression, errors: &mut Vec<String>) {
    match &mut expr.kind {
        ExpressionKind::LiteralInteger(_) | ExpressionKind::LiteralString(_) => (),
        ExpressionKind::Identifier(id) => match st.scope_symbol_lookup(id) {
            Some(s) => expr.node.symbol = Some(s),
            None => {
                errors.push(format!(
                    "[ERROR]: Use of undeclared variable {} in {}:{}",
                    id, expr.node.token.line, expr.node.token.col
                ));
            }
        },
    }
}
