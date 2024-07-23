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
        Statement::Block(_) => todo!(),
        Statement::Conditional(_) => todo!(),
        Statement::Loop(Loop {
            init_expr_opt,
            control_expr_opt,
            next_expr_opt,
            loop_body,
        }) => {
            if let Some(expr) = init_expr_opt.as_mut() {
                resolve_expr(st, expr, errors)
            }
            if let Some(expr) = control_expr_opt.as_mut() {
                resolve_expr(st, expr, errors)
            }
            if let Some(expr) = next_expr_opt.as_mut() {
                resolve_expr(st, expr, errors)
            }
            for stmt in loop_body {
                resolve_stmt(st, stmt, errors);
            }
        }
        Statement::Assignment(assign) => resolve_assign(st, assign, errors),
    }
}

fn resolve_assign(st: &mut SymbolTable, assign: &mut Assignment, errors: &mut Vec<String>) {
    let Assignment { name, val } = assign;
    resolve_expr(st, val, errors);
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
    st.scope_symbol_bind(symbol.clone());

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
    resolve_expr(st, expr, errors)
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
        ExpressionKind::LiteralBoolean(_) => todo!(),
        ExpressionKind::LiteralCharacter(_) => todo!(),
        ExpressionKind::BinaryOpAssignment
        | ExpressionKind::BinaryOpLower
        | ExpressionKind::BinaryOpSub
        | ExpressionKind::BinaryOpMul
        | ExpressionKind::BinaryOpDiv
        | ExpressionKind::BinaryOpAdd => {
            if expr.left.is_none() || expr.right.is_none() {
                errors.push(format!(
                    "[ERROR]: {:?} must have two expressions left:{:?} and right:{:?} in {}:{}",
                    expr.kind, expr.left, expr.right, expr.node.token.line, expr.node.token.col
                ));
            }
            if let Some(expr) = expr.left.as_mut() {
                resolve_expr(st, expr, errors)
            }
            if let Some(expr) = expr.right.as_mut() {
                resolve_expr(st, expr, errors)
            }
        }
        ExpressionKind::UnaryOpNeg => todo!(),
        ExpressionKind::UnaryOpNot => todo!(),
        ExpressionKind::BinaryOpArrayAccess => todo!(),
        ExpressionKind::FuncCall => todo!(),
        ExpressionKind::FuncArg => todo!(),
    }
}
