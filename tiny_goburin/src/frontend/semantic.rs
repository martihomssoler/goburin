use super::*;

pub fn s_check(mut ast: Ast) -> Result<Ast, String> {
    resolve_names(&mut ast)?;
    typecheck(&ast)?;
    Ok(ast)
}

// --- STATIC TYPE CHECKING ---
fn typecheck(ast: &Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let st = &ast.symbol_table;

    for i in 0..ast.program.len() {
        typecheck_def(st, &ast.program[i], &mut errors);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn typecheck_def(st: &SymbolTable, def: &Definition, errors: &mut Vec<String>) {
    match &def.val {
        DefinitionValue::FunctionBody(stmts) => {
            for stmt in stmts {
                typecheck_stmt(st, stmt, errors);
            }
        }
    }
}

fn typecheck_stmt(st: &SymbolTable, stmt: &Statement, errors: &mut Vec<String>) {
    match stmt {
        Statement::Declaration(decl) => typecheck_decl(st, decl, errors),
        Statement::Expression(expr) => {
            typecheck_expr(st, expr, errors);
        }
        Statement::Print(exprs) => {
            for expr in exprs {
                typecheck_expr(st, expr, errors);
            }
        }
        Statement::Return(ret) => {
            let typ = typecheck_expr(st, ret, errors);
            if !type_equals(&Type::Int, &typ) {
                errors.push(format!(
                    "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
                    Type::Int,
                    typ,
                    ret.name,
                    ret.node.token.line,
                    ret.node.token.col,
                ));
            }
        }
    }
}

fn typecheck_decl(st: &SymbolTable, decl: &Declaration, errors: &mut Vec<String>) {
    let DeclarationValue::Expression(expr) = &decl.val else {
        return;
    };
    let t_val = typecheck_expr(st, expr, errors);
    if !type_equals(&t_val, &decl.typ) {
        errors.push(format!(
            "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
            decl.typ, t_val, decl.name, expr.node.token.line, expr.node.token.col,
        ));
    }
}

fn typecheck_expr(st: &SymbolTable, expr: &Expression, errors: &mut Vec<String>) -> Type {
    match &expr.kind {
        ExpressionKind::LiteralInteger(_) => Type::Int,
        ExpressionKind::LiteralString(_) => Type::String,
        ExpressionKind::Identifier(id) => {
            if let Some(symbol) = st.scope_symbol_lookup(id) {
                symbol.typ
            } else {
                errors.push(format!(
                    "Undeclared variable '{}' in {}:{}",
                    id, expr.node.token.line, expr.node.token.col
                ));
                Type::Untyped
            }
        }
    }
}

fn type_equals(a: &Type, b: &Type) -> bool {
    if a == b {
        if matches!(a, Type::Untyped) {
            false
        } else if matches!(a, Type::Array(_)) {
            false
        } else if matches!(a, Type::Function(_, _)) {
            false
        } else {
            true
        }
    } else {
        false
    }
}

// --- NAME RESOLUTION ---
pub fn resolve_names(ast: &mut Ast) -> Result<(), String> {
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
