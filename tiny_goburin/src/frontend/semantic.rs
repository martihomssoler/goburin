use super::*;

pub fn check(mut ast: Ast) -> Result<Ast, String> {
    semantic_resolve(&mut ast)?;
    typecheck(&ast)?;
    Ok(ast)
}

// --- STATIC TYPE CHECKING ---
fn typecheck(ast: &Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let symbol_table = &ast.symbol_table;

    for i in 0..ast.declarations.len() {
        typecheck_decl(symbol_table, &ast.declarations[i], &mut errors);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

// fn typecheck_decl(symbol_table: &SymbolTable, stmt: &Declaration, errors: &mut Vec<String>) {
//     match stmt {
//         Stmt::Decl(decl) => typecheck_decl(symbol_table, decl, errors),
//         Stmt::Print(Print { vals }) => {
//             for val in vals {
//                 typecheck_val(symbol_table, val, errors);
//             }
//         }
//     };
// }

fn typecheck_decl(symbol_table: &SymbolTable, decl: &Declaration, errors: &mut Vec<String>) {
    let Some(expr) = &decl.val else {
        return;
    };
    let t_val = typecheck_expr(symbol_table, expr, errors);
    if !type_equals(&t_val, &decl.typ) {
        errors.push(format!(
            "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
            decl.typ, t_val, decl.name, expr.node.token.line, expr.node.token.col,
        ));
    }
}

fn typecheck_expr(symbol_table: &SymbolTable, expr: &Expression, errors: &mut Vec<String>) -> Type {
    match &expr.kind {
        ExpressionKind::LiteralInteger(_) => Type::Int,
        ExpressionKind::LiteralString(_) => Type::String,
        ExpressionKind::Identifier(id) => {
            if let Some(symbol) = symbol_table.scope_symbol_lookup(id) {
                symbol.typ
            } else {
                errors.push(format!(
                    "Undeclared variable '{}' in {}:{}",
                    id, expr.node.token.line, expr.node.token.col
                ));
                Type::Untyped
            }
        }
        ExpressionKind::PrintCall(_) => Type::Void,
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
pub fn semantic_resolve(ast: &mut Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let mut symbol_table = SymbolTable::default();

    // name resolution
    for i in 0..ast.declarations.len() {
        resolve_decl(&mut symbol_table, &mut ast.declarations[i], &mut errors);
    }

    ast.symbol_table = symbol_table;

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn resolve_decl(symbol_table: &mut SymbolTable, decl: &mut Declaration, errors: &mut Vec<String>) {
    let kind = if symbol_table.scope_level() > 1 {
        SymbolKind::Local { pos: 0 }
    } else {
        SymbolKind::Global
    };

    let symbol = Symbol {
        name: decl.name.clone(),
        kind,
        typ: decl.typ.clone(),
    };

    let Some(expr) = &mut decl.val else {
        return;
    };
    expr.node.symbol = Some(symbol.clone());
    resolve_expr(symbol_table, expr, errors);
    symbol_table.scope_symbol_bind(symbol);
}

fn resolve_expr(symbol_table: &SymbolTable, expr: &mut Expression, errors: &mut Vec<String>) {
    match &mut expr.kind {
        ExpressionKind::LiteralInteger(_) | ExpressionKind::LiteralString(_) => (),
        ExpressionKind::Identifier(id) => match symbol_table.scope_symbol_lookup(id) {
            Some(s) => expr.node.symbol = Some(s),
            None => {
                errors.push(format!(
                    "[ERROR]: Use of undeclared variable {} in {}:{}",
                    id, expr.node.token.line, expr.node.token.col
                ));
            }
        },
        ExpressionKind::PrintCall(PrintCall { exprs }) => {
            for expr in exprs {
                resolve_expr(symbol_table, expr, errors);
            }
        }
    }
}
