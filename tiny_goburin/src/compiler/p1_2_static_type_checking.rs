use super::*;

impl Ast {
    pub fn p1_2_static_type_checking(self) -> Result<Ast, String> {
        t_static_type_checking(&self)?;
        Ok(self)
    }
}

// --- STATIC TYPE CHECKING ---
fn t_static_type_checking(ast: &Ast) -> Result<(), String> {
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
            // TODO(mhs): Allow for arbitrary return types
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
        Statement::Block(_) => todo!(),
        Statement::Conditional(_) => todo!(),
        Statement::Loop(Loop {
            init_expr_opt,
            control_expr_opt,
            next_expr_opt,
            loop_body,
        }) => {
            if let Some(expr) = init_expr_opt.as_ref() {
                typecheck_expr(st, expr, errors);
            }
            if let Some(expr) = control_expr_opt.as_ref() {
                typecheck_expr(st, expr, errors);
            }
            if let Some(expr) = next_expr_opt.as_ref() {
                typecheck_expr(st, expr, errors);
            }
            for stmt in loop_body {
                typecheck_stmt(st, stmt, errors);
            }
        }
        Statement::Assignment(Assignment { name, val }) => {
            let name_typ = st.scope_symbol_lookup(name).map(|s| s.typ).unwrap_or(Type::Untyped);
            let val_typ = typecheck_expr(st, val, errors);
            if !type_equals(&name_typ, &val_typ) {
                errors.push(format!(
                    "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
                    name_typ, val_typ, name, val.node.token.line, val.node.token.col,
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
        ExpressionKind::LiteralBoolean(_) => todo!(),
        ExpressionKind::LiteralCharacter(_) => todo!(),
        ExpressionKind::BinaryOpAssignment
        | ExpressionKind::BinaryOpLower
        | ExpressionKind::BinaryOpSub
        | ExpressionKind::BinaryOpMul
        | ExpressionKind::BinaryOpDiv
        | ExpressionKind::BinaryOpAdd => {
            let left_typ = typecheck_expr(st, expr.left.as_ref().unwrap().as_ref(), errors);
            let right_typ = typecheck_expr(st, expr.right.as_ref().unwrap().as_ref(), errors);
            if !type_equals(&left_typ, &right_typ) {
                errors.push(format!(
                    "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
                    left_typ, right_typ, expr.name, expr.node.token.line, expr.node.token.col,
                ));
                // TODO(mhs): possibly change this to the "Never" type to indicate that this can never happen?
                Type::Untyped
            } else {
                left_typ
            }
        }
        ExpressionKind::UnaryOpNeg => todo!(),
        ExpressionKind::UnaryOpNot => todo!(),
        ExpressionKind::BinaryOpArrayAccess => todo!(),
        ExpressionKind::FuncCall => todo!(),
        ExpressionKind::FuncArg => todo!(),
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
