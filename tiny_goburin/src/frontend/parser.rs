use std::os::unix::fs::FileTypeExt;

use super::*;

pub fn p_parse(tokens: Vec<Token<TokenKind>>) -> Result<Ast, String> {
    let mut errors: Vec<String> = Vec::new();

    let program = parse_program(&tokens, &mut errors)?;

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(Ast {
        program,
        symbol_table: SymbolTable::default(),
    })
}

fn parse_program(tokens: &[Token<TokenKind>], errors: &mut Vec<String>) -> Result<Vec<Definition>, String> {
    let mut definitions = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        let token = &tokens[idx];
        idx += 1;

        match &token.kind {
            TokenKind::Value(Value::Identifier(id)) => {
                let definition = match parse_definition(id, tokens, &mut idx) {
                    Ok(d) => d,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                };
                definitions.push(definition);
            }
            TokenKind::Eof => break,
            _ => {
                errors.push(format!(
                    "[parse_program] Unexpected token {:?} at line {} and col {}",
                    token.kind, token.line, token.col
                ));
                continue;
            }
        }
    }
    Ok(definitions)
}

fn parse_definition(id: &Identifier, tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Definition, String> {
    // double colon
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Colon))?;
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Colon))?;
    // type
    let typ = consume_type(tokens, idx)?.kind;
    // equal
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Equal))?;
    // value
    // NOTE(mhs): for now we only support function bodies
    let val = parse_definition_value(tokens, idx)?;
    Ok(Definition {
        id: id.clone(),
        typ,
        val,
    })
}

fn parse_definition_value(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<DefinitionValue, String> {
    let mut statements = Vec::new();
    // open braces
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceLeft))?;
    // parse statements
    while *idx < tokens.len()
        && !matches!(
            tokens[*idx].kind,
            TokenKind::Operator(Operator::BraceRight) | TokenKind::Eof
        )
    {
        let statement = parse_statement(tokens, idx)?;
        statements.push(statement);
    }
    // NOTE(mhs): do not allow empty function bodies
    if statements.is_empty() {
        return Err("Empty function body.".to_owned());
    }
    // close braces
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceRight))?;
    Ok(DefinitionValue::FunctionBody(statements))
}

fn parse_statement(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Statement, String> {
    let token_kind = &tokens[*idx].kind;
    *idx += 1;
    let stmt = match token_kind {
        TokenKind::Keyword(kw) => match &kw {
            Keyword::Print => parse_statement_print(tokens, idx)?,
            Keyword::Return => {
                let stmt = Statement::Return(parse_expression(tokens, idx)?);
                consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
                stmt
            }
            _ => todo!(),
        },
        TokenKind::Value(v) => match v {
            Value::Constant(c) => {
                let stmt = Statement::Return(parse_expression(tokens, idx)?);
                consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
                stmt
            }
            Value::Identifier(id) => Statement::Declaration(parse_declaration(id.0.clone(), tokens, idx)?),
        },
        TokenKind::Eof => return Err("Encountered EOF while parsing a statement.".to_owned()),
        _ => return Err(format!("[parser_statement] Unexpected token {token_kind:?}.")),
    };

    Ok(stmt)
}

fn parse_statement_print(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Statement, String> {
    let mut exprs = Vec::new();
    let mut next_is_comma = false;
    while *idx < tokens.len()
        && !matches!(
            tokens[*idx].kind,
            TokenKind::Operator(Operator::Semicolon) | TokenKind::Eof
        )
    {
        if next_is_comma {
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Comma))?;
            next_is_comma = false;
        } else {
            exprs.push(parse_expression(tokens, idx)?);
            next_is_comma = true;
        }
    }
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
    Ok(Statement::Print(exprs))
}

fn parse_expression(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Expression, String> {
    let val = consume_value(tokens, idx)?;
    Ok(from_token_value_to_expression(val))
}

// TokenKind::Keyword(kw) => match kw {
//     Keyword::Return => {
//         todo!()
//     }
//     Keyword::Print => {
//     }
//     _ => {
//         unimplemented!("Keyword {kw:?} not supported yet!")
//     }
// },
// TokenKind::Type(_) | TokenKind::Operator(_) | TokenKind::Value(_) => {
//     errors.push(format!(
//         "Unexpected token {:?} at line {} and col {}",
//         token.kind, token.line, token.col
//     ));
//     continue;
// }
/// name : type = value;
fn parse_declaration(name: String, tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Declaration, String> {
    // colon
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Colon))?;
    // type
    let typ = consume_type(tokens, idx)?;
    if consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon)).is_ok() {
        // empty declaration
        return Ok(Declaration {
            name,
            typ: typ.kind,
            val: DeclarationValue::Uninitialized,
        });
    }

    // equal
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Equal))?;
    // value
    let val = match typ.kind {
        Type::Untyped => todo!(),
        Type::Array(_) => todo!(),
        Type::Function(_, _) => {
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceLeft))?;
            let mut statements = Vec::new();
            while *idx < tokens.len()
                && !matches!(
                    tokens[*idx].kind,
                    TokenKind::Operator(Operator::BraceRight) | TokenKind::Eof
                )
            {
                let val = parse_statement(tokens, idx)?;
            }
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceRight))?;
            DeclarationValue::FunctionBody(statements)
        }
        Type::Bool | Type::Char | Type::String | Type::Int => {
            let val = consume_value(tokens, idx)?;
            // semicolon
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
            DeclarationValue::Expression(from_token_value_to_expression(val))
        }
        Type::Tuple(_, _) => todo!(),
        Type::Void => todo!(),
    };

    Ok(Declaration {
        name,
        typ: typ.kind,
        val,
    })
}

fn from_token_value_to_expression(val: Token<Value>) -> Expression {
    let node = AstNode::from_token_value(&val);
    match val.kind {
        Value::Constant(c) => match c {
            Constant::Int(i) => Expression {
                name: "Integer".to_string(),
                kind: ExpressionKind::LiteralInteger(i),
                left: None,
                right: None,
                node,
            },
            Constant::Bool(_) => todo!(),
            Constant::Char(_) => todo!(),
            Constant::String(s) => Expression {
                name: "String".to_string(),
                kind: ExpressionKind::LiteralString(s),
                left: None,
                right: None,
                node,
            },
        },
        Value::Identifier(id) => Expression {
            name: "Identifier".to_string(),
            kind: ExpressionKind::Identifier(id.0),
            left: None,
            right: None,
            node,
        },
    }
}

fn consume_value(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Token<Value>, String> {
    let prev_idx = *idx;
    consume(false, &tokens[prev_idx], idx, TokenKind::constant())?;
    let TokenKind::Value(ref decl_val) = tokens[prev_idx].kind else {
        unreachable!()
    };
    let val = Token {
        kind: decl_val.clone(),
        line: tokens[prev_idx].line,
        col: tokens[prev_idx].col,
        sym: None,
    };

    Ok(val)
}

fn consume_type(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Token<Type>, String> {
    let prev_idx = *idx;
    // check for recursive/multi types
    // TODO(mhs): add the rest of recursive/multi types
    if let TokenKind::Type(Type::Function(_, _)) = &tokens[prev_idx].kind {
        *idx += 1;
        return consume_type_function(tokens, idx);
    }

    consume(false, &tokens[prev_idx], idx, TokenKind::Type(Type::Void))?;
    let TokenKind::Type(ref decl_typ) = tokens[prev_idx].kind else {
        unreachable!()
    };
    let typ = Token {
        kind: decl_typ.clone(),
        line: tokens[prev_idx].line,
        col: tokens[prev_idx].col,
        sym: None,
    };

    Ok(typ)
}

fn consume_type_function(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Token<Type>, String> {
    let prev_idx = *idx;
    // open parenthesis
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::ParenthesisLeft))?;

    // -- start parsing arguments
    // format => arg_name : arg_type , ..
    let mut arg_names = Vec::new();
    let mut arg_types = Vec::new();
    // TODO(mhs): most simple way to have 4 states, once we are self-hosted I should improve this mess
    let mut next_is_argname = true;
    let mut next_is_colon = false;
    let mut next_is_type = false;
    let mut next_is_comma = false;
    while *idx < tokens.len()
        && !matches!(
            tokens[*idx].kind,
            TokenKind::Operator(Operator::ParenthesisRight) | TokenKind::Eof
        )
    {
        if next_is_argname {
            // TODO(mhs): keep the Token<_> struct for better error messages
            let name = consume_identifier(tokens, idx)?.kind;
            arg_names.push(name);
            next_is_argname = false;
            next_is_colon = true;
            continue;
        }
        if next_is_colon {
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Colon))?;
            next_is_colon = false;
            next_is_type = true;
            continue;
        }
        if next_is_type {
            // TODO(mhs): keep the Token<_> struct for better error messages
            let typ = consume_type(tokens, idx)?.kind;
            arg_types.push(typ);
            next_is_type = false;
            next_is_comma = true;
            continue;
        }
        if next_is_comma {
            consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Comma))?;
            next_is_comma = false;
            next_is_argname = true;
            continue;
        }
    }

    if arg_names.len() != arg_types.len() {
        return Err("Non-matching number of Argument Identifiers and Types.".to_owned());
    }

    consume(
        true,
        &tokens[*idx],
        idx,
        TokenKind::Operator(Operator::ParenthesisRight),
    )?;
    // function params done now function arrow
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Arrow))?;
    // consume return type
    // TODO(mhs): for now we only accept single types as return values
    let return_typ = consume_type(tokens, idx)?.kind;
    let mut args = Vec::new();
    for i in 0..arg_names.len() {
        args.push((arg_names[i].clone(), arg_types[i].clone()));
    }

    let func_typ = Token {
        kind: Type::Function(args, Box::new(return_typ)),
        line: tokens[prev_idx].line,
        col: tokens[prev_idx].col,
        sym: None,
    };

    Ok(func_typ)
}

fn consume_identifier(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Token<Identifier>, String> {
    let prev_idx = *idx;
    consume(
        false,
        &tokens[prev_idx],
        idx,
        TokenKind::Value(Value::Identifier(Identifier("None".to_owned()))),
    )?;
    let TokenKind::Value(Value::Identifier(ref id)) = tokens[prev_idx].kind else {
        unreachable!()
    };
    let val = Token {
        kind: id.clone(),
        line: tokens[prev_idx].line,
        col: tokens[prev_idx].col,
        sym: None,
    };

    Ok(val)
}

fn consume(strict: bool, actual: &Token<TokenKind>, idx: &mut usize, expected: TokenKind) -> Result<(), String> {
    let is_match = if strict {
        actual.kind.eq(&expected)
    } else {
        std::mem::discriminant(&actual.kind) == std::mem::discriminant(&expected)
    };

    if is_match {
        *idx += 1;
        Ok(())
    } else {
        Err(format!(
            "[consume] Expected '{:?}' but got '{:?}' on line {} and col {}",
            expected, actual.kind, actual.line, actual.col
        ))
    }
}

// fn parse_stmt(token_iter: &mut TokenIter) -> CompilerResult<Stmt> {
//     if let Some(t) = token_iter.next() {
//         match t.kind {
//             TokenKind::Keyword(k) => match k {
//                 Keyword::Return => {
//                     let expr = parse_expr(token_iter, 0)?;
//                     consume(token_iter, TokenKind::Semicolon)?;
//                     Ok(Stmt::Return(expr))
//                 }
//                 Keyword::Print => {
//                     consume(token_iter, TokenKind::LeftParenthesis)?;
//                     let expr = parse_expr(token_iter, 0)?;
//                     consume(token_iter, TokenKind::RightParenthesis)?;
//                     consume(token_iter, TokenKind::Semicolon)?;
//                     Ok(Stmt::Print(expr))
//                 }
//                 Keyword::Const => {
//                     let (ident, expr_opt) = parse_declaration(token_iter)?;
//                     Ok(Stmt::Let(ident, expr_opt))
//                 }
//                 Keyword::Mut => {
//                     let (ident, expr_opt) = parse_declaration(token_iter)?;
//                     Ok(Stmt::Mut(ident, expr_opt))
//                 }
//                 Keyword::Let => {
//                     let (ident, expr_opt) = parse_declaration(token_iter)?;
//                     Ok(Stmt::Let(ident, expr_opt))
//                 }
//                 Keyword::For => {
//                     let condition_expr = parse_expr(token_iter, 0)?;
//                     consume(token_iter, TokenKind::LeftParenthesis)?;
//                     let mut stmts = Vec::new();
//                     while let Some(t) = token_iter.peek()
//                         && !t.kind.eq(&TokenKind::RightParenthesis)
//                     {
//                         let stmt = parse_stmt(token_iter)?;
//                         stmts.push(stmt);
//                     }
//                     consume(token_iter, TokenKind::RightParenthesis)?;
//                     Ok(Stmt::For(condition_expr, stmts))
//                 }
//             },
//             // id = ?;
//             TokenKind::Identifier(id)
//                 if token_iter
//                     .peek()
//                     .map(|t| t.kind == TokenKind::Operator(Operator::Equal))
//                     .unwrap_or(false) =>
//             {
//                 consume(token_iter, TokenKind::Operator(Operator::Equal))?;
//                 let expr = parse_expr(token_iter, 0)?;
//                 consume(token_iter, TokenKind::Semicolon)?;
//                 Ok(Stmt::Assignment(id, expr))
//             }
//             // id[?]
//             TokenKind::Identifier(id)
//                 if token_iter
//                     .peek()
//                     .map(|t| t.kind == TokenKind::LeftBracket)
//                     .unwrap_or(false) =>
//             {
//                 consume(token_iter, TokenKind::LeftBracket)?;
//                 let expr = parse_expr(token_iter, 0)?;
//                 consume(token_iter, TokenKind::RightBracket)?;
//                 Ok(Stmt::ArrayIndexing(id, expr))
//             }
//             TokenKind::Identifier(_)
//             | TokenKind::Integer(_)
//             | TokenKind::String(_)
//             | TokenKind::Operator(_)
//             | TokenKind::Colon
//             | TokenKind::Semicolon
//             | TokenKind::LeftParenthesis
//             | TokenKind::RightParenthesis
//             | TokenKind::LeftBracket
//             | TokenKind::RightBracket
//             | TokenKind::EOF => {
//                 let expr = parse_expr(token_iter, 0)?;
//                 consume(token_iter, TokenKind::Semicolon)?;
//                 Ok(Stmt::Expr(expr))
//             }
//         }
//     } else {
//         // TODO: improve errors
//         Err(CompilerError::Generic(
//             "calling parse_stmt with empty iterator".to_owned(),
//         ))
//     }
// }

// fn parse_declaration(token_iter: &mut TokenIter) -> Result<(String, Option<Expr>), CompilerError> {
//     let TokenKind::Identifier(id) = consume(token_iter, TokenKind::Identifier(String::new()))?.kind
//     else {
//         panic!("The token should be an identifier");
//     };
//     let mut expr_opt = None;
//     if consume(token_iter, TokenKind::Colon).is_ok() {
//         consume(token_iter, TokenKind::Operator(Operator::Equal))?;
//         expr_opt = Some(parse_expr(token_iter, 0)?);
//     };
//     consume(token_iter, TokenKind::Semicolon)?;
//     Ok((id, expr_opt))
// }

// fn parse_expr(token_iter: &mut TokenIter, bind_pow: u8) -> CompilerResult<Expr> {
//     let Some(t) = token_iter.next() else {
//         // TODO: improve errors
//         return Err(CompilerError::Generic(
//             "calling parse_expr with empty iterator".to_owned(),
//         ));
//     };

//     let mut left = match t.kind {
//         TokenKind::Operator(op) if let Some((_, right_bind_pow)) = prefix_bind_pow(&op) => {
//             let expr = parse_expr(token_iter, right_bind_pow)?;
//             Expr::Operation(op, vec![expr])
//         }
//         TokenKind::LeftParenthesis => {
//             let expr = parse_expr(token_iter, 0)?;
//             consume(token_iter, TokenKind::RightParenthesis)?;
//             Expr::Parenthesis(Box::new(expr))
//         }
//         TokenKind::Integer(i) => Expr::Atom(Atom::Constant(Constant::Integer(i))),
//         TokenKind::String(s) => Expr::Atom(Atom::String(s)),
//         TokenKind::Identifier(id) => Expr::Atom(Atom::Identifier(id)),
//         t => {
//             panic!("wrong token {t}")
//         }
//     };

//     loop {
//         let Some(t) = token_iter.peek() else {
//             // TODO: improve errors
//             return Err(CompilerError::Generic(
//                 "calling parse_expr with empty iterator".to_owned(),
//             ));
//         };

//         let op = match &t.kind {
//             TokenKind::Operator(op) => op,
//             _ => break,
//         };

//         // now that we know that the `op` is a correct operator, we call next and clone.
//         let op = op.clone();

//         if let Some((left_bind_pow, _)) = postfix_bind_pow(&op) {
//             if left_bind_pow < bind_pow {
//                 break;
//             }

//             continue;
//         }
//         if let Some((left_bind_pow, right_bind_pow)) = infix_bind_pow(&op) {
//             if (left_bind_pow < bind_pow) {
//                 break;
//             }

//             token_iter.next();
//             let right = parse_expr(token_iter, right_bind_pow)?;
//             left = Expr::Operation(op, vec![left, right]);
//             continue;
//         }

//         break;
//     }

//     Ok(left)
// }

// /// TODO(mhs): shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
// /// and adapted to our needs with the following rule in mind
// /// we use and odd number for the bare priority and bump it up by one for associativity
// /// Bind Power      Operator
// ///     15          unary operations
// ///     13          *   /   %   %%   &   &~  <<   >>
// ///     11          +   -   |   ~    in  not_in
// ///      9          ==  !=  <   >    <=  >=
// ///      7          &&
// ///      5          ||
// ///      3          ..=    ..<
// ///      1          or_else  =  ?    if  when

// // TODO: make a single function for the binding powers
// fn postfix_bind_pow(op: &Operator) -> Option<(u8, ())> {
//     None
// }

// fn prefix_bind_pow(op: &Operator) -> Option<((), u8)> {
//     let res = match op {
//         Operator::Plus | Operator::Minus => ((), 15),
//         Operator::Equal => return None,
//         _ => panic!("bad prefix op: {op:?}"),
//     };

//     Some(res)
// }

// fn infix_bind_pow(op: &Operator) -> Option<(u8, u8)> {
//     let res = match op {
//         Operator::Plus | Operator::Minus => (11, 12),
//         Operator::Star | Operator::Slash => (13, 14),
//         Operator::Greater | Operator::Lower => (9, 10),
//         Operator::Equal => (1, 2), // TODO(mhs): is equal right associative?
//         _ => panic!("bad prefix op: {op:?}"),
//     };
//     Some(res)
// }
