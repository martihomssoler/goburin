use super::*;

pub fn p_parse(tokens: Vec<Token<TokenKind>>) -> Result<Ast, String> {
    let mut errors: Vec<String> = Vec::new();
    let mut declarations = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        let token = &tokens[idx];
        idx += 1;

        match &token.kind {
            TokenKind::Value(Value::Identifier(id)) => {
                // name : type = value;
                let name = id.0.clone();

                // colon
                consume(
                    true,
                    &tokens[idx],
                    &mut idx,
                    TokenKind::Operator(Operator::Colon),
                )?;
                // type
                let typ = consume_type(&tokens, &mut idx)?;
                // equal
                consume(
                    true,
                    &tokens[idx],
                    &mut idx,
                    TokenKind::Operator(Operator::Equal),
                )?;
                // value
                let val = consume_value(&tokens, &mut idx)?;
                // semicolon
                consume(
                    true,
                    &tokens[idx],
                    &mut idx,
                    TokenKind::Operator(Operator::Semicolon),
                )?;

                let node = AstNode::from_token_value(&val);
                let declaration = Declaration {
                    name,
                    typ: typ.kind,
                    val: from_token_value_to_expression(val),
                };

                declarations.push(declaration);
            }
            TokenKind::Keyword(kw) => match kw {
                Keyword::Print => {
                    // print a, b, c, ..., d;
                    let mut exprs = Vec::new();
                    let mut next_is_comma = false;

                    while idx < tokens.len()
                        && !matches!(
                            tokens[idx].kind,
                            TokenKind::Operator(Operator::Semicolon) | TokenKind::Eof
                        )
                    {
                        if next_is_comma {
                            consume(
                                true,
                                &tokens[idx],
                                &mut idx,
                                TokenKind::Operator(Operator::Comma),
                            )?;
                            next_is_comma = false;
                        } else {
                            let val = consume_value(&tokens, &mut idx)?;
                            exprs.push(from_token_value_to_expression(val));
                            next_is_comma = true;
                        }
                    }
                    // semicolon
                    consume(
                        true,
                        &tokens[idx],
                        &mut idx,
                        TokenKind::Operator(Operator::Semicolon),
                    )?;
                    idx += 1;

                    let node = AstNode::from_token(token);
                    let declaration = Declaration {
                        name: "Print".to_string(),
                        typ: Type::Void,
                        val: Expression {
                            name: "Integer".to_string(),
                            kind: ExpressionKind::PrintCall(PrintCall { exprs }),
                            left: None,
                            right: None,
                            node,
                        },
                    };
                    declarations.push(declaration);
                }
                _ => {
                    unimplemented!("Keyword {kw:?} not supported yet!")
                }
            },
            TokenKind::Type(_) | TokenKind::Operator(_) | TokenKind::Value(_) => {
                errors.push(format!(
                    "Unexpected token {:?} at line {} and col {}",
                    token.kind, token.line, token.col
                ));
                continue;
            }
            TokenKind::Eof => break,
        }
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(Ast {
        declarations,
        symbol_table: SymbolTable::default(),
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
    consume(
        false,
        &tokens[prev_idx],
        idx,
        TokenKind::Value(Value::Constant(Constant::Bool(false))),
    )?;
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

fn consume(
    strict: bool,
    actual: &Token<TokenKind>,
    idx: &mut usize,
    expected: TokenKind,
) -> Result<(), String> {
    let is_match = if strict {
        actual.kind.eq(&expected)
    } else {
        std::mem::discriminant(&actual.kind) == std::mem::discriminant(&expected)
    };
    *idx += 1;

    if is_match {
        Ok(())
    } else {
        Err(format!(
            "Expected '{:?}' but got '{:?}' on line {} and col {}",
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
