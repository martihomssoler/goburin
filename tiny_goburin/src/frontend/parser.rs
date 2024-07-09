use super::*;

pub fn p_parse(tokens: Vec<Token<TokenKind>>) -> Result<Ast, String> {
    let mut errors: Vec<String> = Vec::new();
    let mut stmts = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        let token = &tokens[idx];
        idx += 1;

        match &token.kind {
            TokenKind::Value(Value::Identifier(id)) => {
                // assignment or declaration

                // declaration form
                // identifier : type = value;
                {
                    consume(true, &tokens[idx], TokenKind::Operator(Operator::Colon))?;
                    idx += 1;
                }
                let typ = {
                    consume(false, &tokens[idx], TokenKind::Type(Type::Void))?;
                    let TokenKind::Type(ref decl_typ) = tokens[idx].kind else {
                        unreachable!()
                    };
                    let typ = Token {
                        kind: decl_typ.clone(),
                        line: tokens[idx].line,
                        col: tokens[idx].col,
                    };
                    idx += 1;
                    typ
                };
                {
                    consume(true, &tokens[idx], TokenKind::Operator(Operator::Equal))?;
                    idx += 1;
                }
                let val = {
                    consume(
                        false,
                        &tokens[idx],
                        TokenKind::Value(Value::Constant(Constant::Bool(false))),
                    )?;
                    let TokenKind::Value(ref decl_val) = tokens[idx].kind else {
                        unreachable!()
                    };
                    let val = Token {
                        kind: decl_val.clone(),
                        line: tokens[idx].line,
                        col: tokens[idx].col,
                    };
                    idx += 1;
                    val
                };
                {
                    consume(true, &tokens[idx], TokenKind::Operator(Operator::Semicolon))?;
                    idx += 1;
                }

                let stmt_decl = Stmt::Decl(Decl {
                    id: Token {
                        kind: id.clone(),
                        line: token.line,
                        col: token.col,
                    },
                    typ,
                    val,
                    sym: None,
                });
                stmts.push(stmt_decl);
            }
            TokenKind::Keyword(kw) => match kw {
                Keyword::Print => {
                    let mut values = Vec::new();
                    let mut next_is_comma = false;

                    while idx < tokens.len()
                        && !matches!(
                            tokens[idx].kind,
                            TokenKind::Operator(Operator::Semicolon) | TokenKind::Eof
                        )
                    {
                        if next_is_comma {
                            consume(true, &tokens[idx], TokenKind::Operator(Operator::Comma))?;
                            next_is_comma = false;
                        } else {
                            consume(
                                false,
                                &tokens[idx],
                                TokenKind::Value(Value::Constant(Constant::Bool(false))),
                            )?;
                            let TokenKind::Value(ref decl_val) = tokens[idx].kind else {
                                unreachable!()
                            };
                            let val = Token {
                                kind: decl_val.clone(),
                                line: tokens[idx].line,
                                col: tokens[idx].col,
                            };
                            values.push(val);
                            next_is_comma = true;
                        }

                        idx += 1;
                    }
                    {
                        consume(true, &tokens[idx], TokenKind::Operator(Operator::Semicolon))?;
                        idx += 1;
                    }

                    let stmt_print = Stmt::Print { vals: values };
                    stmts.push(stmt_print);
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

    Ok(Ast { stmts })
}

fn consume(strict: bool, actual: &Token<TokenKind>, expected: TokenKind) -> Result<(), String> {
    let is_match = if strict {
        actual.kind.eq(&expected)
    } else {
        std::mem::discriminant(&actual.kind) == std::mem::discriminant(&expected)
    };

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
