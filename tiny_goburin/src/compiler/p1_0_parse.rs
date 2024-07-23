use super::*;
use std::os::unix::fs::FileTypeExt;

impl TokenList {
    pub fn p1_0_parse(self) -> Result<Ast, String> {
        for t in &self.tokens {
            print!("{t}\t");
        }
        println!();
        p_parse(self.tokens)
    }
}

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
                    "[parse_program] Unexpected token {:?} at line {} and col {}.\n\tTokens: {:?}",
                    token.kind,
                    token.line,
                    token.col,
                    tokens
                        .iter()
                        .skip(idx - 3)
                        .take(6)
                        .map(|t| t.kind.to_string())
                        .collect::<Vec<_>>()
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
    // open braces
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceLeft))?;
    // parse statements
    let mut statements = Vec::new();
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
            Keyword::For => parse_statement_for(tokens, idx)?,
            _ => todo!("{kw:?}"),
        },
        TokenKind::Value(v) => match v {
            Value::Constant(c) => {
                let stmt = Statement::Return(parse_expression(tokens, idx)?);
                consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
                stmt
            }
            Value::Identifier(id) => {
                // is the statement an assignment? then there must be an equal sign.
                if consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Equal)).is_ok() {
                    let stmt = Statement::Assignment({
                        let name = id.0.clone();
                        let expr = parse_expression(tokens, idx)?;
                        Assignment { name, val: expr }
                    });
                    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
                    stmt
                } else {
                    Statement::Declaration(parse_declaration(id.0.clone(), tokens, idx)?)
                }
            }
        },
        TokenKind::Eof => return Err("Encountered EOF while parsing a statement.".to_owned()),
        _ => return Err(format!("[parse_statement] Unexpected token {token_kind:?}.")),
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

fn parse_statement_for(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Statement, String> {
    // for (init; cond; next) { body }
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::ParenthesisLeft))?;
    let init_expr = parse_expression(tokens, idx)?;
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
    let control_expr = parse_expression(tokens, idx)?;
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::Semicolon))?;
    let next_expr = parse_expression(tokens, idx)?;
    consume(
        true,
        &tokens[*idx],
        idx,
        TokenKind::Operator(Operator::ParenthesisRight),
    )?;
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceLeft))?;
    let mut statements = Vec::new();
    while *idx < tokens.len()
        && !matches!(
            tokens[*idx].kind,
            TokenKind::Operator(Operator::BraceRight) | TokenKind::Eof
        )
    {
        let statement = parse_statement(tokens, idx)?;
        statements.push(statement);
    }
    consume(true, &tokens[*idx], idx, TokenKind::Operator(Operator::BraceRight))?;
    Ok(Statement::Loop(Loop {
        init_expr_opt: Some(init_expr),
        control_expr_opt: Some(control_expr),
        next_expr_opt: Some(next_expr),
        loop_body: statements,
    }))
}

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

fn parse_expression(tokens: &[Token<TokenKind>], idx: &mut usize) -> Result<Expression, String> {
    // let val = consume_value(tokens, idx)?;
    // Ok(from_token_value_to_expression(val))
    parse_expression_bindpow(tokens, idx, 0)
}

fn parse_expression_bindpow(tokens: &[Token<TokenKind>], idx: &mut usize, bind_pow: u8) -> Result<Expression, String> {
    let t = &tokens[*idx];

    let mut left = match &t.kind {
        // TokenKind::Operator(op) if prefix_bind_pow(&op).is_some() => {
        //     let (_, right_bind_pow) = prefix_bind_pow(&op).unwrap();
        //     let expr = parse_expression_bindpow(tokens, idx, right_bind_pow)?;
        //     Expression::Operation(op, vec![expr])
        // }
        // TokenKind::LeftParenthesis => {
        //     let expr = parse_expr(token_iter, 0)?;
        //     consume(token_iter, TokenKind::RightParenthesis)?;
        //     Expr::Parenthesis(Box::new(expr))
        // }
        TokenKind::Value(_) => {
            let val = consume_value(tokens, idx)?;
            from_token_value_to_expression(val)
        }
        TokenKind::Keyword(_) | TokenKind::Type(_) | TokenKind::Eof => todo!(),
        TokenKind::Operator(op) => todo!(),
    };

    while *idx < tokens.len() {
        let op = match &tokens[*idx].kind {
            TokenKind::Operator(Operator::Comma | Operator::Semicolon | Operator::ParenthesisRight) => break,
            TokenKind::Operator(op) => op,
            t => {
                todo!("{t} : {:?}", tokens.iter().skip(*idx - 2).take(4).collect::<Vec<_>>())
            }
        };

        if let Some((left_bind_pow, _)) = postfix_bind_pow(op) {
            if left_bind_pow < bind_pow {
                break;
            }
            continue;
        }
        if let Some((left_bind_pow, right_bind_pow)) = infix_bind_pow(op) {
            if (left_bind_pow < bind_pow) {
                break;
            }

            *idx += 1;
            let right = parse_expression_bindpow(tokens, idx, right_bind_pow)?;
            left = Expression {
                name: "mock".to_owned(),
                kind: match op {
                    Operator::Equal => ExpressionKind::BinaryOpAssignment,
                    Operator::Plus => ExpressionKind::BinaryOpAdd,
                    Operator::Lower => ExpressionKind::BinaryOpLower,
                    _ => todo!("{op:?}"),
                },
                node: AstNode {
                    token: tokens[*idx].clone(),
                    symbol: None,
                    reg: u32::MAX,
                },
                left: Some(Box::new(left)),
                right: Some(Box::new(right)),
            };
            continue;
        }

        break;
    }

    Ok(left)
}

/// TODO(mhs): shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
/// and adapted to our needs with the following rule in mind
/// we use and odd number for the bare priority and bump it up by one for associativity
/// Bind Power      Operator
///     15          unary operations
///     13          *   /   %   %%   &   &~  <<   >>
///     11          +   -   |   ~    in  not_in
///      9          ==  !=  <   >    <=  >=
///      7          &&
///      5          ||
///      3          ..=    ..<
///      1          or_else  =  ?    if  when

// TODO: make a single function for the binding powers
fn postfix_bind_pow(op: &Operator) -> Option<(u8, ())> {
    None
}

fn prefix_bind_pow(op: &Operator) -> Option<((), u8)> {
    let res = match op {
        Operator::Plus | Operator::Minus => ((), 15),
        Operator::Equal => return None,
        _ => panic!("bad prefix op: {op:?}"),
    };

    Some(res)
}

fn infix_bind_pow(op: &Operator) -> Option<(u8, u8)> {
    let res = match op {
        Operator::Plus | Operator::Minus => (11, 12),
        Operator::Star | Operator::Slash => (13, 14),
        Operator::Greater | Operator::Lower => (9, 10),
        Operator::Equal => (1, 2), // TODO(mhs): is equal right associative?
        _ => panic!("bad prefix op: {op:?}"),
    };
    Some(res)
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
            name: id.0.to_string(),
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
