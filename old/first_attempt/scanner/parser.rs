use std::{
    fmt::{Display, Write},
    ops::Deref,
};

use crate::{ParserError, ParserResult};

use super::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

// Grammar Rules
impl Parser {
    pub type Output = ParserResult<Expr>;

    pub fn parse(&mut self) -> Self::Output {
        self.expression()
    }

    fn expression(&mut self) -> Self::Output {
        self.equality()
    }

    fn equality(&mut self) -> Self::Output {
        let mut left = self.comparison()?;

        while self.matches_any(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.previous();
            let right = Box::new(self.comparison()?);
            left = Expr::Binary {
                operator,
                left: Box::new(left),
                right,
            };
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Self::Output {
        let mut left = self.term()?;

        while self.matches_any(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator = self.previous();
            let right = Box::new(self.term()?);
            left = Expr::Binary {
                operator,
                left: Box::new(left),
                right,
            };
        }

        Ok(left)
    }

    fn term(&mut self) -> Self::Output {
        let mut left = self.factor()?;

        while self.matches_any(&[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.previous();
            let right = Box::new(self.factor()?);
            left = Expr::Binary {
                operator,
                left: Box::new(left),
                right,
            };
        }

        Ok(left)
    }
    fn factor(&mut self) -> Self::Output {
        let mut left = self.unary()?;

        while self.matches_any(&[TokenKind::Slash, TokenKind::Star]) {
            let operator = self.previous();
            let right = Box::new(self.unary()?);
            left = Expr::Binary {
                operator,
                left: Box::new(left),
                right,
            };
        }

        Ok(left)
    }
    fn unary(&mut self) -> Self::Output {
        if self.matches_any(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.previous();
            let right = Box::new(self.unary()?);
            return Ok(Expr::Unary { operator, right });
        }

        self.primary()
    }

    fn primary(&mut self) -> Self::Output {
        let expr = if self.matches_any(&[TokenKind::False]) {
            Expr::Literal {
                value: LiteralValue::Bool(false),
            }
        } else if self.matches_any(&[TokenKind::True]) {
            Expr::Literal {
                value: LiteralValue::Bool(true),
            }
        } else if self.matches_any(&[TokenKind::Nil]) {
            Expr::Literal {
                value: LiteralValue::Nil,
            }
        } else if self.matches_any(&[TokenKind::Number(0.0, 0)]) {
            let token = self.previous();
            let TokenKind::Number(n, _) = token.kind else {
                return Err(ParserError::wrong_token(
                    &token,
                    "The token is not a number".to_owned(),
                ));
            };

            Expr::Literal {
                value: LiteralValue::Number(n),
            }
        } else if self.matches_any(&[TokenKind::String("".to_string())]) {
            let token = self.previous();
            let TokenKind::String(s) = token.kind else {
                return Err(ParserError::wrong_token(
                    &token,
                    "The token is not a string".to_owned(),
                ));
            };
            Expr::Literal {
                value: LiteralValue::String(s),
            }
        } else if self.matches_any(&[TokenKind::LeftParenthesis]) {
            let expr = self.expression()?;
            let _ = self.consume(
                TokenKind::RightParenthesis,
                "Expected ')' after expression.",
            );
            Expr::Grouping {
                expression: Box::new(expr),
            }
        } else {
            let token = self.peek();
            return Err(ParserError::wrong_token(
                token,
                format!(
                    "Provided primary expression not supported! Current token = {:?}",
                    token
                ),
            ));
        };

        Ok(expr)
    }
}

// Constructor and helper functions
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn matches_any(&mut self, types: &[TokenKind]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&mut self, t: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(t)
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::EOF)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> ParserResult<Token> {
        if self.check(&kind) {
            return Ok(self.advance());
        }

        Err(ParserError::wrong_token(self.peek(), message.to_string()))
    }

    // discards tokens until it thinks it has found a statement boundry
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // for now the only statement boundry is the semicolon
            if matches!(self.previous().kind, TokenKind::Semicolon) {
                return;
            }
            self.advance();
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

pub enum Expr {
    Literal {
        value: LiteralValue,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        operator: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
}

impl Expr {}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { value } => f.write_fmt(format_args!("{value}")),
            Expr::Unary { operator, right } => {
                f.write_fmt(format_args!("({} {})", operator.kind, right))
            }
            Expr::Binary {
                operator,
                left,
                right,
            } => f.write_fmt(format_args!("({} {} {})", operator.kind, left, right)),
            Expr::Grouping { expression } => f.write_fmt(format_args!("(group {})", expression)),
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Nil => f.write_fmt(format_args!("{}", TokenKind::Nil)),
            LiteralValue::Bool(b) => f.write_fmt(format_args!("{b}")),
            LiteralValue::Number(n) => f.write_fmt(format_args!("{n:?}")),
            LiteralValue::String(s) => f.write_fmt(format_args!("{s:?}")),
        }
    }
}

pub fn interpret(expr: &Expr) -> ParserResult<LiteralValue> {
    match expr {
        Expr::Literal { value } => Ok(value.clone()),
        Expr::Unary { operator, right } => {
            let right = interpret(right)?;
            match operator.kind {
                TokenKind::Minus => {
                    let n = check_number_operand(right)?;

                    return Ok(LiteralValue::Number(-n));
                }
                TokenKind::Bang => {
                    return Ok(LiteralValue::Bool(!is_truthy(right)));
                }
                _ => (),
            }
            Ok(LiteralValue::Nil)
        }
        Expr::Binary {
            operator,
            left,
            right,
        } => {
            let left = interpret(left)?;
            let right = interpret(right)?;

            match operator.kind {
                TokenKind::Minus => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Number(l - r));
                }
                TokenKind::Plus => {
                    // addition
                    check_two_number_or_string_operand(&left, &right)?;
                    if let LiteralValue::Number(l) = left
                        && let LiteralValue::Number(r) = right
                    {
                        return Ok(LiteralValue::Number(l + r));
                    };

                    // string concatenation
                    if let LiteralValue::String(mut l) = left
                        && let LiteralValue::String(r) = right
                    {
                        l.push_str(r.as_str());
                        return Ok(LiteralValue::String(l));
                    };
                }
                TokenKind::Slash => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Number(l / r));
                }
                TokenKind::Star => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Number(l * r));
                }
                TokenKind::Greater => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Bool(l > r));
                }
                TokenKind::GreaterEqual => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Bool(l >= r));
                }
                TokenKind::Less => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Bool(l < r));
                }
                TokenKind::LessEqual => {
                    let l = check_number_operand(left)?;
                    let r = check_number_operand(right)?;

                    return Ok(LiteralValue::Bool(l <= r));
                }
                TokenKind::BangEqual => return Ok(LiteralValue::Bool(!is_equal(left, right))),
                TokenKind::EqualEqual => return Ok(LiteralValue::Bool(is_equal(left, right))),
                _ => (),
            }

            Ok(LiteralValue::Nil)
        }
        Expr::Grouping { expression } => interpret(expression),
    }
}

fn check_two_number_or_string_operand(
    left: &LiteralValue,
    right: &LiteralValue,
) -> ParserResult<()> {
    match (left, right) {
        (LiteralValue::Number(_), LiteralValue::Number(_)) => todo!(),
        (LiteralValue::String(_), LiteralValue::String(_)) => todo!(),
        _ => Err(ParserError::interpreter_error(
            "Both operands must be two numbers or two strings.".to_owned(),
        )),
    }
}

fn check_number_operand(literal: LiteralValue) -> ParserResult<f64> {
    if let LiteralValue::Number(r) = literal {
        Ok(r)
    } else {
        Err(ParserError::interpreter_error(format!(
            "Operand {literal} must be a number."
        )))
    }
}

fn is_truthy(value: LiteralValue) -> bool {
    match value {
        LiteralValue::Nil => false,
        LiteralValue::Bool(b) => b,
        LiteralValue::Number(_) | LiteralValue::String(_) => true,
    }
}

fn is_equal(left: LiteralValue, right: LiteralValue) -> bool {
    match (left, right) {
        (LiteralValue::Nil, LiteralValue::Nil) => true,
        (LiteralValue::Nil, _) | (_, LiteralValue::Nil) => false,
        (LiteralValue::Number(_), LiteralValue::Bool(_))
        | (LiteralValue::Number(_), LiteralValue::String(_)) => false,
        (LiteralValue::Number(l), LiteralValue::Number(r)) => l.eq(&r),
        (LiteralValue::String(_), LiteralValue::Bool(_))
        | (LiteralValue::String(_), LiteralValue::Number(_)) => false,
        (LiteralValue::String(l), LiteralValue::String(r)) => l.eq(&r),
        (LiteralValue::Bool(_), LiteralValue::Number(_))
        | (LiteralValue::Bool(_), LiteralValue::String(_)) => false,
        (LiteralValue::Bool(l), LiteralValue::Bool(r)) => l == r,
    }
}
