use std::{error::Error, fmt::Display};

use crate::{
    environment::Environment,
    parser::{ASTNode, Atom, Parser},
    tokenizer::TokenKind,
};

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub fn run_file(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(file_path)?;

    let mut interpreter = Interpreter::new();
    run(&mut interpreter, &file_content)
}
pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    use std::io::{stdin, stdout, Write};

    let mut interpreter = Interpreter::new();
    loop {
        let mut input = String::new();
        print!("> ");
        let _ = stdout().flush();
        stdin().read_line(&mut input)?;

        if let Some('\n') = input.chars().next_back() {
            input.pop();
        }
        if let Some('\r') = input.chars().next_back() {
            input.pop();
        }

        if input.eq("exit") {
            break;
        }
        run(&mut interpreter, &input)?;
    }

    Ok(())
}

fn run(interpreter: &mut Interpreter, source: &str) -> Result<(), Box<dyn Error>> {
    match interpreter.interpret(source) {
        Ok(InterpreterValue::Nothing) => {}
        Ok(value) => {
            println!("{value:?}");
        }
        Err(err) => {
            println!("{}", err);
        }
    }

    Ok(())
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(None),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpreterResult<InterpreterValue> {
        let stmts = Parser::parse(source);
        // stmts.iter().for_each(|s| println!("{s}"));

        let mut res = InterpreterValue::Nothing;

        for stmt in stmts {
            res = self.evaluate_stmt(&stmt)?;
        }

        Ok(res)
    }

    fn evaluate_stmt(&mut self, stmts: &ASTNode) -> InterpreterResult<InterpreterValue> {
        let val = match stmts {
            ASTNode::Atom(a) => match a {
                crate::parser::Atom::Number(n) => InterpreterValue::Number(*n),
                crate::parser::Atom::Identifier(id) => self.env.retrieve(id)?,
                crate::parser::Atom::Str(s) => InterpreterValue::Str(s.clone()),
                crate::parser::Atom::Nil => InterpreterValue::Nothing,
                crate::parser::Atom::Boolean(b) => InterpreterValue::Bool(*b),
            },
            ASTNode::Cons(op, stmts) => self.compute_operand(op, stmts)?,
        };

        Ok(val)
    }

    fn compute_operand(
        &mut self,
        op: &TokenKind,
        stmts: &[ASTNode],
    ) -> InterpreterResult<InterpreterValue> {
        match op {
            TokenKind::LeftBrace => {
                // we are inside a code block
                let inner_env = Environment::new(Some(&self.env));
                let mut inner_intrp = Interpreter { env: inner_env };
                let mut value = InterpreterValue::Nothing;
                for stmt in stmts {
                    value = inner_intrp.evaluate_stmt(stmt)?;
                }

                return Ok(value);
            }
            TokenKind::If => {
                // check condition
                let condition = self.evaluate_stmt(&stmts[0])?;
                if self.check_bool(condition)? {
                    return self.evaluate_stmt(&stmts[1]);
                } else if stmts.len() == 3 {
                    return self.evaluate_stmt(&stmts[2]);
                } else {
                    return Ok(InterpreterValue::Nothing);
                };
            }
            TokenKind::While => {
                // check condition
                while let condition = self.evaluate_stmt(&stmts[0])?
                    && self.check_bool(condition)?
                {
                    for (i, stmt) in stmts.iter().enumerate() {
                        if i == 0 {
                            continue;
                        }
                        let _ = self.evaluate_stmt(stmt)?;
                    }
                }
                return Ok(InterpreterValue::Nothing);
            }
            _ => (),
        };

        // TODO: check stmts len after matching?
        if stmts.len() == 1 {
            return match op {
                TokenKind::Minus => {
                    let n = self.operation_with_one_number(stmts)?;
                    Ok(InterpreterValue::Number(-n))
                }
                TokenKind::Bang => {
                    let b = self.operation_with_one_bool(stmts)?;
                    Ok(InterpreterValue::Bool(!b))
                }
                TokenKind::Print => {
                    let s = self.stringify(stmts)?;
                    println!("{s}");
                    Ok(InterpreterValue::Nothing)
                }
                TokenKind::Semicolon => {
                    let _ = self.operation_with_one_operand(stmts)?;
                    // statements end in semicolon, so they do not produce a value
                    Ok(InterpreterValue::Nothing)
                }
                TokenKind::Else => self.operation_with_one_operand(stmts),
                _ => panic!("Operation '{op}' not implemented!"),
            };
        }

        if stmts.len() == 2 {
            return match op {
                TokenKind::Minus => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Number(l - r))
                }
                TokenKind::Plus => {
                    // addition
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Number(l + r))
                }
                TokenKind::PlusPlus => {
                    // addition
                    let (mut l, r) = self.operation_with_two_strings(stmts)?;
                    l.push_str(r.as_str());
                    Ok(InterpreterValue::Str(l))
                }
                TokenKind::Slash => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Number(l / r))
                }
                TokenKind::Star => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Number(l * r))
                }
                TokenKind::Greater => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Bool(l > r))
                }
                TokenKind::GreaterEqual => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Bool(l >= r))
                }
                TokenKind::Less => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Bool(l < r))
                }
                TokenKind::LessEqual => {
                    let (l, r) = self.operation_with_two_numbers(stmts)?;
                    Ok(InterpreterValue::Bool(l <= r))
                }
                TokenKind::BangEqual => {
                    Ok(InterpreterValue::Bool(!self.are_two_operands_equal(stmts)?))
                }
                TokenKind::EqualEqual => {
                    Ok(InterpreterValue::Bool(self.are_two_operands_equal(stmts)?))
                }
                TokenKind::Equal => {
                    // check identifier
                    let ASTNode::Atom(Atom::Identifier(identifier)) = &stmts[0] else {
                        return Err(InterpreterError::Generic(
                            "Left operand is not an Identifier".to_string(),
                        ));
                    };
                    let value = self.evaluate_stmt(&stmts[1])?;
                    self.env.assign(identifier.clone(), value.clone());

                    Ok(value)
                }
                TokenKind::And => {
                    let l = self.evaluate_stmt(&stmts[0])?;
                    if self.check_bool(l)? {
                        let r = self.evaluate_stmt(&stmts[1])?;
                        let r = self.check_bool(r)?;
                        Ok(InterpreterValue::Bool(r))
                    } else {
                        Ok(InterpreterValue::Bool(false))
                    }
                }
                TokenKind::Or => {
                    let l = self.evaluate_stmt(&stmts[0])?;
                    if !self.check_bool(l)? {
                        let r = self.evaluate_stmt(&stmts[1])?;
                        let r = self.check_bool(r)?;
                        Ok(InterpreterValue::Bool(r))
                    } else {
                        Ok(InterpreterValue::Bool(true))
                    }
                }
                _ => panic!("Operation '{op}' not implemented!"),
            };
        }

        panic!("Operation '{op}' not implemented!");
    }

    // --- STATEMENTS ---
    fn operation_with_one_operand(
        &mut self,
        stmts: &[ASTNode],
    ) -> Result<InterpreterValue, InterpreterError> {
        assert_eq!(stmts.len(), 1);
        let value = self.evaluate_stmt(&stmts[0])?;
        Ok(value)
    }
    // --- STATEMENTS ---

    // --- NUMBERS ---
    fn operation_with_one_number(&mut self, stmts: &[ASTNode]) -> Result<f64, InterpreterError> {
        assert_eq!(stmts.len(), 1);
        let n = self.evaluate_stmt(&stmts[0])?;
        let n = self.check_number(n)?;
        Ok(n)
    }

    fn operation_with_two_numbers(
        &mut self,
        stmts: &[ASTNode],
    ) -> Result<(f64, f64), InterpreterError> {
        assert_eq!(stmts.len(), 2);
        let l = self.evaluate_stmt(&stmts[0])?;
        let r = self.evaluate_stmt(&stmts[1])?;
        let (l, r) = self.check_two_numbers(l, r)?;
        Ok((l, r))
    }

    fn check_number(&mut self, n: InterpreterValue) -> InterpreterResult<f64> {
        let InterpreterValue::Number(n) = n else {
            return Err(InterpreterError::Generic(
                "Operand is not a Number".to_string(),
            ));
        };

        Ok(n)
    }

    fn check_two_numbers(
        &mut self,
        l: InterpreterValue,
        r: InterpreterValue,
    ) -> InterpreterResult<(f64, f64)> {
        let InterpreterValue::Number(l) = l else {
            return Err(InterpreterError::Generic(
                "Left operand is not a Number".to_string(),
            ));
        };
        let InterpreterValue::Number(r) = r else {
            return Err(InterpreterError::Generic(
                "Right operand is not a Number".to_string(),
            ));
        };

        Ok((l, r))
    }
    // --- NUMBERS ---

    // --- STRINGS ---
    fn stringify(&mut self, stmts: &[ASTNode]) -> Result<String, InterpreterError> {
        assert_eq!(stmts.len(), 1);
        let s = self.evaluate_stmt(&stmts[0])?;
        Ok(s.to_string())
    }

    fn operation_with_two_strings(
        &mut self,
        stmts: &[ASTNode],
    ) -> Result<(String, String), InterpreterError> {
        assert_eq!(stmts.len(), 2);
        let l = self.evaluate_stmt(&stmts[0])?;
        let r = self.evaluate_stmt(&stmts[1])?;
        let (l, r) = self.check_two_strings(l, r)?;
        Ok((l, r))
    }

    fn check_two_strings(
        &mut self,
        l: InterpreterValue,
        r: InterpreterValue,
    ) -> InterpreterResult<(String, String)> {
        let InterpreterValue::Str(l) = l else {
            return Err(InterpreterError::Generic(
                "Left operand is not a String".to_string(),
            ));
        };
        let InterpreterValue::Str(r) = r else {
            return Err(InterpreterError::Generic(
                "Right operand is not a String".to_string(),
            ));
        };

        Ok((l, r))
    }
    // --- STRINGS ---

    // --- BOOLS ---
    fn check_bool(&mut self, b: InterpreterValue) -> InterpreterResult<bool> {
        let InterpreterValue::Bool(b) = b else {
            return Err(InterpreterError::Generic(
                "Operand is not a Bool".to_string(),
            ));
        };

        Ok(b)
    }

    fn operation_with_one_bool(&mut self, stmts: &[ASTNode]) -> Result<bool, InterpreterError> {
        assert_eq!(stmts.len(), 1);
        let b = self.evaluate_stmt(&stmts[0])?;
        let b = self.check_bool(b)?;
        Ok(b)
    }

    fn are_two_operands_equal(&mut self, stmts: &[ASTNode]) -> Result<bool, InterpreterError> {
        assert_eq!(stmts.len(), 2);
        let l = self.evaluate_stmt(&stmts[0])?;
        let r = self.evaluate_stmt(&stmts[1])?;

        Ok(match (l, r) {
            (InterpreterValue::Nothing, InterpreterValue::Nothing) => true,
            (InterpreterValue::Bool(l), InterpreterValue::Bool(r)) => l.eq(&r),
            (InterpreterValue::Number(l), InterpreterValue::Number(r)) => l.eq(&r),
            (InterpreterValue::Str(l), InterpreterValue::Str(r)) => l.eq(&r),
            _ => false,
        })
    }

    // --- BOOLS ---
}

#[derive(Debug, Clone)]
pub enum InterpreterValue {
    Nothing,
    Bool(bool),
    Number(f64),
    Str(String),
    Identifier(String),
}

impl Display for InterpreterValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterValue::Nothing => write!(f, ""),
            InterpreterValue::Bool(b) => write!(f, "{}", b),
            InterpreterValue::Number(n) => write!(f, "{}", n),
            InterpreterValue::Str(s) => write!(f, "{:?}", s),
            InterpreterValue::Identifier(id) => write!(f, "{:?}", id),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum InterpreterError {
    Generic(String),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::Generic(s) => write!(f, "{}", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::InterpreterResult;
    use crate::interpreter::Interpreter;

    #[test]
    fn literal_evaluations() -> InterpreterResult<()> {
        let i = Interpreter::new().interpret("1 + 2 * 3 / 1.0 + (3 * (--1-1))")?;
        assert_eq!(i.to_string(), "7");

        let i = Interpreter::new().interpret(r#""a"++"b""#)?;
        assert_eq!(i.to_string(), r#""ab""#);

        let i = Interpreter::new().interpret("!(4 >= 5 * 3)")?;
        assert_eq!(i.to_string(), "true");

        let i = Interpreter::new().interpret("!!(4 == 5 * 3)")?;
        assert_eq!(i.to_string(), "false");

        let i = Interpreter::new().interpret(r#"2 * (3 / -"muffin");"#);
        assert!(i.is_err());

        let i = Interpreter::new().interpret(r#"print "muffin";"#)?;
        assert_eq!(i.to_string(), "");

        let i = Interpreter::new().interpret(
            r#"
            a = "global a";
            b = "global b";
            c = "global c";
            {
              a = "outer a";
              b = "outer b";
              {
                a = "inner a";
                print a;
                print b;
                print c;
              }
              print a;
              print b;
              print c;
            }
            print a;
            print b;
            print c;
            "#,
        )?;
        assert_eq!(i.to_string(), "");

        Ok(())
    }
}
