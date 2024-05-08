use crate::tokenizer::{
    Token,
    TokenKind::{self, *},
    Tokenizer,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn parse(source: &str) -> SExpr {
        // FIXME: return correct error
        let mut tokens = Tokenizer::tokenize(source).unwrap();
        tokens.reverse();
        let mut parser = Parser { tokens, current: 0 };
        parser.expression_binding_power(0)
    }

    fn next(&mut self) -> Token {
        // panics if called after providing the last token, the EOF.
        debug_assert!(!self.tokens.is_empty());
        self.tokens.pop().unwrap()
    }

    fn peek(&self) -> Token {
        // panics if called after providing the last token, the EOF.
        debug_assert!(!self.tokens.is_empty());
        self.tokens.last().cloned().unwrap()
    }

    fn expression_binding_power(&mut self, min_binding_power: u8) -> SExpr {
        let token = self.next();
        let mut left_hand_side = match token.kind {
            Number(n, _) => SExpr::Number(n),
            _ => panic!("bad token {:?}", token),
        };

        loop {
            let token = self.peek();
            let operand = match token.kind {
                EOF => break,
                op => op,
            };

            let (left_binding_power, right_binding_power) = Self::infix_binding_power(&operand);

            if left_binding_power < min_binding_power {
                break;
            }

            self.next();
            let right_hand_side = self.expression_binding_power(right_binding_power);

            left_hand_side = SExpr::Cons(operand, vec![left_hand_side, right_hand_side]);
        }

        left_hand_side
    }

    fn infix_binding_power(op: &TokenKind) -> (u8, u8) {
        match op {
            Plus | Minus => (1, 2),
            Star | Slash => (3, 4),
            _ => panic!("bad op: {op:?}"),
        }
    }
}

use std::fmt;

pub enum SExpr {
    Number(f64),
    Cons(TokenKind, Vec<SExpr>),
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Number(n) => write!(f, "{}", n),
            SExpr::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    #[test]
    fn basic_expression() {
        let s = Parser::parse("1");
        assert_eq!(s.to_string(), "1");

        let s = Parser::parse("1 + 2 * 3");
        assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

        // let s = Parser::parse("a + b * c * d + e");
        // assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
    }
}
