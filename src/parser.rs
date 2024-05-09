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
        let mut left_hand_side = self.sexpr_from_token(token);

        loop {
            let token = self.peek();
            let operand = match token.kind {
                EOF => break,
                op => op,
            };

            if let Some((left_bind_power, ())) = Self::postfix_binding_power(&operand) {
                if left_bind_power < min_binding_power {
                    break;
                }
                self.next();

                left_hand_side = if matches!(operand, TokenKind::LeftBracket) {
                    let right_hand_side = self.expression_binding_power(0);
                    assert_eq!(self.next().kind, TokenKind::RightBracket);
                    SExpr::Cons(operand, vec![left_hand_side, right_hand_side])
                } else {
                    SExpr::Cons(operand, vec![left_hand_side])
                };
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) =
                Self::infix_binding_power(&operand)
            {
                if left_binding_power < min_binding_power {
                    break;
                }

                self.next();
                left_hand_side = if matches!(operand, TokenKind::Question) {
                    let middle_hand_side = self.expression_binding_power(0);
                    assert_eq!(self.next().kind, TokenKind::Colon);
                    let right_hand_side = self.expression_binding_power(right_binding_power);
                    SExpr::Cons(
                        operand,
                        vec![left_hand_side, middle_hand_side, right_hand_side],
                    )
                } else {
                    let right_hand_side = self.expression_binding_power(right_binding_power);
                    SExpr::Cons(operand, vec![left_hand_side, right_hand_side])
                };
                continue;
            }

            break;
        }

        left_hand_side
    }

    fn postfix_binding_power(op: &TokenKind) -> Option<(u8, ())> {
        let res = match op {
            Bang | LeftBracket => (11, ()),
            _ => return None,
        };
        Some(res)
    }

    fn prefix_binding_power(op: &TokenKind) -> ((), u8) {
        match op {
            Plus | Minus => ((), 9),
            _ => panic!("bad prefix op: {op:?}"),
        }
    }

    fn infix_binding_power(op: &TokenKind) -> Option<(u8, u8)> {
        // as a general rule we use and odd number for the bare priority and bump it up by one for associativity
        let res = match op {
            Equal => (2, 1),
            Question => (4, 3),
            Plus | Minus => (5, 6),
            Star | Slash => (7, 8),
            Dot => (14, 13),
            _ => return None,
        };
        Some(res)
    }

    pub fn sexpr_from_token(&mut self, token: Token) -> SExpr {
        match token.kind {
            // Atoms
            Identifier(i) => SExpr::Atom(Atom::Identifier(i)),
            Number(n) => SExpr::Atom(Atom::Number(n)),
            Str(_) => todo!(),

            // Operands
            Minus | Plus | Slash | Star | Dot | Question => {
                let (_, right_binding_power) = Parser::prefix_binding_power(&token.kind);
                let right_hand_side = self.expression_binding_power(right_binding_power);
                SExpr::Cons(token.kind, vec![right_hand_side])
            }
            // Parenthesis
            LeftParenthesis => {
                let left_hand_side = self.expression_binding_power(0);
                assert_eq!(self.next().kind, TokenKind::RightParenthesis);
                left_hand_side
            }
            // Others
            Ampersand => todo!(),
            RightParenthesis => todo!(),
            LeftBrace => todo!(),
            RightBrace => todo!(),
            LeftBracket => todo!(),
            RightBracket => todo!(),
            Comma => todo!(),
            Semicolon => todo!(),
            Colon => todo!(),
            Bang => todo!(),
            BangEqual => todo!(),
            Equal => todo!(),
            EqualEqual => todo!(),
            Greater => todo!(),
            GreaterEqual => todo!(),
            Less => todo!(),
            LessEqual => todo!(),
            And => todo!(),
            Else => todo!(),
            False => todo!(),
            Fn => todo!(),
            For => todo!(),
            If => todo!(),
            Nil => todo!(),
            Or => todo!(),
            Print => todo!(),
            Return => todo!(),
            True => todo!(),
            While => todo!(),
            EOF => todo!(),
        }
    }
}

use std::fmt;

pub enum Atom {
    Number(f64),
    Identifier(String),
}

pub enum SExpr {
    Atom(Atom),
    Cons(TokenKind, Vec<SExpr>),
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Atom(a) => match a {
                Atom::Number(n) => write!(f, "{}", n),
                Atom::Identifier(i) => write!(f, "{}", i),
            },
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

        let s = Parser::parse("a + b * c * d + e");
        assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");

        let s = Parser::parse("--1 * 2");
        assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

        let s = Parser::parse("--f . g");
        assert_eq!(s.to_string(), "(- (- (. f g)))");

        let s = Parser::parse("-9!");
        assert_eq!(s.to_string(), "(- (! 9))");

        let s = Parser::parse("f . g !");
        assert_eq!(s.to_string(), "(! (. f g))");

        let s = Parser::parse("(((0)))");
        assert_eq!(s.to_string(), "0");

        let s = Parser::parse("x[0][1]");
        assert_eq!(s.to_string(), "([ ([ x 0) 1)");

        let s = Parser::parse(
            "a ? b :
         c ? d
         : e",
        );
        assert_eq!(s.to_string(), "(? a b (? c d e))");

        let s = Parser::parse("a = 0 ? b : c = d");
        assert_eq!(s.to_string(), "(= a (= (? 0 b c) d))")
    }
}
