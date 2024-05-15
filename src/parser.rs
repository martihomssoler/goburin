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
    pub fn parse(source: &str) -> Vec<ASTNode> {
        // FIXME: return correct error
        let mut tokens = Tokenizer::tokenize(source).unwrap();
        tokens.reverse();
        let mut parser = Parser { tokens, current: 0 };

        parser.statements(None)
    }

    fn statements(&mut self, stop_token_opt: Option<TokenKind>) -> Vec<ASTNode> {
        let mut stmts = Vec::new();
        loop {
            let stmt = match self.peek().kind {
                stop if let Some(stop_token) = &stop_token_opt
                    && stop.eq(stop_token) =>
                {
                    self.next();
                    return stmts;
                }

                EOF => return stmts,
                Print => {
                    self.next();
                    let expr = self.expression_binding_power(0);
                    assert_eq!(self.next().kind, TokenKind::Semicolon);
                    ASTNode::Cons(Semicolon, vec![ASTNode::Cons(Print, vec![expr])])
                }
                Equal => {
                    // assignment
                    self.next();
                    let id_token = self.next();
                    let identifier = if let TokenKind::Identifier(identifier) = id_token.kind {
                        ASTNode::Atom(Atom::Identifier(identifier))
                    } else {
                        println!("Error: Expected Identifier");
                        return stmts;
                    };
                    let value = self.expression_binding_power(0);
                    ASTNode::Cons(Equal, vec![identifier, value])
                }
                Semicolon => {
                    self.next();
                    // let expr = self.expression_binding_power(0);
                    // let TokenKind::Semicolon = self.peek().kind else {
                    //     // TODO: send missing semicolon at the end of statement error
                    //     stmts.push(ASTNode::Atom(Atom::Nil));
                    //     return stmts;
                    // };
                    // ASTNode::Cons(Semicolon, vec![expr])

                    continue;
                }
                Var => {
                    self.next();
                    let value = self.expression_binding_power(0);
                    assert!(matches!(value, ASTNode::Cons(Equal, _)));
                    ASTNode::Cons(Var, vec![value])
                }
                // Control Flow
                If => {
                    self.next();
                    self.consume(LeftParenthesis);
                    let condition = self.expression_binding_power(0);
                    self.consume(RightParenthesis);
                    let if_branch = if self.peek().kind == LeftBrace {
                        self.next();
                        self.statements(Some(RightBrace))
                    } else {
                        self.statements(None)
                    };

                    let mut body = vec![condition, ASTNode::Cons(LeftBrace, if_branch)];
                    // let's check if there is an "else" clause
                    if self.peek().kind == Else {
                        self.next();
                        body.push(ASTNode::Cons(Else, self.statements(None)));
                    }

                    ASTNode::Cons(If, body)
                }
                While => {
                    self.next();
                    self.consume(LeftParenthesis);
                    let condition = self.expression_binding_power(0);
                    self.consume(RightParenthesis);
                    let mut while_body = if self.peek().kind == LeftBrace {
                        self.next();
                        self.statements(Some(RightBrace))
                    } else {
                        self.statements(None)
                    };
                    let mut condition_and_body = vec![condition];
                    condition_and_body.append(&mut while_body);

                    ASTNode::Cons(While, condition_and_body)
                }
                // Others
                _ => {
                    let expr = self.expression_binding_power(0);
                    if let TokenKind::Semicolon = self.peek().kind {
                        self.next();
                        ASTNode::Cons(Semicolon, vec![expr])
                    } else {
                        expr
                    }
                }
            };
            stmts.push(stmt);
        }
    }

    fn consume(&mut self, expected: TokenKind) {
        let actual = self.peek().kind;
        if actual == expected {
            self.next();
        } else {
            panic!("Expected '{expected}' but got '{actual}'");
        }
    }

    fn expression_binding_power(&mut self, min_binding_power: u8) -> ASTNode {
        let left_token = self.next();
        let mut left_hand_side = self.node_from_token(left_token.clone());

        loop {
            let right_token = self.peek();
            let operand = match right_token.kind {
                EOF | Semicolon => {
                    break;
                }
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
                    ASTNode::Cons(operand, vec![left_hand_side, right_hand_side])
                } else {
                    ASTNode::Cons(operand, vec![left_hand_side])
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
                    ASTNode::Cons(
                        operand,
                        vec![left_hand_side, middle_hand_side, right_hand_side],
                    )
                } else {
                    let right_hand_side = self.expression_binding_power(right_binding_power);
                    ASTNode::Cons(operand, vec![left_hand_side, right_hand_side])
                };
                continue;
            }

            break;
        }

        left_hand_side
    }

    // TODO: make a single function for the binding powers
    fn postfix_binding_power(op: &TokenKind) -> Option<(u8, ())> {
        let res = match op {
            LeftBracket => (15, ()),
            _ => return None,
        };
        Some(res)
    }

    fn prefix_binding_power(op: &TokenKind) -> ((), u8) {
        match op {
            Bang => ((), 15),
            Plus | Minus => ((), 13),
            _ => panic!("bad prefix op: {op:?}"),
        }
    }

    fn infix_binding_power(op: &TokenKind) -> Option<(u8, u8)> {
        // as a general rule we use and odd number for the bare priority and bump it up by one for associativity
        let res = match op {
            Equal => (2, 1),
            Or => (4, 3),
            And => (6, 5),
            BangEqual | EqualEqual | Greater | GreaterEqual | Less | LessEqual | Question => (8, 7),
            Plus | PlusPlus | Minus => (9, 10),
            Star | Slash => (11, 12),
            Dot => (18, 17),
            _ => return None,
        };
        Some(res)
    }

    pub fn node_from_token(&mut self, token: Token) -> ASTNode {
        match token.kind {
            // Atoms
            Identifier(i) => ASTNode::Atom(Atom::Identifier(i)),
            Number(n) => ASTNode::Atom(Atom::Number(n)),
            Str(s) => ASTNode::Atom(Atom::Str(s)),

            // Operands
            Minus | Plus | PlusPlus | Slash | Star | Dot | Question => {
                let (_, right_binding_power) = Parser::prefix_binding_power(&token.kind);
                let right_hand_side = self.expression_binding_power(right_binding_power);
                ASTNode::Cons(token.kind, vec![right_hand_side])
            }
            // Parenthesis
            LeftParenthesis => {
                let left_hand_side = self.expression_binding_power(0);
                assert_eq!(self.next().kind, TokenKind::RightParenthesis);
                left_hand_side
            }
            // Booleans
            True => ASTNode::Atom(Atom::Boolean(true)),
            False => ASTNode::Atom(Atom::Boolean(false)),
            Bang => {
                let (_, right_binding_power) = Parser::prefix_binding_power(&token.kind);
                let right_hand_side = self.expression_binding_power(right_binding_power);
                ASTNode::Cons(token.kind, vec![right_hand_side])
            }
            // Others
            Print => todo!(),
            Ampersand => todo!(),
            RightParenthesis => todo!(),
            LeftBrace => {
                let content = self.statements(Some(RightBrace));
                ASTNode::Cons(token.kind, content)
            }
            RightBrace => todo!(),
            LeftBracket => todo!(),
            RightBracket => todo!(),
            Comma => todo!(),
            Semicolon => todo!(),
            Colon => todo!(),
            BangEqual => todo!(),
            Equal => todo!(),
            EqualEqual => todo!(),
            Greater => todo!(),
            GreaterEqual => todo!(),
            Less => todo!(),
            LessEqual => todo!(),
            And => todo!(),
            Else => todo!(),
            Fn => todo!(),
            For => todo!(),
            If => todo!(),
            Nil => todo!(),
            Or => todo!(),
            Return => todo!(),
            While => todo!(),
            EOF => todo!(),
            Var => todo!(),
        }
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
}

use std::fmt;

pub enum Atom {
    Nil,
    Number(f64),
    Str(String),
    Identifier(String),
    Boolean(bool),
}

pub enum ASTNode {
    Atom(Atom),
    Cons(TokenKind, Vec<ASTNode>),
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Atom(a) => match a {
                Atom::Nil => write!(f, ""),
                Atom::Number(n) => write!(f, "{}", n),
                Atom::Identifier(i) => write!(f, "{}", i),
                Atom::Str(s) => write!(f, "{:?}", s),
                Atom::Boolean(b) => write!(f, "{}", b),
            },
            ASTNode::Cons(head, rest) => {
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
    fn basic_expressions() {
        let s = Parser::parse("1;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; 1)");

        let s = Parser::parse("1");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "1");

        let s = Parser::parse("1 + 2 * 3;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (+ 1 (* 2 3)))");

        let s = Parser::parse("a + b * c * d + e;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (+ (+ a (* (* b c) d)) e))");

        let s = Parser::parse("--1 * 2;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (* (- (- 1)) 2))");

        let s = Parser::parse(r#""a"++"b";"#);
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), r#"(; (++ "a" "b"))"#);

        let s = Parser::parse("--1-2;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (- (- (- 1)) 2))");

        let s = Parser::parse("(1 + 2 * 3) / 1.0 + (3 * (--1-1));");
        assert_eq!(s.len(), 1);
        assert_eq!(
            s[0].to_string(),
            "(; (+ (/ (+ 1 (* 2 3)) 1) (* 3 (- (- (- 1)) 1))))"
        );

        let s = Parser::parse("--f . g;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (- (- (. f g))))");

        let s = Parser::parse("! f . g;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (! (. f g)))");

        let s = Parser::parse("(((0)));");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; 0)");

        let s = Parser::parse("x[0][1];");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; ([ ([ x 0) 1))");

        let s = Parser::parse(
            "a ? b :
         c ? d
         : e;",
        );
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (? a b (? c d e)))");

        let s = Parser::parse("a = 0 ? b : c = d;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (= a (= (? 0 b c) d)))");

        let s = Parser::parse("!(4 >= 5 * 3);");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (! (>= 4 (* 5 3))))");

        let s = Parser::parse(r#"print "muffin";"#);
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), r#"(; (print "muffin"))"#);

        let s = Parser::parse("x=  1;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (= x 1))");

        let s = Parser::parse("11 =-1*4+2;");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "(; (= 11 (+ (* (- 1) 4) 2)))");

        let s = Parser::parse("{x=1;print x;}");
        assert_eq!(s.len(), 1);
        assert_eq!(s[0].to_string(), "({ (; (= x 1)) (; (print x)))");

        let s = Parser::parse(
            r#"
                a = 1;
                while (a < 2) {
                  print a;
                  a = a + 1;
                } 
            "#,
        );
        assert_eq!(s.len(), 2);
        assert_eq!(s[0].to_string(), "(; (= a 1))");
        assert_eq!(
            s[1].to_string(),
            "(while (< a 2) (; (print a)) (; (= a (+ a 1))))"
        );
    }
}
