use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Minus,
    Plus,
    Star,
    Slash,
    Equal,
    Greater,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Print,
    Const,
    Mut,
    For,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Operators
    Operator(Operator),
    // Literals
    Integer(i64),
    // Keywords
    Keyword(Keyword),
    // Others
    Identifier(String),
    Colon,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    // EOF
    EOF,
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Token::Identifier(id) => write!(f, "{id}"),
            TokenKind::Integer(n) => write!(fmt, "{n}"),
            TokenKind::Colon => write!(fmt, ":"),
            TokenKind::Semicolon => write!(fmt, ";"),
            TokenKind::LeftParenthesis => write!(fmt, "("),
            TokenKind::RightParenthesis => write!(fmt, ")"),
            TokenKind::EOF => write!(fmt, "EOF"),
            TokenKind::Operator(o) => o.fmt(fmt),
            TokenKind::Keyword(k) => k.fmt(fmt),
            TokenKind::Identifier(id) => write!(fmt, "'{id}'"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn location(&self) -> String {
        format!("[ line:{} ; col:{} ]", self.line, self.col)
    }
}

impl Display for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(fmt)
    }
}

impl Display for Keyword {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Return => write!(fmt, "return"),
            Keyword::Print => write!(fmt, "print"),
            Keyword::Const => write!(fmt, "const"),
            Keyword::Mut => write!(fmt, "mut"),
            Keyword::For => write!(fmt, "for"),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Minus => write!(f, "-"),
            Operator::Plus => write!(f, "+"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Equal => write!(f, "="),
            Operator::Greater => write!(f, ">"),
        }
    }
}
