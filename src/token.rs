use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ampersand,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(f64, usize),
    // keywords
    And,
    Else,
    False,
    Fn,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    True,
    While,
    // misc
    #[allow(clippy::upper_case_acronyms)]
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize, line: usize) -> Self {
        Self {
            kind,
            start,
            end,
            line,
        }
    }
    pub fn len(&self) -> usize {
        match &self.kind {
            // Token w/ length 1
            TokenKind::Ampersand
            | TokenKind::LeftParenthesis
            | TokenKind::RightParenthesis
            | TokenKind::LeftBrace
            | TokenKind::RightBrace
            | TokenKind::LeftBracket
            | TokenKind::RightBracket
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::Minus
            | TokenKind::Plus
            | TokenKind::Semicolon
            | TokenKind::Colon
            | TokenKind::Slash
            | TokenKind::Star
            | TokenKind::Equal
            | TokenKind::Greater
            | TokenKind::Less
            | TokenKind::Bang => 1,
            // Token w/ length 2
            TokenKind::BangEqual
            | TokenKind::EqualEqual
            | TokenKind::GreaterEqual
            | TokenKind::Fn
            | TokenKind::If
            | TokenKind::Or
            | TokenKind::LessEqual => 2,
            // Token w/ length 3
            TokenKind::For | TokenKind::Nil | TokenKind::EOF | TokenKind::And => 3,
            // Token w/ length 4
            TokenKind::True | TokenKind::Else => 4,
            // Token w/ length 5
            TokenKind::Print | TokenKind::While | TokenKind::False => 5,
            // Token w/ length 6
            TokenKind::Return => 6,
            TokenKind::Identifier(i) => i.len(),
            TokenKind::String(s) => s.len(),
            TokenKind::Number(_, i) => *i,
        }
    }
}

pub fn get_keyword(identifier: &str) -> Option<TokenKind> {
    match identifier {
        "fn" => Some(TokenKind::Fn),
        "if" => Some(TokenKind::If),
        "or" => Some(TokenKind::Or),
        "and" => Some(TokenKind::And),
        "else" => Some(TokenKind::Else),
        "for" => Some(TokenKind::For),
        "nil" => Some(TokenKind::Nil),
        "print" => Some(TokenKind::Print),
        "return" => Some(TokenKind::Return),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "while" => Some(TokenKind::While),
        _ => None,
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ampersand => f.write_str("&"),
            TokenKind::LeftParenthesis => f.write_str("("),
            TokenKind::RightParenthesis => f.write_str(")"),
            TokenKind::LeftBrace => f.write_str("{"),
            TokenKind::RightBrace => f.write_str("}"),
            TokenKind::LeftBracket => f.write_str("["),
            TokenKind::RightBracket => f.write_str("]"),
            TokenKind::Comma => f.write_str(","),
            TokenKind::Dot => f.write_str("."),
            TokenKind::Minus => f.write_str("-"),
            TokenKind::Plus => f.write_str("+"),
            TokenKind::Semicolon => f.write_str(";"),
            TokenKind::Colon => f.write_str(":"),
            TokenKind::Slash => f.write_str("/"),
            TokenKind::Star => f.write_str("*"),
            TokenKind::Bang => f.write_str("!"),
            TokenKind::BangEqual => f.write_str("!="),
            TokenKind::Equal => f.write_str("="),
            TokenKind::EqualEqual => f.write_str("=="),
            TokenKind::Greater => f.write_str(">"),
            TokenKind::GreaterEqual => f.write_str(">="),
            TokenKind::Less => f.write_str("<"),
            TokenKind::LessEqual => f.write_str("<="),
            TokenKind::Identifier(id) => f.write_fmt(format_args!("{id}")),
            TokenKind::String(s) => f.write_fmt(format_args!("{s}")),
            TokenKind::Number(n, _) => f.write_fmt(format_args!("{n}")),
            TokenKind::And => f.write_str("and"),
            TokenKind::Else => f.write_str("else"),
            TokenKind::False => f.write_str("false"),
            TokenKind::Fn => f.write_str("fn"),
            TokenKind::For => f.write_str("for"),
            TokenKind::If => f.write_str("if"),
            TokenKind::Nil => f.write_str("nil"),
            TokenKind::Or => f.write_str("or"),
            TokenKind::Print => f.write_str("print"),
            TokenKind::Return => f.write_str("return"),
            TokenKind::True => f.write_str("true"),
            TokenKind::While => f.write_str("while"),
            TokenKind::EOF => f.write_str("EOF"),
        }
    }
}
