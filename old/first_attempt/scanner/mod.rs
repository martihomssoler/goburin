pub mod parser;

use std::{error::Error, fmt::Display};

use crate::ParserResult;

use self::parser::Parser;

pub fn run_file(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(file_path)?;

    run(&file_content)
}
pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    use std::io::{stdin, stdout, Write};

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
        run(&input)?;
    }

    Ok(())
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let tokens = scan_tokens(source)?;
    // for token in &tokens {
    //     println!("{token:?}");
    // }
    let expr = Parser::new(tokens).parse()?;
    // println!("{expr}");
    match parser::interpret(&expr) {
        Ok(value) => {
            println!("{value:?}");
        }
        Err(err) => {
            println!("{} \n    [Error on line {}]", err.message, err.line);
        }
    }

    Ok(())
}

fn scan_tokens(source: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens = Vec::new();

    let mut start = 0;
    let mut current = 0;

    let mut col: usize = 1;
    let mut line: usize = 1;

    let mut char_iterator = source.chars().peekable();
    while let Some(c) = char_iterator.next() {
        start = current;
        current += 1;

        let kind = match c {
            ' ' | '\t' | '\r' => {
                continue;
            }
            '\n' => {
                col = 1;
                line += 1;
                continue;
            }
            '&' => TokenKind::Ampersand,
            '(' => TokenKind::LeftParenthesis,
            ')' => TokenKind::RightParenthesis,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,
            '*' => TokenKind::Star,
            '!' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            '=' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '<' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '/' => {
                // is sigle line comment "//"
                if next_char_matches(&mut char_iterator, &mut current, '/') {
                    while let Some(c) = char_iterator.peek()
                        && !c.eq(&'\n')
                    {
                        current += 1;
                        char_iterator.next();
                    }
                    current += 1;
                    col = 1;
                    line += 1;
                    char_iterator.next();
                    continue;
                }
                // is sigle line comment "//"
                else if next_char_matches(&mut char_iterator, &mut current, '*') {
                    multiline_comment(
                        &mut char_iterator,
                        &mut current,
                        &mut start,
                        &mut col,
                        &mut line,
                    );
                    continue;
                } else {
                    TokenKind::Slash
                }
            }
            '"' => {
                start = current;
                consume_string(&mut char_iterator, &mut current, &mut col, &mut line);

                if char_iterator.peek().is_none() {
                    return Err(format!("Unterminated string at {line}:{col}").into());
                }

                let value = source[start..current].to_string();
                current += 1;
                char_iterator.next();
                TokenKind::String(value)
            }
            c => {
                if c.is_ascii_digit() {
                    consume_digits(&mut char_iterator, &mut current);

                    // Look for a fractional part
                    if next_char_matches(&mut char_iterator, &mut current, '.')
                        && let Some(c) = char_iterator.peek()
                        && c.is_ascii_digit()
                    {
                        consume_digits(&mut char_iterator, &mut current);
                    }
                    let value = source[start..current].to_string();
                    let number_length_in_chars = current - start;
                    let parsed_number = value.parse::<f64>()?;

                    TokenKind::Number(parsed_number, number_length_in_chars)
                } else if c.is_alphabetic() {
                    identifier_or_keyword(source, &mut char_iterator, &mut current, start)
                } else {
                    return Err(format!("Unexpected character '{c}' at {line}:{col}").into());
                }
            }
        };

        tokens.push(Token { kind, start, line });
    }

    tokens.push(Token {
        kind: TokenKind::EOF,
        start,
        line,
    });

    Ok(tokens)
}

fn identifier_or_keyword(
    source: &str,
    char_iterator: &mut PeekableChars,
    current: &mut usize,
    start: usize,
) -> TokenKind {
    while let Some(c) = char_iterator.peek()
        && c.is_alphanumeric()
    {
        *current += 1;
        char_iterator.next();
    }

    let value = source[start..*current].to_string();
    if let Some(token_kind) = get_keyword(&value) {
        token_kind
    } else {
        TokenKind::Identifier(value)
    }
}

fn multiline_comment(
    char_iterator: &mut PeekableChars,
    current: &mut usize,
    start: &mut usize,
    col: &mut usize,
    line: &mut usize,
) {
    while let Some(c) = char_iterator.next() {
        *start = *current;
        *current += 1;
        *col += 1;

        match c {
            '\n' => {
                *col = 1;
                *line += 1;
            }
            '*' => {
                if next_char_matches(char_iterator, current, '/') {
                    return;
                }
            }

            '/' => {
                if next_char_matches(char_iterator, current, '*') {
                    multiline_comment(char_iterator, current, start, col, line);
                }
            }
            _ => {}
        }
    }
}

fn consume_string(
    char_iterator: &mut PeekableChars,
    current: &mut usize,
    col: &mut usize,
    line: &mut usize,
) {
    while let Some(c) = char_iterator.peek()
        && !c.eq(&'"')
    {
        *current += 1;
        if c.eq(&'\n') {
            *col = 1;
            *line += 1;
        }
        char_iterator.next();
    }
}

fn consume_digits(char_iterator: &mut PeekableChars, current: &mut usize) {
    while let Some(c) = char_iterator.peek()
        && c.is_ascii_digit()
    {
        *current += 1;
        char_iterator.next();
    }
}

type PeekableChars<'a> = std::iter::Peekable<std::str::Chars<'a>>;

fn next_char_matches(iterator: &mut PeekableChars, current: &mut usize, expected: char) -> bool {
    if let Some(c) = iterator.peek() {
        if expected.eq(c) {
            iterator.next();
            *current += 1;
            true
        } else {
            false
        }
    } else {
        false
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub line: usize,
}

impl Token {
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

fn get_keyword(identifier: &str) -> Option<TokenKind> {
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
