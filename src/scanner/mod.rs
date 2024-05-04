use std::{error::Error, io::Read};

pub fn run_file(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(file_path)?;

    run(&file_content)
}
pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut input_content = String::new();

    loop {
        print!("> ");
        std::io::stdin().read_to_string(&mut input_content)?;
        if input_content.eq("exit") {
            break;
        }
        run(&input_content)?;
    }

    Ok(())
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let tokens = scan_tokens(source)?;

    for token in tokens {
        println!("{token:?}");
    }

    Ok(())
}

fn scan_tokens(source: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens = Vec::new();

    let mut start = 0;
    let mut current = 0;

    let mut col = 1;
    let mut line = 1;

    let mut char_iterator = source.char_indices().peekable();
    while let Some((len, c)) = char_iterator.next() {
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
            '(' => TokenKind::LEFT_PARENTHESIS,
            ')' => TokenKind::RIGHT_PARENTHESIS,
            '{' => TokenKind::LEFT_BRACE,
            '}' => TokenKind::RIGHT_BRACE,
            ',' => TokenKind::COMMA,
            '.' => TokenKind::DOT,
            '-' => TokenKind::MINUS,
            '+' => TokenKind::PLUS,
            ';' => TokenKind::SEMICOLON,
            '*' => TokenKind::STAR,
            '!' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::BANG_EQUAL
                } else {
                    TokenKind::BANG
                }
            }
            '=' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::EQUAL_EQUAL
                } else {
                    TokenKind::EQUAL
                }
            }
            '<' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::LESS_EQUAL
                } else {
                    TokenKind::LESS
                }
            }
            '>' => {
                if next_char_matches(&mut char_iterator, &mut current, '=') {
                    TokenKind::GREATER_EQUAL
                } else {
                    TokenKind::GREATER
                }
            }
            '/' => {
                if next_char_matches(&mut char_iterator, &mut current, '/') {
                    while let Some((_, c)) = char_iterator.peek()
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
                } else {
                    TokenKind::SLASH
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
                TokenKind::STRING(value)
            }
            c => {
                if c.is_ascii_digit() {
                    consume_digits(&mut char_iterator, &mut current);

                    // Look for a fractional part
                    if next_char_matches(&mut char_iterator, &mut current, '.')
                        && let Some((_, c)) = char_iterator.peek()
                        && c.is_ascii_digit()
                    {
                        consume_digits(&mut char_iterator, &mut current);
                    }
                    let value = source[start..current].to_string();
                    let number_length_in_chars = current - start;
                    let parsed_number = value.parse::<f64>()?;

                    current += 1;
                    char_iterator.next();

                    TokenKind::NUMBER(parsed_number, number_length_in_chars)
                } else if c.is_alphabetic() {
                    continue;
                } else {
                    return Err(format!("Unexpected character '{c}' at {line}:{col}").into());
                }
            }
        };

        start = current;
        tokens.push(Token { kind, start });
    }

    Ok(tokens)
}

fn consume_string(
    char_iterator: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    current: &mut usize,
    col: &mut i32,
    line: &mut i32,
) {
    while let Some((_, c)) = char_iterator.peek()
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

fn consume_digits(
    char_iterator: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    current: &mut usize,
) {
    while let Some((_, c)) = char_iterator.peek()
        && c.is_ascii_digit()
    {
        *current += 1;
        char_iterator.next();
    }
}

type PeekableCharIndices<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

fn next_char_matches(
    iterator: &mut PeekableCharIndices,
    current: &mut usize,
    expected: char,
) -> bool {
    if let Some((_, c)) = iterator.peek() {
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

#[derive(Debug)]
pub enum TokenKind {
    LEFT_PARENTHESIS,
    RIGHT_PARENTHESIS,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64, usize),
    AND,
    ELSE,
    FALSE,
    FN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    TRUE,
    WHILE,

    EOF,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    start: usize,
}

impl Token {
    pub fn len(&self) -> usize {
        match &self.kind {
            // Token w/ length 1
            TokenKind::LEFT_PARENTHESIS
            | TokenKind::RIGHT_PARENTHESIS
            | TokenKind::LEFT_BRACE
            | TokenKind::RIGHT_BRACE
            | TokenKind::COMMA
            | TokenKind::DOT
            | TokenKind::MINUS
            | TokenKind::PLUS
            | TokenKind::SEMICOLON
            | TokenKind::SLASH
            | TokenKind::STAR
            | TokenKind::EQUAL
            | TokenKind::GREATER
            | TokenKind::LESS
            | TokenKind::BANG => 1,
            // Token w/ length 2
            TokenKind::BANG_EQUAL
            | TokenKind::EQUAL_EQUAL
            | TokenKind::GREATER_EQUAL
            | TokenKind::FN
            | TokenKind::IF
            | TokenKind::OR
            | TokenKind::LESS_EQUAL => 2,
            // Token w/ length 3
            TokenKind::FOR | TokenKind::NIL | TokenKind::EOF | TokenKind::AND => 3,
            // Token w/ length 4
            TokenKind::TRUE | TokenKind::ELSE => 4,
            // Token w/ length 5
            TokenKind::PRINT | TokenKind::WHILE | TokenKind::FALSE => 5,
            // Token w/ length 6
            TokenKind::RETURN => 6,
            TokenKind::IDENTIFIER(i) => i.len(),
            TokenKind::STRING(s) => s.len(),
            TokenKind::NUMBER(_, i) => *i,
        }
    }
}
