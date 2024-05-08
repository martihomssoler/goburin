use std::fmt::Display;

use crate::token::{get_keyword, Token, TokenKind};

pub type LexerResult<T> = Result<T, LexerError>;
type CharPeekableIterator<'a> = std::iter::Peekable<std::str::Chars<'a>>;

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    iterator: CharPeekableIterator<'a>,
    current_lexeme_pos: usize,
    start_lexeme_pos: usize,
    column: usize,
    line: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn scan_source(source: &'a str) -> LexerResult<Vec<Token>> {
        let lexer = Self {
            source,
            iterator: source.chars().peekable(),
            current_lexeme_pos: 0,
            start_lexeme_pos: 0,
            column: 1,
            line: 1,
            tokens: Vec::new(),
        };

        lexer.scan_all_tokens()
    }

    fn scan_all_tokens(mut self) -> LexerResult<Vec<Token>> {
        while !self.is_at_end() {
            self.start_lexeme_pos = self.current_lexeme_pos;
            self.scan_next_token()?;
        }

        self.start_lexeme_pos = self.current_lexeme_pos;
        self.add_token(TokenKind::EOF);

        Ok(self.tokens)
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            start: self.start_lexeme_pos,
            end: self.current_lexeme_pos,
            line: self.line,
        });
    }

    #[inline(always)]
    fn is_at_end(&mut self) -> bool {
        self.iterator.peek().is_none()
    }

    fn scan_next_token(&mut self) -> LexerResult<()> {
        let Some(c) = self.next() else {
            return Ok(());
        };

        match c {
            // | '\t' | '\r'
            ' ' => {}
            '\n' => {
                self.column = 1;
                self.line += 1;
            }
            '&' => self.add_token(TokenKind::Ampersand),
            '(' => self.add_token(TokenKind::LeftParenthesis),
            ')' => self.add_token(TokenKind::RightParenthesis),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            '[' => self.add_token(TokenKind::LeftBracket),
            ']' => self.add_token(TokenKind::RightBracket),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            ':' => self.add_token(TokenKind::Colon),
            '*' => self.add_token(TokenKind::Star),
            '!' => {
                self.match_char_and_add_token('=', TokenKind::BangEqual, TokenKind::Bang);
            }
            '=' => {
                self.match_char_and_add_token('=', TokenKind::EqualEqual, TokenKind::Equal);
            }
            '<' => {
                self.match_char_and_add_token('=', TokenKind::LessEqual, TokenKind::Less);
            }
            '>' => {
                self.match_char_and_add_token('=', TokenKind::GreaterEqual, TokenKind::Greater);
            }
            '/' => {
                // is sigle line comment "//"
                if let Some(c) = self.peek()
                    && '/'.eq(c)
                {
                    self.skip_single_line_comment();
                } else if let Some(c) = self.peek()
                    && '*'.eq(c)
                {
                    self.skip_multi_line_comment();
                } else {
                    self.add_token(TokenKind::Slash)
                }
            }
            '"' => self.add_string()?,
            d if d.is_ascii_digit() => self.add_number()?,
            c if c.is_alphabetic() => self.add_identifier_or_keyword()?,
            unexpected => {
                panic!(
                    "Unexpected character {unexpected:?} at [line {}: col {}].",
                    self.line, self.column
                );
            }
        }

        Ok(())
    }

    fn match_char(&mut self, target: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if let Some(c) = self.peek()
            && !target.eq(c)
        {
            return false;
        }
        let _ = self.next();
        true
    }

    fn match_char_and_add_token(
        &mut self,
        target: char,
        true_val: TokenKind,
        false_ret: TokenKind,
    ) {
        let kind = if self.match_char(target) {
            true_val
        } else {
            false_ret
        };
        self.add_token(kind);
    }

    fn skip_single_line_comment(&mut self) {
        while let Some(c) = self.peek()
            && !c.eq(&'\n')
        {
            self.current_lexeme_pos += 1;
            self.next();
        }
        self.current_lexeme_pos += 1;
        self.column = 1;
        self.line += 1;
        self.next();
    }

    fn skip_multi_line_comment(&mut self) {
        while let Some(c) = self.next() {
            self.current_lexeme_pos += 1;
            self.column += 1;

            match c {
                '\n' => {
                    self.column = 1;
                    self.line += 1;
                }
                '*' => {
                    if self.match_char('/') {
                        return;
                    }
                }

                '/' => {
                    if self.match_char('*') {
                        self.skip_multi_line_comment();
                    }
                }
                _ => {}
            }
        }
    }

    // Return the lookahead character.
    fn peek(&mut self) -> Option<&char> {
        self.iterator.peek()
    }

    // Return the next character.
    fn next(&mut self) -> Option<char> {
        self.current_lexeme_pos += 1;
        self.iterator.next()
    }

    fn add_identifier_or_keyword(&mut self) -> LexerResult<()> {
        while let Some(c) = self.peek()
            && (c.is_ascii_alphanumeric() || c.eq(&'_'))
        {
            self.next();
        }

        let value = self.source[self.start_lexeme_pos..self.current_lexeme_pos].to_string();
        if let Some(token_kind) = get_keyword(&value) {
            self.add_token(token_kind);
        } else {
            self.add_token(TokenKind::Identifier(value));
        };

        Ok(())
    }

    fn add_number(&mut self) -> LexerResult<()> {
        self.consume_digits();

        // Look for a fractional part
        if self.match_char('.') {
            if let Some(c) = self.next()
                && c.is_ascii_digit()
            {
                self.consume_digits();
            } else {
                return Err(LexerError::new_unterminated_float(self.line, self.column));
            }
        }

        let value = self.source[self.start_lexeme_pos..self.current_lexeme_pos].to_string();
        let number_length_in_chars = self.current_lexeme_pos - self.start_lexeme_pos;
        let parsed_number = value
            .parse::<f64>()
            .map_err(|e| LexerError::new_unknown_error(e.to_string(), self.line, self.column))?;

        TokenKind::Number(parsed_number, number_length_in_chars);

        Ok(())
    }

    fn consume_digits(&mut self) {
        while let Some(c) = self.peek()
            && c.is_ascii_digit()
        {
            self.next();
        }
    }

    fn add_string(&mut self) -> LexerResult<()> {
        self.start_lexeme_pos = self.current_lexeme_pos;
        self.consume_string();

        if self.peek().is_none() {
            return Err(LexerError::new_unterminated_string(self.line, self.column));
        }

        let value = self.source[self.start_lexeme_pos..self.current_lexeme_pos].to_string();
        self.next();
        self.add_token(TokenKind::String(value));
        Ok(())
    }

    fn consume_string(&mut self) {
        while let Some(c) = self.peek()
            && !c.eq(&'"')
        {
            if c.eq(&'\n') {
                self.column = 1;
                self.line += 1;
            }
            self.next();
        }
    }
}

#[derive(Debug)]
pub enum LexerErrorKind {
    UnterminatedString,
    UnterminatedFloat,
    UnknownError(String),
}

#[derive(Debug, thiserror::Error)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub column: usize,
    pub line: usize,
}

impl LexerError {
    pub fn new_unterminated_string(line: usize, column: usize) -> Self {
        Self {
            kind: LexerErrorKind::UnterminatedString,
            column,
            line,
        }
    }

    fn new_unterminated_float(line: usize, column: usize) -> Self {
        Self {
            kind: LexerErrorKind::UnterminatedFloat,
            column,
            line,
        }
    }

    fn new_unknown_error(message: String, line: usize, column: usize) -> Self {
        Self {
            kind: LexerErrorKind::UnknownError(message),
            column,
            line,
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LexerErrorKind::UnterminatedString => f.write_fmt(format_args!(
                "Unterminated string at {}:{}",
                self.line, self.column
            )),
            LexerErrorKind::UnterminatedFloat => f.write_fmt(format_args!(
                "Unterminated number at {}:{}. Expected a digit after the '.' decimal point.",
                self.line, self.column
            )),
            LexerErrorKind::UnknownError(message) => f.write_fmt(format_args!(
                "Error at {}:{} with message:\n\t{message}.",
                self.line, self.column
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::token::{Token, TokenKind::*};

    use super::{Lexer, LexerResult};

    #[test]
    fn single_char_tokens() -> LexerResult<()> {
        let source = "&(){}[],.-+*;:";
        let expected_tokens = [
            Token::new(Ampersand, 0, 1, 1),
            Token::new(LeftParenthesis, 1, 2, 1),
            Token::new(RightParenthesis, 2, 3, 1),
            Token::new(LeftBrace, 3, 4, 1),
            Token::new(RightBrace, 4, 5, 1),
            Token::new(LeftBracket, 5, 6, 1),
            Token::new(RightBracket, 6, 7, 1),
            Token::new(Comma, 7, 8, 1),
            Token::new(Dot, 8, 9, 1),
            Token::new(Minus, 9, 10, 1),
            Token::new(Plus, 10, 11, 1),
            Token::new(Star, 11, 12, 1),
            Token::new(Semicolon, 12, 13, 1),
            Token::new(Colon, 13, 14, 1),
            Token::new(EOF, 14, 14, 1),
        ]
        .to_vec();

        let actual_tokens = Lexer::scan_source(source)?;

        actual_tokens
            .iter()
            .zip(expected_tokens.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));

        Ok(())
    }

    #[test]
    fn single_or_double_char_tokens() -> LexerResult<()> {
        let source = "! != == = < <= > >=";
        let expected_tokens = [
            Token::new(Bang, 0, 1, 1),
            Token::new(BangEqual, 2, 4, 1),
            Token::new(EqualEqual, 5, 7, 1),
            Token::new(Equal, 8, 9, 1),
            Token::new(Less, 10, 11, 1),
            Token::new(LessEqual, 12, 14, 1),
            Token::new(Greater, 15, 16, 1),
            Token::new(GreaterEqual, 17, 19, 1),
            Token::new(EOF, 19, 19, 1),
        ]
        .to_vec();

        let actual_tokens = Lexer::scan_source(source)?;

        actual_tokens
            .iter()
            .zip(expected_tokens.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));

        Ok(())
    }

    #[test]
    fn comments() -> LexerResult<()> {
        let source = " 
            // --- Comments
            //   single line comments
            /*  multi-line comments with /* nesting */ */
        ";
        let expected_tokens = [Token::new(EOF, 215, 215, 5)].to_vec();

        let actual_tokens = Lexer::scan_source(source)?;

        actual_tokens
            .iter()
            .zip(expected_tokens.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));

        Ok(())
    }

    #[test]
    fn strings_correct() -> LexerResult<()> {
        let source = r#" 
            "Comment1"
            "Comment2"
            "Multi-line
             Comment"
        "#;
        let expected_tokens = [
            Token::new(String("Comment1".to_owned()), 15, 24, 2),
            Token::new(String("Comment2".to_owned()), 38, 47, 3),
            Token::new(
                String(
                    "Multi-line
             Comment"
                        .to_owned(),
                ),
                61,
                93,
                5,
            ),
            Token::new(EOF, 102, 102, 6),
        ]
        .to_vec();

        let actual_tokens = Lexer::scan_source(source)?;

        actual_tokens
            .iter()
            .zip(expected_tokens.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));

        Ok(())
    }

    #[test]
    fn strings_error() -> LexerResult<()> {
        let source = r#" 
            "Comment1
        "#;
        let error = Lexer::scan_source(source);

        assert!(error.is_err());

        Ok(())
    }

    #[test]
    fn identifiers_and_keywords() -> LexerResult<()> {
        let source = "or orchid and Andy if if_for_while-for-fn";
        let expected_tokens = [
            Token::new(Or, 0, 2, 1),
            Token::new(Identifier("orchid".to_owned()), 3, 9, 1),
            Token::new(And, 10, 13, 1),
            Token::new(Identifier("Andy".to_owned()), 14, 18, 1),
            Token::new(If, 19, 21, 1),
            Token::new(Identifier("if_for_while".to_owned()), 22, 34, 1),
            Token::new(Minus, 34, 35, 1),
            Token::new(For, 35, 38, 1),
            Token::new(Minus, 38, 39, 1),
            Token::new(Fn, 39, 41, 1),
            Token::new(EOF, 41, 41, 1),
        ]
        .to_vec();

        let actual_tokens = Lexer::scan_source(source)?;

        actual_tokens
            .iter()
            .zip(expected_tokens.iter())
            .for_each(|(actual, expected)| assert_eq!(actual, expected));

        Ok(())
    }
}
