use super::*;

pub fn l_tokenize(source: &str) -> Result<Vec<Token<TokenKind>>, String> {
    let mut errors: Vec<String> = Vec::new();
    let mut tokens: Vec<Token<TokenKind>> = Vec::new();
    let chars = source.chars().collect::<Vec<_>>();
    let mut idx = 0;
    let mut line = 1;
    let mut col = 0;

    while idx < chars.len() {
        let c = chars[idx];
        idx += 1;
        col += 1;

        let kind = match c {
            ' ' => continue,
            '\t' => {
                col += 3; // TODO(mhs): a tab counts as 4 columns, for now
                continue;
            }
            '\n' => {
                col = 0;
                line += 1;
                continue;
            }
            '!' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::BangEqual)
                } else {
                    TokenKind::Symbol(Symbol::Bang)
                }
            }
            '{' => TokenKind::Symbol(Symbol::BraceLeft),
            '}' => TokenKind::Symbol(Symbol::BraceRight),
            '[' => TokenKind::Symbol(Symbol::BracketLeft),
            ']' => TokenKind::Symbol(Symbol::BracketRight),
            '^' => TokenKind::Symbol(Symbol::Caret),
            ':' => TokenKind::Symbol(Symbol::Colon),
            ',' => TokenKind::Symbol(Symbol::Comma),
            '=' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::EqualEqual)
                } else {
                    TokenKind::Symbol(Symbol::Equal)
                }
            }
            '>' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::GreaterEqual)
                } else {
                    TokenKind::Symbol(Symbol::Greater)
                }
            }
            '&' => {
                if idx < chars.len() && chars[idx].eq(&'&') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::LogicAnd)
                } else {
                    errors.push(format!("Logical And is written as `&&` but only one `&` was provided. {line}:{col}"));
                    continue;
                }
            }
            '|' => {
                if idx < chars.len() && chars[idx].eq(&'|') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::LogicOr)
                } else {
                    errors.push(format!(
                        "Logical Or is written as `||` but only one `|` was provided. {line}:{col}"
                    ));
                    continue;
                }
            }
            '<' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Symbol(Symbol::LowerEqual)
                } else {
                    TokenKind::Symbol(Symbol::Lower)
                }
            }
            '-' => {
                if idx < chars.len() && chars[idx].is_ascii_digit() {
                    let ch = chars[idx];
                    idx += 1;
                    col += 1;
                    l_tokenize_kind_int(ch, true, &mut idx, &chars, &mut col)
                } else {
                    TokenKind::Symbol(Symbol::Minus)
                }
            }
            '(' => TokenKind::Symbol(Symbol::ParenthesisLeft),
            ')' => TokenKind::Symbol(Symbol::ParenthesisRight),
            '%' => TokenKind::Symbol(Symbol::Percent),
            '+' => TokenKind::Symbol(Symbol::Plus),
            '.' => TokenKind::Symbol(Symbol::Point),
            ';' => TokenKind::Symbol(Symbol::Semicolon),
            '/' => {
                if idx < chars.len() && chars[idx].eq(&'/') {
                    while idx < chars.len() && !chars[idx].eq(&'\n') {
                        idx += 1;
                    }
                    col = 0;
                    line += 1;
                    continue;
                } else {
                    TokenKind::Symbol(Symbol::Slash)
                }
            }
            '*' => TokenKind::Symbol(Symbol::Star),
            d if d.is_ascii_digit() => l_tokenize_kind_int(c, false, &mut idx, &chars, &mut col),
            '\'' => {
                let is_escape_char = idx < chars.len() && chars[idx].eq(&'\\');

                if is_escape_char {
                    idx += 1;
                    col += 1;
                }
                let ch = if idx + 1 < chars.len() && !chars[idx + 1].eq(&'\'') {
                    // the char is not closed by a '
                    errors.push(format!("Chars have to be enclosed in '. {line}:{col}"));
                    continue;
                } else {
                    chars[idx]
                };

                idx += 1;
                col += 1;
                TokenKind::Value(Value::Constant(Constant::Char(ch)))
            }
            '"' => l_tokenize_kind_string(&mut idx, &chars, &mut col),
            c if c.is_alphabetic() || c.eq(&'_') => {
                let mut id = c.to_string();
                while idx < chars.len()
                    && (chars[idx].is_ascii_alphanumeric() || chars[idx].eq(&'_'))
                {
                    id.push(chars[idx]);
                    idx += 1;
                    col += 1;
                }

                match id.as_str() {
                    // Keyword
                    "else" => TokenKind::Keyword(Keyword::Else),
                    "false" => TokenKind::Keyword(Keyword::False),
                    "for" => TokenKind::Keyword(Keyword::For),
                    "function" => TokenKind::Keyword(Keyword::Function),
                    "if" => TokenKind::Keyword(Keyword::If),
                    "print" => TokenKind::Keyword(Keyword::Print),
                    "return" => TokenKind::Keyword(Keyword::Return),
                    "true" => TokenKind::Keyword(Keyword::True),
                    "while" => TokenKind::Keyword(Keyword::While),
                    // Types
                    "array" => TokenKind::Type(Type::Array),
                    "bool" => TokenKind::Type(Type::Bool),
                    "char" => TokenKind::Type(Type::Char),
                    "int" => TokenKind::Type(Type::Int),
                    "string" => TokenKind::Type(Type::String),
                    "void" => TokenKind::Type(Type::Void),
                    _ => TokenKind::Value(Value::Identifier(Identifier(id))),
                }
            }
            _ => {
                errors.push(format!(
                    "Unexpected character {c:?} at line {line} and col {col}"
                ));
                continue;
            }
        };

        let token = Token { kind, line, col };
        tokens.push(token);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    let token = Token {
        kind: TokenKind::Eof,
        line,
        col,
    };
    tokens.push(token);

    Ok(tokens)
}

fn l_tokenize_kind_string(idx: &mut usize, chars: &[char], col: &mut usize) -> TokenKind {
    let mut can_escape = false;
    let mut str = String::new();
    while *idx < chars.len() {
        let next_c = chars[*idx];
        let ch = if can_escape {
            match next_c {
                // replace("\\\\", "\\")
                '\\' => {
                    str.pop();
                    '\\'
                }
                // replace("\\n", "\n")
                'n' => {
                    str.pop();
                    '\n'
                }
                // replace("\\\"", "\"")
                '\"' => {
                    str.pop();
                    '\"'
                }
                _ => next_c,
            }
        } else {
            next_c
        };

        let is_str_ending = !can_escape && next_c.eq(&'"');
        can_escape = !can_escape && next_c.eq(&'\\');
        *idx += 1;
        *col += 1;

        if is_str_ending {
            break;
        }

        str.push(ch);
    }
    TokenKind::Value(Value::Constant(Constant::String(str)))
}

fn l_tokenize_kind_int(
    c: char,
    is_negative: bool,
    idx: &mut usize,
    chars: &[char],
    col: &mut usize,
) -> TokenKind {
    let mut number = c.to_string();
    while *idx < chars.len() && chars[*idx].is_ascii_digit() {
        number.push(chars[*idx]);
        *idx += 1;
        *col += 1;
    }
    let n = number.parse::<i64>().unwrap();
    if is_negative {
        TokenKind::Value(Value::Constant(Constant::Int(-n)))
    } else {
        TokenKind::Value(Value::Constant(Constant::Int(n)))
    }
}
