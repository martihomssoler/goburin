use super::*;

pub struct TokenList {
    pub tokens: Vec<Token<TokenKind>>,
}
impl TokenList {
    pub(crate) fn save(self, output: PathBuf) {}
}

impl SourceFile {
    pub fn p0_0_tokenize(self) -> Result<TokenList, String> {
        t_tokenize(self)
    }
}

/// {?} Pass
///
/// * input : path to the source file
/// * output : list of tokens
fn t_tokenize(input: SourceFile) -> Result<TokenList, String> {
    let source = input.0;
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
                    TokenKind::Operator(Operator::BangEqual)
                } else {
                    TokenKind::Operator(Operator::Bang)
                }
            }
            '{' => TokenKind::Operator(Operator::BraceLeft),
            '}' => TokenKind::Operator(Operator::BraceRight),
            '[' => TokenKind::Operator(Operator::BracketLeft),
            ']' => TokenKind::Operator(Operator::BracketRight),
            '^' => TokenKind::Operator(Operator::Caret),
            ':' => TokenKind::Operator(Operator::Colon),
            ',' => TokenKind::Operator(Operator::Comma),
            '=' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::EqualEqual)
                } else {
                    TokenKind::Operator(Operator::Equal)
                }
            }
            '>' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::GreaterEqual)
                } else {
                    TokenKind::Operator(Operator::Greater)
                }
            }
            '&' => {
                if idx < chars.len() && chars[idx].eq(&'&') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::LogicAnd)
                } else {
                    errors.push(format!(
                        "[l_tokenize] Logical And is written as `&&` but only one `&` was provided. {line}:{col}"
                    ));
                    continue;
                }
            }
            '|' => {
                if idx < chars.len() && chars[idx].eq(&'|') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::LogicOr)
                } else {
                    errors.push(format!(
                        "[l_tokenize] Logical Or is written as `||` but only one `|` was provided. {line}:{col}"
                    ));
                    continue;
                }
            }
            '<' => {
                if idx < chars.len() && chars[idx].eq(&'=') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::LowerEqual)
                } else {
                    TokenKind::Operator(Operator::Lower)
                }
            }
            '-' => {
                if idx < chars.len() && chars[idx].eq(&'>') {
                    idx += 1;
                    col += 1;
                    TokenKind::Operator(Operator::Arrow)
                } else if idx < chars.len() && chars[idx].is_ascii_digit() {
                    let ch = chars[idx];
                    idx += 1;
                    col += 1;
                    t_tokenize_kind_int(ch, true, &mut idx, &chars, &mut col)
                } else {
                    TokenKind::Operator(Operator::Minus)
                }
            }
            '(' => TokenKind::Operator(Operator::ParenthesisLeft),
            ')' => TokenKind::Operator(Operator::ParenthesisRight),
            '%' => TokenKind::Operator(Operator::Percent),
            '+' => TokenKind::Operator(Operator::Plus),
            '.' => TokenKind::Operator(Operator::Point),
            ';' => TokenKind::Operator(Operator::Semicolon),
            '/' => {
                if idx < chars.len() && chars[idx].eq(&'/') {
                    while idx < chars.len() && !chars[idx].eq(&'\n') {
                        idx += 1;
                    }
                    col = 0;
                    line += 1;
                    continue;
                } else {
                    TokenKind::Operator(Operator::Slash)
                }
            }
            '*' => TokenKind::Operator(Operator::Star),
            d if d.is_ascii_digit() => t_tokenize_kind_int(c, false, &mut idx, &chars, &mut col),
            '\'' => {
                let is_escape_char = idx < chars.len() && chars[idx].eq(&'\\');

                if is_escape_char {
                    idx += 1;
                    col += 1;
                }
                let ch = if idx + 1 < chars.len() && !chars[idx + 1].eq(&'\'') {
                    // the char is not closed by a '
                    errors.push(format!("[l_tokenize] Chars have to be enclosed in '. {line}:{col}"));
                    continue;
                } else {
                    chars[idx]
                };

                idx += 1;
                col += 1;
                TokenKind::Value(Value::Constant(Constant::Char(ch)))
            }
            '"' => t_tokenize_kind_string(&mut idx, &chars, &mut col),
            c if c.is_alphabetic() || c.eq(&'_') => {
                let mut id = c.to_string();
                while idx < chars.len() && (chars[idx].is_ascii_alphanumeric() || chars[idx].eq(&'_')) {
                    id.push(chars[idx]);
                    idx += 1;
                    col += 1;
                }

                match id.as_str() {
                    // Keyword
                    "else" => TokenKind::Keyword(Keyword::Else),
                    "false" => TokenKind::Keyword(Keyword::False),
                    "for" => TokenKind::Keyword(Keyword::For),
                    "if" => TokenKind::Keyword(Keyword::If),
                    "print" => TokenKind::Keyword(Keyword::Print),
                    "return" => TokenKind::Keyword(Keyword::Return),
                    "true" => TokenKind::Keyword(Keyword::True),
                    "while" => TokenKind::Keyword(Keyword::While),
                    // Types
                    "Array" => TokenKind::Type(Type::Array(Box::new(Type::Void))),
                    "Bool" => TokenKind::Type(Type::Bool),
                    "Char" => TokenKind::Type(Type::Char),
                    "Function" => TokenKind::Type(Type::Function(Vec::new(), Box::new(Type::Void))),
                    "Int" => TokenKind::Type(Type::Int),
                    "String" => TokenKind::Type(Type::String),
                    "Void" => TokenKind::Type(Type::Void),
                    _ => TokenKind::Value(Value::Identifier(Identifier(id))),
                }
            }
            _ => {
                errors.push(format!(
                    "[l_tokenize] Unexpected character {c:?} at line {line} and col {col}"
                ));
                continue;
            }
        };

        let token = Token {
            kind,
            line,
            col,
            sym: None,
        };
        tokens.push(token);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    let token = Token {
        kind: TokenKind::Eof,
        line,
        col,
        sym: None,
    };
    tokens.push(token);

    let token_list = TokenList { tokens };

    Ok(token_list)
}

fn t_tokenize_kind_string(idx: &mut usize, chars: &[char], col: &mut usize) -> TokenKind {
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

fn t_tokenize_kind_int(c: char, is_negative: bool, idx: &mut usize, chars: &[char], col: &mut usize) -> TokenKind {
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
