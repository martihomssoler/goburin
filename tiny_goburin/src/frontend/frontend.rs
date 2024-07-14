use std::ops::Deref;

mod lexer;
mod parser;
mod semantic;

pub(crate) fn frontend_pass(source: &str) -> Result<Ast, String> {
    let tokens = lexer::l_tokenize(source)?; // print_tokens(&tokens);
    let ast = parser::p_parse(tokens)?; // print_ast(&ast);
    let ast = semantic::check(ast)?; // print_ast(&ast);

    Ok(ast)
}

// --- SEMANTIC ---
#[derive(Default)]
pub struct SymbolTable {
    /// Poor mans hashmap of [ var_name => temporary assigned to it ]
    /// TODO(mhs): improve this, making it dumb so it is easy to self-host later
    scope_stack: Vec<Vec<Symbol>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub typ: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Local { pos: u32 },
    Parameter { pos: u32 },
    Global,
}

impl SymbolTable {
    pub fn scope_enter(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    pub fn scope_exit(&mut self) {
        let _ = self.scope_stack.pop();
    }

    pub fn scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    /// Declarations overwrite previous ones
    pub fn scope_symbol_bind(&mut self, symbol: Symbol) {
        if self.scope_stack.is_empty() {
            self.scope_stack.push(Vec::new());
        };

        let curr_scope = &mut self.scope_stack[0];

        let name_to_find = &symbol.name;
        for i in 0..curr_scope.len() {
            if curr_scope[i].name.eq(name_to_find) {
                curr_scope[i] = symbol;
                return;
            }
        }

        curr_scope.push(symbol);
    }

    pub fn scope_symbol_lookup(&self, name_to_find: &str) -> Option<Symbol> {
        for i in (0..self.scope_stack.len()).rev() {
            let curr_scope = &self.scope_stack[i];

            for j in 0..curr_scope.len() {
                if curr_scope[j].name.eq(name_to_find) {
                    return Some(curr_scope[j].clone());
                }
            }
        }

        None
    }

    /// This is used to determine whether a symbol has already been defined in the current scope
    pub fn scope_symbol_lookup_current(&self, name_to_find: &str) -> Option<Symbol> {
        if self.scope_stack.is_empty() {
            return None;
        };
        let curr_scope = &self.scope_stack[0];

        for j in 0..curr_scope.len() {
            if curr_scope[j].name.eq(name_to_find) {
                return Some(curr_scope[j].clone());
            }
        }

        None
    }
}

// --- ---

// --- PARSER ---
pub struct Ast {
    pub declarations: Vec<Declaration>,
    pub symbol_table: SymbolTable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstNode {
    pub token: Token<TokenKind>,
    pub symbol: Option<Symbol>,
    pub reg: u32,
}
impl AstNode {
    fn from_token_value(val: &Token<Value>) -> AstNode {
        AstNode {
            token: Token {
                kind: TokenKind::Value(val.kind.clone()),
                line: val.line,
                col: val.col,
                sym: None, // FIXME(mhs): remove the symbol in here
            },
            symbol: None,
            reg: u32::MAX,
        }
    }

    fn from_token(token: &Token<TokenKind>) -> AstNode {
        AstNode {
            token: token.clone(),
            symbol: None,
            reg: u32::MAX,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub typ: Type,
    pub val: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub name: String,
    pub kind: ExpressionKind,
    pub node: AstNode,
    pub left: Option<Box<Expression>>,
    pub right: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    LiteralInteger(i64),
    LiteralString(String),
    Identifier(String),
    // TODO(mhs): remove this, it is just for testing purposes, print should be a proper function call
    PrintCall(PrintCall),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrintCall {
    pub exprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Print {
    pub vals: Vec<Token<Value>>,
}

// --- ---

impl std::fmt::Display for Declaration {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.val.kind {
            ExpressionKind::LiteralInteger(i) => write!(fmt, "LiteralInteger"),
            ExpressionKind::LiteralString(s) => write!(fmt, "LiteralString"),
            ExpressionKind::Identifier(id) => write!(fmt, "Identifier"),
            ExpressionKind::PrintCall(p) => write!(fmt, "PrintCall"),
        }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in &self.declarations {
            write!(fmt, "{decl}")?;
        }
        Ok(())
    }
}
// --- ---

// --- LEXER ---
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<K> {
    pub kind: K,
    pub line: usize,
    pub col: usize,
    pub sym: Option<Symbol>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// Identifiers must begin with a letter or an underscore. E.g.: i, x, mystr, fog123, _bigLongName55
    Keyword(Keyword),
    Type(Type),
    Operator(Operator),
    Value(Value),
    Eof,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Else,
    False,
    For,
    Function,
    If,
    Print,
    Return,
    True,
    While,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Untyped,
    Array(Box<Type>),
    Bool,
    Char,
    Int,
    String,
    Void,
    Function(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Bang,
    BangEqual,
    BraceLeft,
    BraceRight,
    BracketLeft,
    BracketRight,
    Caret,
    Colon,
    Comma,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    LogicAnd,
    LogicOr,
    Lower,
    LowerEqual,
    Minus,
    ParenthesisLeft,
    ParenthesisRight,
    Percent,
    Plus,
    Point,
    Semicolon,
    Slash,
    Star,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Constant(Constant),
    Identifier(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constant {
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
}

// TODO(mhs): Remove this
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier(pub String);

impl std::fmt::Display for Token<TokenKind> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenKind::Keyword(kw) => match kw {
                Keyword::Else => write!(fmt, "Keyword: Else"),
                Keyword::False => write!(fmt, "Keyword: False"),
                Keyword::For => write!(fmt, "Keyword: For"),
                Keyword::Function => write!(fmt, "Keyword: Function"),
                Keyword::If => write!(fmt, "Keyword: If"),
                Keyword::Print => write!(fmt, "Keyword: Print"),
                Keyword::Return => write!(fmt, "Keyword: Return"),
                Keyword::True => write!(fmt, "Keyword: True"),
                Keyword::While => write!(fmt, "Keyword: While"),
            },
            TokenKind::Type(typ) => match typ {
                Type::Array(t) => write!(fmt, "Keyword: Array of {:?}", t.as_ref()),
                Type::Bool => write!(fmt, "Keyword: Bool"),
                Type::Char => write!(fmt, "Keyword: Char"),
                Type::Int => write!(fmt, "Keyword: Int"),
                Type::String => write!(fmt, "Keyword: String"),
                Type::Void => write!(fmt, "Keyword: Void"),
                Type::Function(a, r) => write!(
                    fmt,
                    "Keyword: Function ({:?}) -> ({:?})",
                    a.as_ref(),
                    r.as_ref()
                ),
                Type::Untyped => write!(fmt, "Untyped"),
            },
            TokenKind::Operator(sy) => match sy {
                Operator::Bang => write!(fmt, "Symbol \"!\""),
                Operator::BangEqual => write!(fmt, "Symbol \"!=\""),
                Operator::BraceLeft => write!(fmt, "Symbol \"{{\""),
                Operator::BraceRight => write!(fmt, "Symbol \"}}\""),
                Operator::BracketLeft => write!(fmt, "Symbol \"[\""),
                Operator::BracketRight => write!(fmt, "Symbol \"]\""),
                Operator::Caret => write!(fmt, "Symbol \"^\""),
                Operator::Colon => write!(fmt, "Symbol \":\""),
                Operator::Comma => write!(fmt, "Symbol \",\""),
                Operator::Equal => write!(fmt, "Symbol \"=\""),
                Operator::EqualEqual => write!(fmt, "Symbol \"==\""),
                Operator::Greater => write!(fmt, "Symbol \">\""),
                Operator::GreaterEqual => write!(fmt, "Symbol \">=\""),
                Operator::LogicAnd => write!(fmt, "Symbol \"&&\""),
                Operator::LogicOr => write!(fmt, "Symbol \"||\""),
                Operator::Lower => write!(fmt, "Symbol \"<\""),
                Operator::LowerEqual => write!(fmt, "Symbol \"<=\""),
                Operator::Minus => write!(fmt, "Symbol \"-\""),
                Operator::ParenthesisLeft => write!(fmt, "Symbol \"(\""),
                Operator::ParenthesisRight => write!(fmt, "Symbol \")\""),
                Operator::Percent => write!(fmt, "Symbol \"%\""),
                Operator::Plus => write!(fmt, "Symbol \"+\""),
                Operator::Point => write!(fmt, "Symbol \".\""),
                Operator::Semicolon => write!(fmt, "Symbol \";\""),
                Operator::Slash => write!(fmt, "Symbol \"/\""),
                Operator::Star => write!(fmt, "Symbol \"*\""),
            },
            TokenKind::Value(val) => match val {
                Value::Constant(cons) => match cons {
                    Constant::Int(i) => write!(fmt, "Int {i}"),
                    Constant::Bool(b) => write!(fmt, "Bool {b}"),
                    Constant::Char(c) => write!(fmt, "Char {c:?}"),
                    Constant::String(s) => write!(fmt, "String {s:?}"),
                },
                Value::Identifier(id) => write!(fmt, "Id {}", id.0),
            },
            TokenKind::Eof => write!(fmt, "~~ EOF ~~"),
        }
    }
}
// --- ---

// --- PRINTERS ---
fn print_ast(ast: &Ast) {
    println!(
        "[AST]\n{}\n",
        ast.declarations
            .iter()
            .map(|t| format!("  ->  {t:?}"))
            .collect::<Vec<String>>()
            .join("\n")
    );
}

fn print_tokens(tokens: &[Token<TokenKind>]) {
    println!(
        "Tokens --> {}",
        tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    );
}
// --- ---
