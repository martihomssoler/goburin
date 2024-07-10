use std::ops::Deref;

mod lexer;
mod parser;

pub(crate) fn frontend_pass(source: &str) -> Result<Ast, String> {
    let tokens = lexer::l_tokenize(source)?;
    // print_tokens(&tokens);

    let mut ast = parser::p_parse(tokens)?;
    // print_ast(&ast);

    semantic_resolve(&mut ast)?;
    // print_ast(&ast);

    typecheck(&ast)?;

    Ok(ast)
}

// --- STATIC TYPE CHECKING ---
fn typecheck(ast: &Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let symbol_table = &ast.symbol_table;

    for i in 0..ast.stmts.len() {
        typecheck_stmt(symbol_table, &ast.stmts[i], &mut errors);
    }

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn typecheck_stmt(symbol_table: &SymbolTable, stmt: &Stmt, errors: &mut Vec<String>) {
    match stmt {
        Stmt::Decl(decl) => typecheck_decl(symbol_table, decl, errors),
        Stmt::Print(Print { vals }) => {
            for val in vals {
                typecheck_val(symbol_table, val, errors);
            }
        }
    };
}

fn typecheck_decl(symbol_table: &SymbolTable, decl: &Decl, errors: &mut Vec<String>) {
    let t_val = typecheck_val(symbol_table, &decl.val, errors);
    if !type_equals(&t_val, &decl.typ.kind) {
        errors.push(format!(
            "[TypeError]: Expected type '{:?}' but got '{:?}' for variable '{}' in {}:{}",
            decl.typ.kind, t_val, decl.id.kind.0, decl.val.line, decl.val.col,
        ));
    }
}

fn typecheck_val(symbol_table: &SymbolTable, val: &Token<Value>, errors: &mut Vec<String>) -> Type {
    match &val.kind {
        Value::Constant(c) => match c {
            Constant::Int(_) => Type::Int,
            Constant::Bool(_) => Type::Bool,
            Constant::Char(_) => Type::Char,
            Constant::String(_) => Type::String,
        },
        Value::Identifier(id) => {
            if let Some(symbol) = symbol_table.scope_symbol_lookup(&id.0) {
                symbol.typ
            } else {
                errors.push(format!(
                    "Undeclared variable '{}' in {}:{}",
                    id.0, val.line, val.col
                ));
                Type::Untyped
            }
        }
    }
}

fn type_equals(a: &Type, b: &Type) -> bool {
    if a == b {
        if matches!(a, Type::Untyped) {
            false
        } else if matches!(a, Type::Array(_)) {
            false
        } else if matches!(a, Type::Function(_, _)) {
            false
        } else {
            true
        }
    } else {
        false
    }
}
// --- ---

// --- NAME RESOLUTION ---
pub fn semantic_resolve(ast: &mut Ast) -> Result<(), String> {
    let mut errors: Vec<String> = Vec::new();
    let mut symbol_table = SymbolTable::default();

    // name resolution
    for i in 0..ast.stmts.len() {
        resolve_stmt(&mut symbol_table, &mut ast.stmts[i], &mut errors);
    }

    ast.symbol_table = symbol_table;

    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }

    Ok(())
}

fn resolve_stmt(symbol_table: &mut SymbolTable, stmt: &mut Stmt, errors: &mut Vec<String>) {
    match stmt {
        Stmt::Decl(decl) => resolve_decl(symbol_table, decl, errors),
        Stmt::Print(Print { vals }) => {
            for v in vals {
                resolve_val(symbol_table, v.clone(), errors);
            }
        }
    }
}

fn resolve_decl(symbol_table: &mut SymbolTable, decl: &mut Decl, errors: &mut Vec<String>) {
    let symbol_kind = if symbol_table.scope_level() > 1 {
        SymbolKind::Local { pos: 0 }
    } else {
        SymbolKind::Global
    };

    let symbol = Symbol {
        name: decl.id.kind.0.clone(),
        kind: symbol_kind,
        typ: decl.typ.kind.clone(),
    };
    decl.sym = Some(symbol.clone());

    resolve_val(symbol_table, decl.val.clone(), errors);
    symbol_table.scope_symbol_bind(symbol);
}

fn resolve_val(symbol_table: &SymbolTable, v: Token<Value>, errors: &mut Vec<String>) {
    match v.kind {
        Value::Constant(_) => (),
        Value::Identifier(id) => {
            if symbol_table.scope_symbol_lookup(&id.0).is_none() {
                errors.push(format!(
                    "[ERROR]: Use of undeclared variable {} in {}:{}",
                    id.0, v.line, v.col
                ));
            }
        }
    }
}
// --- ---

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
    pub stmts: Vec<Stmt>,
    pub symbol_table: SymbolTable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Decl(Decl),
    Print(Print),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Decl {
    pub id: Token<Identifier>,
    pub typ: Token<Type>,
    pub val: Token<Value>,
    pub sym: Option<Symbol>,
    // TODO(mhs): add function declarations
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Print {
    pub vals: Vec<Token<Value>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    LiteralInt(i64),
    LiteralString(String),
    Arg,
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Print(Print { vals }) => write!(fmt, "print {vals:?}"),
            Stmt::Decl(Decl { id, typ, val, sym }) => {
                write!(fmt, "{id:?} : {typ:?} = {val:?} |{sym:?}|")
            }
        }
    }
}
// --- ---

// --- LEXER ---
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<K> {
    pub kind: K,
    pub line: usize,
    pub col: usize,
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
        "AST --> {}",
        ast.stmts
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ")
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
