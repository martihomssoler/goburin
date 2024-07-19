mod p0_0_tokenize;
mod p1_0_parse;
mod p1_1_name_resolution;
mod p1_2_static_type_checking;
mod p2_0_intermediate_representation;
mod p3_0_codegen;

pub use p0_0_tokenize::*;
pub use p1_0_parse::*;
pub use p1_1_name_resolution::*;
pub use p1_2_static_type_checking::*;
pub use p2_0_intermediate_representation::*;
pub use p3_0_codegen::*;

use std::path::PathBuf;

/// The compiler architecture is based on the nanopass idea and divided into Stages.
///
/// A Stage is a grouping of functionality that generate the same output representation. Subsequently,
/// each stage of the compiler is sub-divided into passes, which represent the smallest amount of relevant wort in a
/// particular stage. Here is an overview of the different Stages:
///
/// * Stage 0 - `"p0_*"` => output: [`TokenList`]. Usually called tokenization or lexing. Responsible for digesting the
/// source file into tokens.
/// * Stage 1 - `"p1_*"` => output: [`Ast`]. Usually called parsing. All passes that produce an [`Ast`], such as
/// typechecking, belong here.
/// * Stage 2 - `"p2_*"` => output: [`IR`]. Home to some of the earlier optimizations.
/// * Stage 3 - `"p2_*"` => output: [`binary`]. Target dependant code generation.
pub fn c_compile_file(file_path: PathBuf) -> Result<(), String> {
    SourceFile::new(&file_path)?
        // Stage 0
        .p0_0_tokenize()?
        // Stage 1
        .p1_0_parse()?
        .p1_1_name_resolution()?
        .p1_2_static_type_checking()?
        // Stage 2
        .p2_0_ir()?
        // Stage 3
        .p3_0_codegen(&file_path, &mut X86_64::default())?; // comment

    Ok(())
}

pub struct SourceFile(String);
impl SourceFile {
    pub fn new(path: &Path) -> Result<Self, String> {
        // [`canonicalize`] converts the file_path into an absolute path, in case the user provided a relative path.
        let canonized_path = std::fs::canonicalize(path).map_err(|e| format!("{e} : {path:?}"))?;
        let source = std::fs::read_to_string(&canonized_path).map_err(|e| format!("{e} : {canonized_path:?}"))?;
        Ok(Self(source))
    }
}

// TODO(mhs): move this to a "common"/"shared" module
use std::ops::Deref;

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
    pub program: Vec<Definition>,
    pub symbol_table: SymbolTable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Definition {
    pub id: Identifier,
    pub typ: Type,
    pub val: DefinitionValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefinitionValue {
    FunctionBody(Vec<Statement>),
}
// --- ---

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Declaration(Declaration),
    Expression(Expression),
    Print(Vec<Expression>),
    Return(Expression),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub typ: Type,
    pub val: DeclarationValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationValue {
    Uninitialized,
    Expression(Expression),
    FunctionBody(Vec<Statement>),
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
}

// --- ---
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

impl std::fmt::Display for Ast {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for def in &self.program {
            write!(fmt, "{def:?}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.val {
            DeclarationValue::Uninitialized => write!(fmt, "Uninitialized "),
            DeclarationValue::Expression(e) => write!(fmt, "Expression {e:?}"),
            DeclarationValue::FunctionBody(s) => write!(fmt, "Statements {s:?}"),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Expression {
            name,
            kind,
            node,
            left,
            right,
        } = self;

        let kind_str = match kind {
            ExpressionKind::LiteralInteger(i) => format!("{i}"),
            ExpressionKind::LiteralString(s) => format!("\"{s}\""),
            ExpressionKind::Identifier(id) => format!("\"{id}\""),
        };
        let l = if let Some(l) = left {
            l.as_ref().to_string()
        } else {
            "()".to_string()
        };
        let r = if let Some(r) = right {
            r.as_ref().to_string()
        } else {
            "()".to_string()
        };

        write!(
            fmt,
            "[EXPR: name:{name:?}, value:{kind_str}, node:{node},\n\tleft:{},\n\tright:{},\n]",
            l, r
        )
    }
}

impl std::fmt::Display for AstNode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AstNode { token, symbol, reg } = self;
        write!(fmt, "({}:{})", token.line, token.col)
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
impl TokenKind {
    fn constant() -> TokenKind {
        TokenKind::Value(Value::Constant(Constant::Bool(false)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Else,
    False,
    For,
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
    Function(Vec<(Identifier, Type)>, Box<Type>),
    Int,
    String,
    Tuple(Box<Type>, Option<Box<Type>>),
    Void,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Arrow,
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
                Keyword::If => write!(fmt, "Keyword: If"),
                Keyword::Print => write!(fmt, "Keyword: Print"),
                Keyword::Return => write!(fmt, "Keyword: Return"),
                Keyword::True => write!(fmt, "Keyword: True"),
                Keyword::While => write!(fmt, "Keyword: While"),
            },
            TokenKind::Type(typ) => match typ {
                Type::Array(t) => write!(fmt, "Type: Array of {:?}", t.as_ref()),
                Type::Bool => write!(fmt, "Type: Bool"),
                Type::Char => write!(fmt, "Type: Char"),
                Type::Int => write!(fmt, "Type: Int"),
                Type::String => write!(fmt, "Type: String"),
                Type::Void => write!(fmt, "Type: Void"),
                Type::Function(a, r) => {
                    write!(fmt, "Type: Function ({:?}) -> ({:?})", a, r.as_ref())
                }
                Type::Tuple(f, s) => {
                    write!(fmt, "Type: Tuple ({:?}, {:?})", f.as_ref(), s.as_ref())
                }
                Type::Untyped => write!(fmt, "Untyped"),
            },
            TokenKind::Operator(sy) => match sy {
                Operator::Arrow => write!(fmt, "Symbol \"->\""),
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
        ast.program
            .iter()
            .map(|t| format!("  ->  {t:?}"))
            .collect::<Vec<String>>()
            .join("\n")
    );
}

fn print_tokens(tokens: &[Token<TokenKind>]) {
    println!(
        "Tokens --> {}",
        tokens.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
    );
}
// --- ---

pub struct Ir {
    pub program: Vec<Definition>,
    pub state: IrState,
}

pub struct IrState {
    pub symbol_table: SymbolTable,
    pub vars: Vec<(String, String)>,
    pub strs: Vec<(String, String)>,
}

impl IrState {
    pub fn var_insert(&mut self, name: &str, val: &str) {
        self.vars.push((name.to_owned(), val.to_owned()));
    }

    pub fn var_get(&mut self, name: &str) -> Option<String> {
        for i in (0..self.vars.len()).rev() {
            let curr_var = &self.vars[i];

            if curr_var.0.eq(name) {
                return Some(curr_var.1.clone());
            }
        }

        None
    }

    pub fn str_insert(&mut self, name: &str, val: &str) {
        self.strs.push((name.to_owned(), val.to_owned()));
    }

    pub fn str_get(&mut self, name: &str) -> Option<String> {
        for i in (0..self.strs.len()).rev() {
            let curr_var = &self.strs[i];

            if curr_var.0.eq(name) {
                return Some(curr_var.1.clone());
            }
        }

        None
    }
}

/// For now we use FASM as our assembly target language
use std::{
    fmt::Write,
    fs::File,
    path::Path,
    process::{Command, Stdio},
};

// --- TARGET ---
pub trait Target {
    fn codegen(&mut self, ir: Ir) -> Result<String, String>;

    fn scratch_alloc(&mut self) -> u32;
    fn scratch_free(&mut self, r: u32);
    fn scratch_name(&self, r: u32) -> String;

    fn label_create(&mut self) -> u32;
    fn label_name(&self, l: u32) -> String;
}

#[derive(Default)]
pub struct X86_64 {
    pub regs: [bool; X86_64::SCRATCH_REGS.len()],
}

impl X86_64 {
    const SCRATCH_REGS: [&'static str; 7] = ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"];
}

impl Target for X86_64 {
    fn codegen(&mut self, mut ir: Ir) -> Result<String, String> {
        let mut errors: Vec<String> = Vec::new();
        let mut code = String::new();

        // --- PRELUDE ---
        // file format
        writeln!(code, r#"format ELF64"#);
        writeln!(code);

        // code section
        writeln!(code, r#"section '.text' executable"#);
        writeln!(code);
        // make start visible
        writeln!(code, r#"public _start"#);
        writeln!(code);
        // declare external functions
        writeln!(code, r#"extrn printf"#);
        writeln!(code, r#"extrn exit"#);
        writeln!(code);
        // code starts
        writeln!(code, r#"_start:"#);

        codegen_definition(&mut ir, &mut code);

        // data section
        writeln!(code, r#"section '.data' writeable"#);
        writeln!(code);
        writeln!(code, r#"fmt_int: db "%d", 0"#);
        writeln!(code, r#"fmt_char: db "%c", 0"#);
        writeln!(code, r#"fmt_string: db "%s", 0"#);

        for (str_content, str_label) in ir.state.strs {
            let str_content = str_content.replace('\n', "\", 10, \"");
            writeln!(code, r#"{str_label}: db "{str_content}", 0"#);
        }

        if !errors.is_empty() {
            return Err(errors.join("\n"));
        }

        Ok(code)
    }

    fn scratch_alloc(&mut self) -> u32 {
        for i in 0..self.regs.len() {
            if !self.regs[i] {
                self.regs[i] = true;
                return i as u32;
            }
        }
        u32::MAX
    }

    fn scratch_free(&mut self, r: u32) {
        if (r as usize) < self.regs.len() {
            self.regs[r as usize] = false;
        }
    }

    fn scratch_name(&self, r: u32) -> String {
        if (r as usize) < self.regs.len() {
            return X86_64::SCRATCH_REGS[r as usize].to_string();
        }
        println!("[PANIC ERROR] Register_Overflow");
        panic!()
    }

    fn label_create(&mut self) -> u32 {
        todo!()
    }

    fn label_name(&self, _l: u32) -> String {
        todo!()
    }
}

fn codegen_definition(ir: &mut Ir, code: &mut String) {
    for def in &mut ir.program {
        match &mut def.val {
            DefinitionValue::FunctionBody(stmts) => {
                for stmt in stmts {
                    codegen_stmt(&mut ir.state, stmt, code);
                }
            }
        }
    }
}

fn codegen_stmt(state: &mut IrState, stmt: &mut Statement, code: &mut String) {
    match stmt {
        Statement::Declaration(d) => codegen_decl(state, d, code),
        Statement::Expression(e) => codegen_expr(state, e, code),
        Statement::Print(p) => codegen_print(state, p, code),
        Statement::Return(r) => codegen_return(state, r, code),
    }
}

fn codegen_decl(state: &mut IrState, decl: &mut Declaration, code: &mut String) {
    match &mut decl.val {
        DeclarationValue::Uninitialized => (),
        DeclarationValue::Expression(expr) => codegen_expr(state, expr, code),
        DeclarationValue::FunctionBody(stmts) => {
            for stmt in stmts {
                codegen_stmt(state, stmt, code);
            }
        }
    }
}

fn codegen_expr(state: &mut IrState, expr: &mut Expression, code: &mut String) {
    println!("{expr}");
    match &mut expr.kind {
        ExpressionKind::LiteralInteger(i) => {
            state.var_insert(&expr.name, &format!("{i}"));
        }
        ExpressionKind::LiteralString(s) => {
            let str_label = format!("str_{}", state.strs.len());
            state.str_insert(s, &str_label);
        }
        ExpressionKind::Identifier(_) => todo!(),
    }
}

fn codegen_print(state: &mut IrState, exprs: &[Expression], code: &mut String) {
    for expr in exprs {
        match &expr.kind {
            ExpressionKind::LiteralInteger(i) => {
                state.var_insert(&expr.name, &format!("{i}"));
            }
            ExpressionKind::LiteralString(s) => {
                let str_label = format!("str_{}", state.strs.len());
                state.str_insert(s, &str_label);

                writeln!(code, r#"mov rdi, fmt_string"#);
                writeln!(code, r#"mov rsi, {}"#, str_label);
                writeln!(code, r#"call printf"#);
            }
            ExpressionKind::Identifier(id) => {
                let Some(s) = state.var_get(id) else {
                    println!("[ERROR]");
                    println!("VARS:\n{:?}", state.vars);
                    println!("STRS:\n{:?}", state.strs);
                    panic!("Symbol {id:?} should be resolved by now");
                };

                writeln!(code, r#"mov rdi, fmt_int"#);
                writeln!(code, r#"mov rsi, {s}"#);
                writeln!(code, r#"call printf"#);
            }
        }
    }
}

fn codegen_return(state: &mut IrState, r: &mut Expression, code: &mut String) {
    codegen_expr(state, r, code);

    writeln!(code, r#"mov rdi, 0"#);
    writeln!(code, r#"call exit"#);
    writeln!(code);
}
// --- ---
