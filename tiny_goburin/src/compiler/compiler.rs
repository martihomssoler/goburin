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

use std::path::{Path, PathBuf};

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
pub fn c_compile_file(stage: u32, input_path: PathBuf, output_path: PathBuf) -> Result<(), String> {
    let stage_0 = |path: &Path| -> Result<TokenList, String> {
        SourceFile::new(path)?
            //
            .p0_0_tokenize()
    };
    let stage_1 = |tokens: TokenList| -> Result<Ast, String> {
        tokens
            //
            .p1_0_parse()?
            .p1_1_name_resolution()?
            .p1_2_static_type_checking()
    };
    let stage_2 = |ast: Ast| -> Result<Ir, String> {
        ast
            //
            .p2_0_ir()
    };
    let stage_3 = |ir: Ir| -> Result<(), String> {
        ir
            //
            .p3_0_codegen(&input_path, &mut X86_64::default())
    };

    // Logic for saving the intermediate passes between stages
    // apply one after the others and save at the end
    match stage {
        0 => stage_0(&input_path)?.save(output_path)?,
        1 => stage_1(stage_0(&input_path)?)?.save(output_path),
        2 => stage_2(stage_1(stage_0(&input_path)?)?)?.save(output_path),
        s => stage_3(stage_2(stage_1(stage_0(&input_path)?)?)?)?,
    }
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
impl Ast {
    fn save(&self, output: PathBuf) {}
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
    Block(Vec<Statement>),
    Conditional(Conditional),
    Declaration(Declaration),
    Assignment(Assignment),
    Expression(Expression),
    Loop(Loop),
    Print(Vec<Expression>),
    Return(Expression),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conditional {
    pub control_expr: Expression,
    pub if_body: Vec<Statement>,
    pub else_body_opt: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Loop {
    pub init_expr_opt: Option<Expression>,
    pub control_expr_opt: Option<Expression>,
    pub next_expr_opt: Option<Expression>,
    pub loop_body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub typ: Type,
    pub val: DeclarationValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assignment {
    pub name: String,
    pub val: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationValue {
    Uninitialized,
    Expression(Expression),
    FunctionBody(Vec<Statement>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    // TODO(mhs): what is the `name` used for? I think it is for symbol lookup
    pub name: String,
    pub kind: ExpressionKind,
    pub node: AstNode,
    pub left: Option<Box<Expression>>,
    pub right: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Identifier(String),
    LiteralBoolean(bool),
    LiteralCharacter(char),
    LiteralInteger(i64),
    LiteralString(String),
    BinaryOpAssignment,
    BinaryOpAdd,
    BinaryOpSub,
    BinaryOpMul,
    BinaryOpDiv,
    BinaryOpLower,
    /// a[b] -> left: a, right: b
    BinaryOpArrayAccess,
    UnaryOpNeg,
    UnaryOpNot,
    FuncCall,
    FuncArg,
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
            ExpressionKind::LiteralBoolean(_) => todo!(),
            ExpressionKind::LiteralCharacter(_) => todo!(),
            ExpressionKind::BinaryOpAdd => todo!(),
            ExpressionKind::BinaryOpSub => todo!(),
            ExpressionKind::BinaryOpMul => todo!(),
            ExpressionKind::BinaryOpDiv => todo!(),
            ExpressionKind::UnaryOpNeg => todo!(),
            ExpressionKind::UnaryOpNot => todo!(),
            ExpressionKind::BinaryOpArrayAccess => todo!(),
            ExpressionKind::FuncCall => todo!(),
            ExpressionKind::FuncArg => todo!(),
            ExpressionKind::BinaryOpAssignment => todo!(),
            ExpressionKind::BinaryOpLower => todo!(),
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
    // FIXME(mhs): cannot create "Array [10] Integer", just "Array Integer"
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
        write!(fmt, "{}", self.kind)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
                Operator::Arrow => write!(fmt, "Symbol ->"),
                Operator::Bang => write!(fmt, "Symbol !"),
                Operator::BangEqual => write!(fmt, "Symbol !="),
                Operator::BraceLeft => write!(fmt, "Symbol {{"),
                Operator::BraceRight => write!(fmt, "Symbol }}"),
                Operator::BracketLeft => write!(fmt, "Symbol ["),
                Operator::BracketRight => write!(fmt, "Symbol ]"),
                Operator::Caret => write!(fmt, "Symbol ^"),
                Operator::Colon => write!(fmt, "Symbol :"),
                Operator::Comma => write!(fmt, "Symbol ,"),
                Operator::Equal => write!(fmt, "Symbol ="),
                Operator::EqualEqual => write!(fmt, "Symbol =="),
                Operator::Greater => write!(fmt, "Symbol >"),
                Operator::GreaterEqual => write!(fmt, "Symbol >="),
                Operator::LogicAnd => write!(fmt, "Symbol &&"),
                Operator::LogicOr => write!(fmt, "Symbol ||"),
                Operator::Lower => write!(fmt, "Symbol <"),
                Operator::LowerEqual => write!(fmt, "Symbol <="),
                Operator::Minus => write!(fmt, "Symbol -"),
                Operator::ParenthesisLeft => write!(fmt, "Symbol ("),
                Operator::ParenthesisRight => write!(fmt, "Symbol )"),
                Operator::Percent => write!(fmt, "Symbol %"),
                Operator::Plus => write!(fmt, "Symbol +"),
                Operator::Point => write!(fmt, "Symbol ."),
                Operator::Semicolon => write!(fmt, "Symbol ;"),
                Operator::Slash => write!(fmt, "Symbol /"),
                Operator::Star => write!(fmt, "Symbol *"),
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
impl Ir {
    fn save(&self, output: PathBuf) {}
}

pub struct IrState {
    pub symbol_table: SymbolTable,
    pub vars: Vec<(String, String)>,
    pub strs: Vec<(String, String)>,
}

impl IrState {
    pub fn var_insert(&mut self, name: &str, val: &str) {
        if let Some(old) = self.var_get_mut(name) {
            val.clone_into(old);
        } else {
            self.vars.push((name.to_owned(), val.to_owned()));
        }
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

    fn var_get_mut(&mut self, name: &str) -> Option<&mut String> {
        for i in (0..self.vars.len()).rev() {
            let curr_var = &self.vars[i];

            if curr_var.0.eq(name) {
                return Some(&mut self.vars[i].1);
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
