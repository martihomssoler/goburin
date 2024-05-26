#![feature(let_chains)]
#![feature(if_let_guard)]
#![allow(dead_code, unused)]

use std::{
    env,
    error::Error,
    fmt::Display,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
};

type CompilerResult<T> = Result<T, CompilerError>;

fn main() -> CompilerResult<()> {
    let mut args = env::args();
    let count = args.len();
    match count {
        // 1 => interpreter::run_prompt()?,
        2 => compile_file(&args.nth(1).unwrap().into())?,
        _ => {
            println!("Error: Wrong number of arguments provided.");
            println!("Usage: cargo run --[script]");
        }
    }
    Ok(())
}

pub fn compile_file(file_path: &PathBuf) -> CompilerResult<()> {
    let file_content = std::fs::read_to_string(file_path)?;
    // generate AST
    let ast = frontend::generate_ast(&file_content)?;
    // generate IR
    let ir = middleend::generate_ir(ast)?;
    // generate ASM
    let asm = backend::generate_asm(ir)?;

    compile_asm(file_path, asm)?;

    Ok(())
}

fn compile_asm(file_path: &Path, asm: String) -> CompilerResult<()> {
    let output_path = file_path.with_extension("asm");
    let mut output = File::create(output_path.clone())?;

    output.write_all(asm.as_bytes())?;

    let file_dir = output_path.parent().unwrap().to_str().unwrap();
    let basename = output_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
        .unwrap()
        .0
        .to_string();
    let mut asm_file = basename.clone();
    asm_file.push_str(".asm");
    let mut obj_file = basename.clone();
    obj_file.push_str(".o");

    Command::new("nasm")
        .arg("-felf64")
        .arg(asm_file)
        .current_dir(file_dir)
        .spawn()?
        .wait()?;
    Command::new("ld")
        .arg("-o")
        .arg(basename)
        .arg(obj_file)
        .current_dir(file_dir)
        .spawn()?
        .wait()?;

    Ok(())
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CompilerError {
    LexerError,
    ParserError,
    SemanticError,
    IRError,
    ASMError,
    Generic(String),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl From<io::Error> for CompilerError {
    fn from(value: io::Error) -> Self {
        CompilerError::Generic(format!("{value}"))
    }
}

pub mod backend {
    use crate::middleend::{Op, Value, Variable};
    use crate::{middleend::IR, CompilerResult};
    use core::panic;
    use std::fmt::Write;
    use std::path::PathBuf;

    pub struct ASM {}
    /// Generate x86 assembly, in the future there will be a parameter to define which ASM to generate
    pub fn generate_asm(ir: IR) -> CompilerResult<String> {
        let ir = register_allocation(ir);
        let asm = write_asm(ir);

        Ok(asm)
    }

    fn register_allocation(mut ir: IR) -> IR {
        let mut interf_graph = InterferenceGraph::new(1 + ir.temps as usize);

        // x86 specific general purpose registers
        let registers = vec!["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"];

        // build interference graph
        for (op_idx, op) in ir.ops.iter().enumerate() {
            match op {
                Op::Assignment { res, val } => {
                    let res = get_temp(res);
                    interf_graph.lifetimes[res].push(op_idx);
                }
                Op::Minus { res, left, right }
                | Op::Plus { res, left, right }
                | Op::Star { res, left, right }
                | Op::Slash { res, left, right } => {
                    let res = get_temp(res);
                    let left = get_temp(left);
                    let right = get_temp(right);

                    interf_graph.lifetimes[res].push(op_idx);
                    interf_graph.lifetimes[left].push(op_idx);
                    interf_graph.lifetimes[right].push(op_idx);
                }
                Op::Syscall { var, .. } => {
                    let var = get_temp(var);
                    interf_graph.lifetimes[var].push(op_idx);
                }
            }
        }

        // color interference graph
        interf_graph.allocate_regs(registers);

        // assign registers to the ops
        for op in &mut ir.ops {
            match op {
                Op::Assignment { res, .. } => {
                    set_register(&interf_graph, res);
                }
                Op::Minus { res, left, right }
                | Op::Plus { res, left, right }
                | Op::Star { res, left, right }
                | Op::Slash { res, left, right } => {
                    set_register(&interf_graph, res);
                    set_register(&interf_graph, left);
                    set_register(&interf_graph, right);
                }
                Op::Syscall { var, .. } => {
                    set_register(&interf_graph, var);
                }
            }
        }

        ir
    }

    fn set_register(interf_graph: &InterferenceGraph, res: &mut Variable) {
        if let Variable::Temporary(t) = res {
            let register = interf_graph.regs[*t as usize].clone();
            *res = Variable::Register(register);
        }
    }

    fn get_temp(res: &Variable) -> usize {
        if let Variable::Temporary(t) = res {
            *t as usize
        } else {
            0
        }
    }

    struct InterferenceGraph {
        lifetimes: Vec<Vec<usize>>,
        regs: Vec<String>,
    }

    impl InterferenceGraph {
        pub fn new(temps: usize) -> Self {
            Self {
                lifetimes: vec![Vec::new(); temps],
                regs: vec![String::new(); temps],
            }
        }

        pub fn allocate_regs(&mut self, available_regs: Vec<&str>) {
            // we skip 0 because _t0 is used for empty values
            self.regs[1] = available_regs[0].to_string();
            for i in 1..self.regs.len() {
                let mut unused_regs = available_regs.clone();
                // get stast and end of lifetime
                let Some(i_start) = self.lifetimes[i].first() else {
                    continue;
                };
                let Some(i_end) = self.lifetimes[i].last() else {
                    continue;
                };
                for j in 1..self.regs.len() {
                    let Some(j_start) = self.lifetimes[j].first() else {
                        continue;
                    };
                    let Some(j_end) = self.lifetimes[j].last() else {
                        continue;
                    };
                    if (i_start <= j_start && j_start <= i_end)
                        || (j_start <= i_start && i_start <= j_end)
                    {
                        unused_regs.retain(|r| !r.eq(&self.regs[j]));
                    }
                }
                self.regs[i] = unused_regs.first().unwrap().to_string();
            }
            println!("{:?}", self.regs);
        }
    }

    fn write_asm(ir: IR) -> String {
        let mut asm = String::new();

        writeln!(asm, "global _start");
        writeln!(asm, "section .text");
        writeln!(asm, "_start:");

        for op in ir.ops {
            match op {
                Op::Assignment { res, val } => {
                    let res = get_register(res);
                    let Value::Constant(n) = val;
                    writeln!(asm, "    mov {res}, {n}");
                }
                Op::Minus { res, left, right } => {
                    let res = get_register(res);
                    let left = if let Variable::Value(Value::Constant(n)) = left {
                        n.to_string()
                    } else {
                        get_register(left)
                    };
                    let right = get_register(right);

                    writeln!(asm, "    ;; -- {left} - {right}");
                    writeln!(asm, "    mov {res}, {left}");
                    writeln!(asm, "    sub {res}, {right}");
                }
                Op::Plus { res, left, right } => {
                    let res = get_register(res);
                    let left = get_register(left);
                    let right = get_register(right);

                    writeln!(asm, "    ;; -- {left} + {right}");
                    writeln!(asm, "    mov {res}, {left}");
                    writeln!(asm, "    add {res}, {right}");
                }
                Op::Star { res, left, right } => {
                    let res = get_register(res);
                    let left = get_register(left);
                    let right = get_register(right);

                    writeln!(asm, "    ;; -- {left} * {right}");
                    writeln!(asm, "    xor rdx, rdx");
                    writeln!(asm, "    mov rax, {left}");
                    writeln!(asm, "    imul rax, {right}");
                    writeln!(asm, "    mov {res}, rax");
                }
                Op::Slash { res, left, right } => {
                    let res = get_register(res);
                    let left = get_register(left);
                    let right = get_register(right);

                    writeln!(asm, "    ;; -- {left} / {right}");
                    writeln!(asm, "    xor rdx, rdx");
                    writeln!(asm, "    mov rax, {left}");
                    writeln!(asm, "    idiv {right}");
                    writeln!(asm, "    mov {res}, rax");
                }
                Op::Syscall { num, var } => {
                    let var = get_register(var);

                    writeln!(asm, "    ;; -- syscall {num}, {var}");
                    writeln!(asm, "    mov rdi, {var}");
                    writeln!(asm, "    mov rax, {num}");
                    writeln!(asm, "    syscall");
                }
            }
        }
        asm
    }

    fn get_register(res: Variable) -> String {
        let Variable::Register(r) = res else {
            panic!("Cannot generate ASM without assigning registers");
        };
        r
    }
}

pub mod middleend {

    use std::fmt::Display;

    use crate::{
        frontend::{
            parser::{Atom, Expr, Stmt},
            AST,
        },
        shared::Operator,
        CompilerResult,
    };
    type StmtIter = std::iter::Peekable<std::vec::IntoIter<Stmt>>;

    pub fn generate_ir(ast: AST) -> CompilerResult<IR> {
        let ir = generate_ops(ast)?;

        let ir = nano_passes(ir)?;

        Ok(ir)
    }

    fn nano_passes(ir: IR) -> CompilerResult<IR> {
        Ok(ir)
    }

    fn generate_ops(ast: AST) -> CompilerResult<IR> {
        let mut stmt_iter: StmtIter = ast.stmts.into_iter().peekable();
        let mut ir = IR {
            ops: Vec::new(),
            temps: 0,
        };

        for stmt in stmt_iter {
            stmt_ops(stmt, &mut ir)?;
        }

        reduce_ops(&mut ir);

        for op in &ir.ops {
            println!("{}", op);
        }
        Ok(ir)
    }

    fn reduce_ops(ops: &mut IR) {}

    fn stmt_ops(stmt: Stmt, ir: &mut IR) -> CompilerResult<()> {
        match stmt {
            Stmt::Expr(expr) => {
                expr_ops(expr, ir)?;
            }
            Stmt::Return(expr) => {
                expr_ops(expr, ir)?;

                ir.ops.push(Op::Syscall {
                    num: 60,
                    var: Variable::Temporary(ir.temps),
                });
            }
        }

        Ok(())
    }

    fn expr_ops(expr: Expr, ir: &mut IR) -> CompilerResult<()> {
        match expr {
            Expr::Atom(a) => match a {
                Atom::Constant(c) => {
                    ir.temps += 1;
                    let r = Op::Assignment {
                        res: Variable::Temporary(ir.temps),
                        val: Value::Constant(c),
                    };
                    ir.ops.push(r);
                }
            },
            Expr::Operation(op, operands) => op_tacs(op, operands, ir)?,
            Expr::Parenthesis(expr) => expr_ops(*expr, ir)?,
        };

        Ok(())
    }

    fn op_tacs(op: Operator, operands: Vec<Expr>, ir: &mut IR) -> CompilerResult<()> {
        assert!(operands.len() <= 2);

        expr_ops(operands[0].clone(), ir)?;
        let left_temp = ir.temps;

        if operands.len() == 1 {
            assert!(op == Operator::Minus);
            ir.temps += 1;
            ir.ops.push(Op::Minus {
                res: Variable::Temporary(ir.temps),
                left: Variable::Value(Value::Constant(0)),
                right: Variable::Temporary(left_temp),
            });

            return Ok(());
        }

        expr_ops(operands[1].clone(), ir)?;
        let right_temp = ir.temps;

        ir.temps += 1;
        let ops = match op {
            Operator::Minus => Op::Minus {
                res: Variable::Temporary(ir.temps),
                left: Variable::Temporary(left_temp),
                right: Variable::Temporary(right_temp),
            },
            Operator::Plus => Op::Plus {
                res: Variable::Temporary(ir.temps),
                left: Variable::Temporary(left_temp),
                right: Variable::Temporary(right_temp),
            },
            Operator::Star => Op::Star {
                res: Variable::Temporary(ir.temps),
                left: Variable::Temporary(left_temp),
                right: Variable::Temporary(right_temp),
            },
            Operator::Slash => Op::Slash {
                res: Variable::Temporary(ir.temps),
                left: Variable::Temporary(left_temp),
                right: Variable::Temporary(right_temp),
            },
        };

        ir.ops.push(ops);

        Ok(())
    }

    pub struct IR {
        pub ops: Vec<Op>,
        pub temps: u8,
    }

    /// Three-address code operations
    #[derive(Debug, Clone)]
    pub enum Op {
        Assignment {
            res: Variable,
            val: Value,
        },
        Minus {
            res: Variable,
            left: Variable,
            right: Variable,
        },
        Plus {
            res: Variable,
            left: Variable,
            right: Variable,
        },
        Star {
            res: Variable,
            left: Variable,
            right: Variable,
        },
        Slash {
            res: Variable,
            left: Variable,
            right: Variable,
        },
        Syscall {
            num: i64,
            var: Variable,
        },
    }

    #[derive(Debug, Clone)]
    pub enum Variable {
        Temporary(u8),
        Register(String),
        Value(Value),
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Constant(i64),
    }

    impl Display for Op {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Op::Assignment { res, val } => write!(f, "{res} := {val}",),
                Op::Minus { res, left, right } => write!(f, "{res} := sub {left} {right}",),
                Op::Plus { res, left, right } => write!(f, "{res} := add {left} {right}",),
                Op::Star { res, left, right } => write!(f, "{res} := mul {left} {right}",),
                Op::Slash { res, left, right } => write!(f, "{res} := div {left} {right}",),
                Op::Syscall { num, var } => write!(f, "_t0 := syscall {num} {var}",),
            }
        }
    }

    impl Display for Variable {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Variable::Temporary(t) => write!(f, "_t{t}"),
                Variable::Register(r) => write!(f, "%{r}"),
                Variable::Value(v) => write!(f, "{v}"),
            }
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Constant(c) => write!(f, "{c}"),
            }
        }
    }
}

pub mod frontend {
    pub use self::parser::AST;
    use crate::CompilerResult;

    pub fn generate_ast(source: &str) -> CompilerResult<AST> {
        let tokens = lexer::tokenize(source)?;

        let ast = parser::parse(tokens)?;

        semantic::analyze(ast)
    }

    pub mod semantic {
        use crate::CompilerResult;

        use super::parser::AST;

        /// empty for now
        pub fn analyze(ast: AST) -> CompilerResult<AST> {
            Ok(ast)
        }
    }

    pub mod parser {
        use std::fmt::Display;

        use crate::shared::{Keyword, Operator, Token};
        use crate::{CompilerError, CompilerResult};

        type TokenIter = std::iter::Peekable<std::vec::IntoIter<Token>>;

        pub fn parse(tokens: Vec<Token>) -> CompilerResult<AST> {
            let mut stmts = Vec::new();

            let mut token_iter: TokenIter = tokens.into_iter().peekable();

            while let Some(t) = token_iter.peek()
                && !t.eq(&Token::EOF)
            {
                let stmt = parse_stmt(&mut token_iter)?;
                stmts.push(stmt);
            }

            let ast = AST { stmts };

            Ok(ast)
        }

        fn parse_stmt(token_iter: &mut TokenIter) -> CompilerResult<Stmt> {
            if let Some(t) = token_iter.next() {
                match t {
                    Token::Keyword(Keyword::Return) => {
                        let expr = parse_expr(token_iter, 0)?;
                        consume(token_iter, Token::Semicolon)?;
                        Ok(Stmt::Return(expr))
                    }
                    _ => {
                        let expr = parse_expr(token_iter, 0)?;
                        consume(token_iter, Token::Semicolon)?;
                        Ok(Stmt::Expr(expr))
                    }
                }
            } else {
                // TODO: improve errors
                Err(CompilerError::Generic(
                    "calling parse_stmt with empty iterator".to_owned(),
                ))
            }
        }

        fn parse_expr(token_iter: &mut TokenIter, bind_pow: u8) -> CompilerResult<Expr> {
            let Some(t) = token_iter.next() else {
                // TODO: improve errors
                return Err(CompilerError::Generic(
                    "calling parse_expr with empty iterator".to_owned(),
                ));
            };

            let mut left = match t {
                Token::Operator(op) => {
                    let (_, right_bind_pow) = prefix_bind_pow(&op);
                    let expr = parse_expr(token_iter, right_bind_pow)?;
                    Expr::Operation(op, vec![expr])
                }
                Token::LeftParenthesis => {
                    let expr = parse_expr(token_iter, 0)?;
                    consume(token_iter, Token::RightParenthesis)?;
                    Expr::Parenthesis(Box::new(expr))
                }
                Token::Number(n) => Expr::Atom(Atom::Constant(n)),
                t => {
                    panic!("wrong token {t}")
                }
            };

            loop {
                let Some(t) = token_iter.peek() else {
                    // TODO: improve errors
                    return Err(CompilerError::Generic(
                        "calling parse_expr with empty iterator".to_owned(),
                    ));
                };

                let op = match t {
                    Token::Operator(op) => op,
                    _ => break,
                };

                // now that we know that the `op` is a correct operator, we call next and clone.
                let op = op.clone();

                if let Some((left_bind_pow, _)) = postfix_bind_pow(&op) {
                    if left_bind_pow < bind_pow {
                        break;
                    }

                    continue;
                }
                if let Some((left_bind_pow, right_bind_pow)) = infix_bind_pow(&op) {
                    if (left_bind_pow < bind_pow) {
                        break;
                    }

                    token_iter.next();
                    let right = parse_expr(token_iter, right_bind_pow)?;
                    left = Expr::Operation(op, vec![left, right]);
                    continue;
                }

                break;
            }

            Ok(left)
        }

        // TODO: make a single function for the binding powers
        fn postfix_bind_pow(op: &Operator) -> Option<(u8, ())> {
            None
        }

        fn prefix_bind_pow(op: &Operator) -> ((), u8) {
            match op {
                Operator::Plus | Operator::Minus => ((), 5),
                _ => panic!("bad prefix op: {op:?}"),
            }
        }

        fn infix_bind_pow(op: &Operator) -> Option<(u8, u8)> {
            // as a general rule we use and odd number for the bare priority and bump it up by one for associativity
            let res = match op {
                Operator::Plus | Operator::Minus => (1, 2),
                Operator::Star | Operator::Slash => (3, 4),
                _ => return None,
            };
            Some(res)
        }

        fn consume(token_iter: &mut TokenIter, expected: Token) -> CompilerResult<Token> {
            if let Some(actual) = token_iter.peek()
                && actual.eq(&expected)
            {
                Ok(token_iter.next().unwrap())
            } else {
                Err(CompilerError::Generic(format!(
                    "Expected '{expected}' but got '{:?}'",
                    token_iter.peek()
                )))
            }
        }

        #[derive(Debug)]
        pub struct AST {
            pub stmts: Vec<Stmt>,
        }

        #[derive(Debug, Clone)]
        pub enum Stmt {
            Expr(Expr),
            Return(Expr),
        }

        #[derive(Debug, Clone)]
        pub enum Expr {
            Atom(Atom),
            Parenthesis(Box<Expr>),
            Operation(Operator, Vec<Expr>),
        }

        #[derive(Debug, Clone, Copy)]
        pub enum Atom {
            Constant(i64),
        }

        impl Display for Stmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Stmt::Expr(expr) => write!(f, "{expr}"),
                    Stmt::Return(expr) => write!(f, "(return {expr})"),
                }
            }
        }

        impl Display for Expr {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Expr::Atom(a) => write!(f, "{a}"),
                    Expr::Parenthesis(expr) => write!(f, "{expr}"),
                    Expr::Operation(op, exprs) => write!(
                        f,
                        "({op} {})",
                        exprs
                            .iter()
                            .enumerate()
                            .map(|(i, expr)| {
                                if i < exprs.len() - 1 {
                                    format!("{expr} ")
                                } else {
                                    format!("{expr}")
                                }
                            })
                            .collect::<String>()
                    ),
                }
            }
        }

        impl Display for Operator {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Operator::Minus => write!(f, "-"),
                    Operator::Plus => write!(f, "+"),
                    Operator::Star => write!(f, "*"),
                    Operator::Slash => write!(f, "/"),
                }
            }
        }

        impl Display for Atom {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Atom::Constant(c) => write!(f, "{c}"),
                }
            }
        }
    }

    pub mod lexer {
        pub fn tokenize(source: &str) -> CompilerResult<Vec<Token>> {
            let mut iter = source.chars().peekable();

            let mut tokens = Vec::new();
            loop {
                let Some(c) = iter.next() else {
                    break;
                };

                let t = match c {
                    ' ' => continue,
                    '-' => Token::Operator(Operator::Minus),
                    '+' => Token::Operator(Operator::Plus),
                    '*' => Token::Operator(Operator::Star),
                    '/' => {
                        if iter.peek().is_some_and(|nt| '/'.eq(nt)) {
                            while let Some(nt) = iter.next()
                                && !'\n'.eq(&nt)
                            {}
                            continue;
                        } else {
                            Token::Operator(Operator::Slash)
                        }
                    }
                    '(' => Token::LeftParenthesis,
                    ')' => Token::RightParenthesis,
                    // '{' => Token::LeftBrace,
                    // '}' => Token::RightBrace,
                    ';' => Token::Semicolon,
                    d if d.is_ascii_digit() => {
                        let mut number = c.to_string();
                        while let Some(c) = iter.peek()
                            && c.is_ascii_digit()
                        {
                            number.push(*c);
                            iter.next();
                        }

                        let value = number.parse::<i64>().unwrap();
                        Token::Number(value)
                    }
                    c if c.is_alphabetic() => {
                        let mut id = c.to_string();
                        while let Some(c) = iter.peek()
                            && (c.is_ascii_alphanumeric() || c.eq(&'_'))
                        {
                            id.push(*c);
                            iter.next();
                        }

                        if let Some(keyword) = get_keyword(&id) {
                            keyword
                        } else {
                            panic!("{id} is not a keyword");
                            // Token::Identifier(id)
                        }
                    }
                    _ => continue,
                };

                tokens.push(t);
            }

            tokens.push(Token::EOF);

            Ok(tokens)
        }

        fn get_keyword(id: &str) -> Option<Token> {
            match id {
                "return" => Some(Token::Keyword(Keyword::Return)),
                _ => None,
            }
        }

        use crate::{
            shared::{Keyword, Operator, Token},
            CompilerResult,
        };
        use std::fmt::Display;
    }
}

pub mod shared {
    use std::fmt::Display;

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Operator {
        Minus,
        Plus,
        Star,
        Slash,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Keyword {
        Return,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Token {
        // Operators
        Operator(Operator),
        // Literals
        Number(i64),
        // Keywords
        Keyword(Keyword),
        // Others
        Semicolon,
        LeftParenthesis,
        RightParenthesis,
        // EOF
        EOF,
    }

    impl Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                // Token::Identifier(id) => write!(f, "{id}"),
                Token::Number(n) => write!(f, "{n}"),
                Token::Semicolon => write!(f, ";"),
                Token::LeftParenthesis => write!(f, "("),
                Token::RightParenthesis => write!(f, ")"),
                Token::EOF => write!(f, "EOF"),
                Token::Operator(o) => match o {
                    Operator::Minus => write!(f, "-"),
                    Operator::Plus => write!(f, "+"),
                    Operator::Star => write!(f, "*"),
                    Operator::Slash => write!(f, "/"),
                },
                Token::Keyword(k) => match k {
                    Keyword::Return => write!(f, "return"),
                },
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::{
        fs::File,
        io::{self, Write},
        path::PathBuf,
        process::Command,
    };

    use crate::compile_file;

    #[test]
    fn goburin_compiler() -> io::Result<()> {
        let mut tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        tests_dir.push("tests/");
        for entry in tests_dir.read_dir()? {
            let test = entry?;
            let test_path = test.path();
            if !test_path.is_dir()
                && let Some(ext) = test_path.extension()
                && ext.eq("gobo")
            {
                let filename = test_path.file_stem().unwrap().to_str().unwrap();
                println!("[Compiling {:?}]", filename);
                let res = compile_file(&test_path);
                assert_eq!(Ok(()), res);

                println!("[Removing files {:?}]", filename);
                let output = Command::new("rm")
                    .arg(filename)
                    .arg(format!("{filename}.o"))
                    .arg(format!("{filename}.asm"))
                    .current_dir(tests_dir.clone())
                    .status()
                    .unwrap()
                    .code();
            }
        }

        Ok(())
    }
} // pub mod compiler {
  //     use std::{error::Error, fs::File, path::PathBuf, process::Command};

//     use self::ir_generator::generate_ir;

//     pub fn compile_file(file_path: PathBuf) -> Result<(), Box<dyn Error>> {
//         let file_content = std::fs::read_to_string(file_path.clone())?;

//         let tokens = tokenizer::tokenize(&file_content);
//         let program = parser::parse(tokens);

//         let output_path = file_path.with_extension("asm");
//         let mut output = File::create(output_path.clone())?;
//         generate_ir(output, program);

//         let file_dir = output_path.parent().unwrap().to_str().unwrap();
//         let basename = output_path
//             .file_name()
//             .unwrap()
//             .to_str()
//             .unwrap()
//             .split_once('.')
//             .unwrap()
//             .0
//             .to_string();
//         let mut asm_file = basename.clone();
//         asm_file.push_str(".asm");
//         let mut obj_file = basename.clone();
//         obj_file.push_str(".o");

//         Command::new("nasm")
//             .arg("-felf64")
//             .arg(asm_file)
//             .current_dir(file_dir)
//             .spawn()
//             .unwrap();
//         Command::new("ld")
//             .arg("-o")
//             .arg(basename)
//             .arg(obj_file)
//             .current_dir(file_dir)
//             .spawn()
//             .unwrap();

//         Ok(())
//     }

//     pub mod parser {
//         use core::panic;
//         use std::{any::Any, iter::Peekable, vec::IntoIter};

//         use super::tokenizer::Token;

//         pub struct Program {
//             pub name: String,
//             pub stmts: Vec<Statement>,
//         }
//         pub enum Statement {
//             Print(Expr),
//             Return(Expr),
//         }
//         pub enum Expr {
//             Sum(i64, i64),
//             Constant(i64),
//         }

//         type TokenIter = Peekable<IntoIter<Token>>;

//         pub fn parse(tokens: Vec<Token>) -> Program {
//             let mut token_iter = tokens.into_iter().peekable();

//             let name = consume(&mut token_iter, Token::MainId);
//             consume(&mut token_iter, Token::LeftParenthesis);
//             consume(&mut token_iter, Token::RightParenthesis);
//             consume(&mut token_iter, Token::LeftBrace);
//             let stmt = parse_stmts(&mut token_iter);
//             consume(&mut token_iter, Token::RightBrace);

//             Program {
//                 name: name.to_string(),
//                 stmts: stmt,
//             }
//         }

//         fn parse_stmts(token_iter: &mut TokenIter) -> Vec<Statement> {
//             let mut stmts = Vec::new();
//             while let Some(t) = token_iter.peek()
//                 && t != &Token::RightBrace
//             {
//                 let stmt = parse_stmt(token_iter);
//                 stmts.push(stmt);
//             }
//             stmts
//         }

//         fn parse_stmt(token_iter: &mut TokenIter) -> Statement {
//             if let Some(t) = token_iter.next() {
//                 match t {
//                     Token::Return => {
//                         let expr = parse_expression(token_iter);
//                         consume(token_iter, Token::Semicolon);
//                         Statement::Return(expr)
//                     }
//                     Token::Print => {
//                         let expr = parse_expression(token_iter);
//                         consume(token_iter, Token::Semicolon);
//                         Statement::Print(expr)
//                     }
//                     _ => panic!("Expected '' but got '{t:?}'"),
//                 }
//             } else {
//                 panic!("")
//             }
//         }

//         fn parse_expression(token_iter: &mut TokenIter) -> Expr {
//             if let Some(t) = token_iter.next() {
//                 match t {
//                     Token::Number(n) => Expr::Constant(n),
//                     _ => todo!(),
//                 }
//             } else {
//                 panic!("")
//             }
//         }

//         fn consume(token_iter: &mut TokenIter, expected: Token) -> Token {
//             if let Some(token) = token_iter.peek()
//                 && token.type_id().eq(&expected.type_id())
//             {
//                 let token = token.clone();
//                 token_iter.next();
//                 token
//             } else {
//                 panic!("Expected {expected:?} but got {:?}", token_iter.peek());
//             }
//         }
//     }

//     pub mod ir_generator {
//         use super::parser::{Expr, Program, Statement};
//         use std::{fs::File, io::Write};

//         pub fn generate_ir(mut output: File, program: Program) {
//             let Program { name, stmts } = program;
//             assert_eq!(name.as_str(), "main");

//             writeln!(output, "global _start");
//             writeln!(output, "section .text");
//             writeln!(output, "_start:");

//             generate_stmts(&mut output, stmts);

//             output.sync_data();
//         }

//         fn generate_stmts(output: &mut File, stmts: Vec<Statement>) {
//             for stmt in stmts {
//                 match stmt {
//                     Statement::Return(expr) => {
//                         writeln!(output, "    mov rdi, {}", resolve_expr(expr));
//                         writeln!(output, "    mov rax, 60");
//                         writeln!(output, "    syscall");
//                     }
//                     Statement::Print(expr) => {
//                         writeln!(output, "    mov rdi, {}", resolve_expr(expr));
//                         writeln!(output, "    mov rax, 1");
//                         writeln!(output, "    syscall");
//                     }
//                 }
//             }
//         }

//         fn resolve_expr(expr: Expr) -> String {
//             match expr {
//                 Expr::Constant(i) => i.to_string(),
//                 Expr::Sum(_, _) => todo!(),
//             }
//         }
//     }
// }
