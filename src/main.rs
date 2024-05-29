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
    let qbe = backend::generate_qbe(ir)?;

    compile_qbe(file_path, qbe)?;

    Ok(())
}

fn compile_qbe(file_path: &Path, qbe: String) -> CompilerResult<()> {
    let output_path = file_path.with_extension("ssa");
    let mut output = File::create(output_path.clone())?;

    output.write_all(qbe.as_bytes())?;

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
    let mut qbe_file = basename.clone();
    qbe_file.push_str(".ssa");
    let mut asm_file = basename.clone();
    asm_file.push_str(".s");

    // qbe -o file.s file.ssa
    Command::new("qbe")
        .arg("-o")
        .arg(asm_file.clone())
        .arg(qbe_file)
        .current_dir(file_dir)
        .spawn()?
        .wait()?;

    // cc -o file file.s
    Command::new("cc")
        .arg("-o")
        .arg(basename)
        .arg(asm_file)
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
    QBEError,
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
    use crate::frontend::parser::Constant;
    use crate::middleend::{Op, Variable};
    use crate::{middleend::IR, CompilerResult};
    use core::panic;
    use std::fmt::Write;
    use std::path::PathBuf;

    pub struct QBE {}
    /// Generate x86 assembly, in the future there will be a parameter to define which QBE to generate
    pub fn generate_qbe(ir: IR) -> CompilerResult<String> {
        let qbe = write_qbe(ir);

        Ok(qbe)
    }

    fn write_qbe(ir: IR) -> String {
        let mut qbe = String::new();

        writeln!(qbe, r#"export function w $main() {{"#);
        writeln!(qbe, r#"@start"#);

        for op in ir.ops {
            match op {
                Op::Assignment { res, val } => {
                    let res = get_variable(res);
                    let val = get_value(val);
                    writeln!(qbe, "    {res} =w copy {val}");
                }
                Op::Minus { res, left, right } => {
                    let res = get_variable(res);
                    let left = get_variable(left);
                    let right = get_variable(right);

                    writeln!(qbe, "    # -- {left} - {right}");
                    writeln!(qbe, "    {res} =w sub {left}, {right}");
                }
                Op::Plus { res, left, right } => {
                    let res = get_variable(res);
                    let left = get_variable(left);
                    let right = get_variable(right);

                    writeln!(qbe, "    # -- {left} + {right}");
                    writeln!(qbe, "    {res} =w add {left}, {right}");
                }
                Op::Star { res, left, right } => {
                    let res = get_variable(res);
                    let left = get_variable(left);
                    let right = get_variable(right);

                    writeln!(qbe, "    # -- {left} * {right}");
                    writeln!(qbe, "    {res} =w mul {left}, {right}");
                }
                Op::Slash { res, left, right } => {
                    let res = get_variable(res);
                    let left = get_variable(left);
                    let right = get_variable(right);

                    writeln!(qbe, "    # -- {left} / {right}");
                    writeln!(qbe, "    {res} =w div {left}, {right}");
                }
                Op::Return { var } => {
                    let var = get_variable(var);

                    writeln!(qbe, "    # -- return {var}");
                    writeln!(qbe, "    ret {var}");
                }
                Op::Print { var } => {
                    let var = get_variable(var);

                    writeln!(qbe, "    # -- print {var}");
                    writeln!(qbe, "    call $printf(l $fmt_int, ..., w {var})");
                }
            }
        }

        writeln!(qbe, r#"}}"#);
        writeln!(qbe);
        writeln!(qbe, r#"data $fmt_int = {{ b "%d\n", b 0 }}"#);
        writeln!(qbe, r#"data $fmt_float = {{ b "%.6f\n", b 0 }}"#);

        qbe
    }

    fn get_value(val: Constant) -> String {
        match val {
            Constant::Integer(i) => format!("{i}"),
            Constant::Float(f) => format!("s_{f}"),
        }
    }

    fn get_variable(res: Variable) -> String {
        match res {
            Variable::Temporary(t) => format!("%_t{t}"),
            Variable::Value(v) => get_value(v),
        }
    }
}

pub mod middleend {

    use std::fmt::Display;

    use crate::{
        frontend::{
            parser::{Atom, Constant, Expr, Stmt},
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

        Ok(ir)
    }

    #[allow(irrefutable_let_patterns)]
    fn reduce_ops(ir: &mut IR) {
        // constant folding?
        let ops_count = ir.ops.len();
        for op_idx in 0..ops_count {
            let op = ir.ops[op_idx].clone();
            let mut reduced = false;
            match op {
                Op::Assignment { res, val } if let Variable::Temporary(t) = res => {
                    // found an assignment w/ a constant
                    for other_op_idx in op_idx..ops_count {
                        match &mut ir.ops[other_op_idx] {
                            Op::Minus { left, right, .. }
                            | Op::Plus { left, right, .. }
                            | Op::Star { left, right, .. }
                            | Op::Slash { left, right, .. }
                                if res.eq(left) || res.eq(right) =>
                            {
                                reduced = true;
                                if res.eq(left) {
                                    *left = Variable::Value(val);
                                } else {
                                    *right = Variable::Value(val);
                                }
                            }
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
            if reduced
                && let Op::Assignment { res, val } = &mut ir.ops[op_idx]
                && let Variable::Temporary(t) = res
            {
                *t = 0;
            }
        }

        #[allow(clippy::match_like_matches_macro)]
        ir.ops.retain(|f| match f {
            Op::Assignment { res, val }
                if let Variable::Temporary(t) = res
                    && *t == 0 =>
            {
                false
            }
            _ => true,
        })
    }

    fn stmt_ops(stmt: Stmt, ir: &mut IR) -> CompilerResult<()> {
        match stmt {
            Stmt::Expr(expr) => {
                expr_ops(expr, ir)?;
            }
            Stmt::Return(expr) => {
                expr_ops(expr, ir)?;

                ir.ops.push(Op::Return {
                    var: Variable::Temporary(ir.temps),
                });
            }
            Stmt::Dump(expr) => {
                expr_ops(expr, ir)?;

                ir.ops.push(Op::Print {
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
                        val: c,
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
                left: Variable::Value(Constant::Integer(0)),
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
            val: Constant,
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
        Return {
            var: Variable,
        },
        Print {
            var: Variable,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Variable {
        Temporary(u8),
        Value(crate::frontend::parser::Constant),
    }

    impl Display for Op {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Op::Assignment { res, val } => write!(f, "{res} := {val}",),
                Op::Minus { res, left, right } => write!(f, "{res} := sub {left} {right}",),
                Op::Plus { res, left, right } => write!(f, "{res} := add {left} {right}",),
                Op::Star { res, left, right } => write!(f, "{res} := mul {left} {right}",),
                Op::Slash { res, left, right } => write!(f, "{res} := div {left} {right}",),
                Op::Return { var } => write!(f, "_t0 := return {var}",),
                Op::Print { var } => write!(f, "_t0 := print {var}",),
            }
        }
    }

    impl Display for Variable {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Variable::Temporary(t) => write!(f, "_t{t}"),
                Variable::Value(v) => write!(f, "{v}"),
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
                    Token::Keyword(k) => match k {
                        Keyword::Return => {
                            let expr = parse_expr(token_iter, 0)?;
                            consume(token_iter, Token::Semicolon)?;
                            Ok(Stmt::Return(expr))
                        }
                        Keyword::Dump => {
                            let expr = parse_expr(token_iter, 0)?;
                            consume(token_iter, Token::Semicolon)?;
                            Ok(Stmt::Dump(expr))
                        }
                    },
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
                Token::Integer(i) => Expr::Atom(Atom::Constant(Constant::Integer(i))),
                Token::Float(f) => Expr::Atom(Atom::Constant(Constant::Float(f))),
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
            Dump(Expr),
        }

        #[derive(Debug, Clone)]
        pub enum Expr {
            Atom(Atom),
            Parenthesis(Box<Expr>),
            Operation(Operator, Vec<Expr>),
        }

        #[derive(Debug, Clone, Copy)]
        pub enum Atom {
            Constant(Constant),
        }

        #[derive(Debug, Clone, Copy, PartialEq)]
        pub enum Constant {
            Integer(i64),
            Float(f32),
        }

        impl Display for Stmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Stmt::Expr(expr) => write!(f, "{expr}"),
                    Stmt::Return(expr) => write!(f, "(return {expr})"),
                    Stmt::Dump(expr) => write!(f, "(print {expr})"),
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

        impl Display for Constant {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Constant::Integer(i) => write!(fmt, "{i}"),
                    Constant::Float(f) => write!(fmt, "{f}"),
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
                        if let Some(c) = iter.peek()
                            && '.'.eq(c)
                        {
                            number.push(*c);
                            iter.next();
                            // next token has to be a digit!
                            let c = iter
                                .peek()
                                .expect("Expected a digit after decimal dot `.` but got `EOF`");

                            if !c.is_ascii_digit() {
                                panic!("Expected a digit after decimal dot `.` but got `{c}`");
                            };

                            while let Some(c) = iter.peek()
                                && c.is_ascii_digit()
                            {
                                number.push(*c);
                                iter.next();
                            }

                            let value = number.parse::<f32>().unwrap();
                            Token::Float(value)
                        } else {
                            let value = number.parse::<i64>().unwrap();
                            Token::Integer(value)
                        }
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
                "dump" => Some(Token::Keyword(Keyword::Dump)),
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
        Dump,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Token {
        // Operators
        Operator(Operator),
        // Literals
        Integer(i64),
        Float(f32),
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
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                // Token::Identifier(id) => write!(f, "{id}"),
                Token::Integer(n) => write!(fmt, "{n}"),
                Token::Float(f) => write!(fmt, "{f}"),
                Token::Semicolon => write!(fmt, ";"),
                Token::LeftParenthesis => write!(fmt, "("),
                Token::RightParenthesis => write!(fmt, ")"),
                Token::EOF => write!(fmt, "EOF"),
                Token::Operator(o) => match o {
                    Operator::Minus => write!(fmt, "-"),
                    Operator::Plus => write!(fmt, "+"),
                    Operator::Star => write!(fmt, "*"),
                    Operator::Slash => write!(fmt, "/"),
                },
                Token::Keyword(k) => match k {
                    Keyword::Return => write!(fmt, "return"),
                    Keyword::Dump => write!(fmt, "print"),
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
                println!("[[Compiling {:?}]]", filename);
                let res = compile_file(&test_path);
                assert_eq!(Ok(()), res);

                println!("--> [Removing files {:?}]", filename);
                let output = Command::new("rm")
                    .arg(filename)
                    .arg(format!("{filename}.s"))
                    .arg(format!("{filename}.ssa"))
                    .current_dir(tests_dir.clone())
                    .status()
                    .unwrap()
                    .code();

                println!();
            }
        }

        Ok(())
    }
}
