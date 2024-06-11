#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(variant_count)]
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
                    let val = get_variable(val);
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
                Op::Label { name } => {
                    writeln!(qbe, "@{name}");
                }
                Op::CheckGreater {
                    var,
                    true_label,
                    false_label,
                } => {
                    let var = get_variable(var);

                    writeln!(
                        qbe,
                        "    # -- jump if greater {true_label} else {false_label}"
                    );
                    writeln!(qbe, "    jnz {var}, @{true_label}, @{false_label}");
                }
                Op::Greater { res, left, right } => {
                    let res = get_variable(res);
                    let left = get_variable(left);
                    let right = get_variable(right);

                    writeln!(qbe, "    # -- {left} > {right}");
                    writeln!(qbe, "    {res} =w csgtw {left}, {right}");
                }
                Op::Jump { dest } => {
                    writeln!(qbe, "    # -- jump to {dest}");
                    writeln!(qbe, "    jmp @{dest}");
                }
            }
        }

        writeln!(qbe, r#"}}"#);
        writeln!(qbe);
        writeln!(qbe, r#"data $fmt_int = {{ b "%d\n", b 0 }}"#);

        qbe
    }

    fn get_value(val: Constant) -> String {
        match val {
            Constant::Integer(i) => format!("{i}"),
        }
    }

    fn get_variable(var: Variable) -> String {
        match var {
            Variable::Temporary(t) => format!("%_t{t}"),
            Variable::Value(v) => get_value(v),
            Variable::Named(id) => format!("%var_{id}"),
        }
    }
}

pub mod middleend {

    use std::{collections::HashMap, fmt::Display, ops::Deref};

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
            labels: 0,
            vars: Vec::new(),
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
                                    *left = val.clone();
                                } else {
                                    *right = val.clone();
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
                let var = get_expr_variable(expr, ir)?;

                ir.ops.push(Op::Return { var });
            }
            Stmt::Print(expr) => {
                let var = get_expr_variable(expr, ir)?;

                ir.ops.push(Op::Print { var });
            }
            // TODO(mhs): for now "let" and "mut" are basically the same
            // I want to test how ergonomic it is to declare it that way
            // and I will add constaints later (like "let" is constants)
            Stmt::Let(id, expr_opt) | Stmt::Mut(id, expr_opt) => {
                let var_temp = ir.temps;
                ir.vars.push((id.clone(), var_temp));

                if let Some(expr) = expr_opt {
                    let val = get_expr_variable(expr, ir)?;
                    ir.ops.push(Op::Assignment {
                        res: Variable::Named(id),
                        val,
                    })
                } else {
                    ir.ops.push(Op::Assignment {
                        res: Variable::Named(id),
                        val: Variable::Value(Constant::Integer(0)), // TODO(mhs): for now memory is "zero-initialized"
                    })
                }
            }
            Stmt::Assignment(id, expr) => {
                let val = get_expr_variable(expr, ir)?;

                ir.ops.push(Op::Assignment {
                    res: Variable::Named(id),
                    val,
                });
            }
            Stmt::For(cond, stmts) => {
                let begin_label = format!("begin_label_{}", ir.labels);
                let body_label = format!("body_label_{}", ir.labels);
                let end_label = format!("end_label_{}", ir.labels);
                ir.labels += 1;

                // begin label
                ir.ops.push(Op::Label {
                    name: begin_label.clone(),
                });
                // check condition, jump to end
                let var = get_expr_variable(cond, ir)?;
                ir.ops.push(Op::CheckGreater {
                    var,
                    true_label: body_label.clone(),
                    false_label: end_label.clone(),
                });
                // body label
                ir.ops.push(Op::Label {
                    name: body_label.clone(),
                });
                // body
                for stmt in stmts {
                    stmt_ops(stmt, ir);
                }
                // inconditional jump to start
                ir.ops.push(Op::Jump { dest: begin_label });
                // end label
                ir.ops.push(Op::Label { name: end_label });
            }
        }

        Ok(())
    }

    fn get_expr_variable(expr: Expr, ir: &mut IR) -> CompilerResult<Variable> {
        let res = match expr {
            Expr::Atom(a) => match a {
                Atom::Constant(c) => Variable::Value(c),
                Atom::Identifier(id) => Variable::Named(id),
            },
            Expr::Parenthesis(boxed_expr) => get_expr_variable(*boxed_expr, ir)?,
            Expr::Operation(_, _) => {
                expr_ops(expr, ir)?;
                Variable::Temporary(ir.temps)
            }
        };

        Ok(res)
    }

    fn expr_ops(expr: Expr, ir: &mut IR) -> CompilerResult<()> {
        match expr {
            Expr::Atom(a) => match a {
                Atom::Constant(c) => {
                    ir.temps += 1;
                    let r = Op::Assignment {
                        res: Variable::Temporary(ir.temps),
                        val: Variable::Value(c),
                    };
                    ir.ops.push(r);
                }
                Atom::Identifier(_) => (),
            },
            Expr::Operation(op, operands) => op_tacs(op, operands, ir)?,
            Expr::Parenthesis(expr) => expr_ops(*expr, ir)?,
        };

        Ok(())
    }

    fn op_tacs(op: Operator, operands: Vec<Expr>, ir: &mut IR) -> CompilerResult<()> {
        assert!(operands.len() <= 2);

        let left_var = get_expr_variable(operands[0].clone(), ir)?;

        if operands.len() == 1 {
            assert_eq!(op, Operator::Minus);
            ir.temps += 1;
            ir.ops.push(Op::Minus {
                res: Variable::Temporary(ir.temps),
                left: Variable::Value(Constant::Integer(0)),
                right: left_var,
            });

            return Ok(());
        }

        let right_var = get_expr_variable(operands[1].clone(), ir)?;

        ir.temps += 1;
        let ops = match op {
            Operator::Minus => Op::Minus {
                res: Variable::Temporary(ir.temps),
                left: left_var,
                right: right_var,
            },
            Operator::Plus => Op::Plus {
                res: Variable::Temporary(ir.temps),
                left: left_var,
                right: right_var,
            },
            Operator::Star => Op::Star {
                res: Variable::Temporary(ir.temps),
                left: left_var,
                right: right_var,
            },
            Operator::Slash => Op::Slash {
                res: Variable::Temporary(ir.temps),
                left: left_var,
                right: right_var,
            },
            Operator::Equal => todo!(),
            Operator::Greater => Op::Greater {
                res: Variable::Temporary(ir.temps),
                left: left_var,
                right: right_var,
            },
        };

        ir.ops.push(ops);

        Ok(())
    }

    pub struct IR {
        pub ops: Vec<Op>,
        pub temps: u8,
        pub labels: u8,
        /// Poor mans map of [ var_name => temporary assigned to it ]
        pub vars: Vec<(String, u8)>, // TODO(mhs): improve this, making it dumb so it is easy to self-host later
    }
    impl IR {
        fn get_variable_value(&self, id: &str) -> Option<u8> {
            let ret = None;

            for (var, value) in self.vars.iter() {
                if var.eq(id) {
                    return Some(*value);
                }
            }

            ret
        }
    }

    /// Three-address code operations
    #[derive(Debug, Clone)]
    pub enum Op {
        Assignment {
            res: Variable,
            val: Variable,
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
        Greater {
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
        Label {
            name: String,
        },
        Jump {
            dest: String,
        },
        CheckGreater {
            var: Variable,
            true_label: String,
            false_label: String,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Variable {
        Temporary(u8),
        Named(String),
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
                Op::Label { name } => todo!(),
                Op::CheckGreater {
                    var,
                    true_label,
                    false_label,
                } => todo!(),
                Op::Greater { res, left, right } => todo!(),
                Op::Jump { dest } => todo!(),
            }
        }
    }

    impl Display for Variable {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Variable::Temporary(t) => write!(f, "_t{t}"),
                Variable::Value(v) => write!(f, "{v}"),
                Variable::Named(id) => write!(f, "var_{id}"),
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
        use core::panic;
        use std::fmt::Display;
        use std::mem::discriminant;

        use crate::shared::{Keyword, Operator, Token, TokenKind};
        use crate::{CompilerError, CompilerResult};

        pub struct TokenIter {
            iter: std::iter::Peekable<std::vec::IntoIter<Token>>,
        }

        impl TokenIter {
            pub fn new(iter: std::iter::Peekable<std::vec::IntoIter<Token>>) -> Self {
                Self { iter }
            }

            pub fn peek(&mut self) -> Option<&Token> {
                self.iter.peek()
            }

            #[allow(clippy::should_implement_trait)]
            pub fn next(&mut self) -> Option<Token> {
                self.iter.next()
            }
        }

        pub fn parse(tokens: Vec<Token>) -> CompilerResult<AST> {
            let mut stmts = Vec::new();

            let mut token_iter = TokenIter::new(tokens.into_iter().peekable());

            while let Some(t) = token_iter.peek()
                && !t.kind.eq(&TokenKind::EOF)
            {
                let stmt = parse_stmt(&mut token_iter)?;
                stmts.push(stmt);
            }

            let ast = AST { stmts };

            Ok(ast)
        }

        fn parse_stmt(token_iter: &mut TokenIter) -> CompilerResult<Stmt> {
            if let Some(t) = token_iter.next() {
                match t.kind {
                    TokenKind::Keyword(k) => match k {
                        Keyword::Return => {
                            let expr = parse_expr(token_iter, 0)?;
                            consume(token_iter, TokenKind::Semicolon)?;
                            Ok(Stmt::Return(expr))
                        }
                        Keyword::Print => {
                            consume(token_iter, TokenKind::LeftParenthesis)?;
                            let expr = parse_expr(token_iter, 0)?;
                            consume(token_iter, TokenKind::RightParenthesis)?;
                            consume(token_iter, TokenKind::Semicolon)?;
                            Ok(Stmt::Print(expr))
                        }
                        Keyword::Let => {
                            let (ident, expr_opt) = parse_declaration(token_iter)?;
                            Ok(Stmt::Let(ident, expr_opt))
                        }
                        Keyword::Mut => {
                            let (ident, expr_opt) = parse_declaration(token_iter)?;
                            Ok(Stmt::Mut(ident, expr_opt))
                        }
                        Keyword::For => {
                            let condition_expr = parse_expr(token_iter, 0)?;
                            consume(token_iter, TokenKind::LeftParenthesis)?;
                            let mut stmts = Vec::new();
                            while let Some(t) = token_iter.peek()
                                && !t.kind.eq(&TokenKind::RightParenthesis)
                            {
                                let stmt = parse_stmt(token_iter)?;
                                stmts.push(stmt);
                            }
                            consume(token_iter, TokenKind::RightParenthesis)?;
                            Ok(Stmt::For(condition_expr, stmts))
                        }
                    },
                    TokenKind::Identifier(id)
                        if token_iter
                            .peek()
                            .map(|t| t.kind == TokenKind::Operator(Operator::Equal))
                            .unwrap_or(false) =>
                    {
                        consume(token_iter, TokenKind::Operator(Operator::Equal))?;
                        let expr = parse_expr(token_iter, 0)?;
                        consume(token_iter, TokenKind::Semicolon)?;
                        Ok(Stmt::Assignment(id, expr))
                    }
                    TokenKind::Identifier(_)
                    | TokenKind::Integer(_)
                    | TokenKind::Operator(_)
                    | TokenKind::Colon
                    | TokenKind::Semicolon
                    | TokenKind::LeftParenthesis
                    | TokenKind::RightParenthesis
                    | TokenKind::EOF => {
                        let expr = parse_expr(token_iter, 0)?;
                        consume(token_iter, TokenKind::Semicolon)?;
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

        fn parse_declaration(
            token_iter: &mut TokenIter,
        ) -> Result<(String, Option<Expr>), CompilerError> {
            let TokenKind::Identifier(id) =
                consume(token_iter, TokenKind::Identifier(String::new()))?.kind
            else {
                panic!("The token should be an identifier");
            };
            let mut expr_opt = None;
            if consume(token_iter, TokenKind::Colon).is_ok() {
                consume(token_iter, TokenKind::Operator(Operator::Equal))?;
                expr_opt = Some(parse_expr(token_iter, 0)?);
            };
            consume(token_iter, TokenKind::Semicolon)?;
            Ok((id, expr_opt))
        }

        fn parse_expr(token_iter: &mut TokenIter, bind_pow: u8) -> CompilerResult<Expr> {
            let Some(t) = token_iter.next() else {
                // TODO: improve errors
                return Err(CompilerError::Generic(
                    "calling parse_expr with empty iterator".to_owned(),
                ));
            };

            let mut left = match t.kind {
                TokenKind::Operator(op) if let Some((_, right_bind_pow)) = prefix_bind_pow(&op) => {
                    let expr = parse_expr(token_iter, right_bind_pow)?;
                    Expr::Operation(op, vec![expr])
                }
                TokenKind::LeftParenthesis => {
                    let expr = parse_expr(token_iter, 0)?;
                    consume(token_iter, TokenKind::RightParenthesis)?;
                    Expr::Parenthesis(Box::new(expr))
                }
                TokenKind::Integer(i) => Expr::Atom(Atom::Constant(Constant::Integer(i))),
                TokenKind::Identifier(id) => Expr::Atom(Atom::Identifier(id)),
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

                let op = match &t.kind {
                    TokenKind::Operator(op) => op,
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

        /// TODO(mhs): shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
        /// and adapted to our needs with the following rule in mind
        /// we use and odd number for the bare priority and bump it up by one for associativity
        /// Bind Power      Operator
        ///     15          unary operations
        ///     13          *   /   %   %%   &   &~  <<   >>
        ///     11          +   -   |   ~    in  not_in
        ///      9          ==  !=  <   >    <=  >=
        ///      7          &&
        ///      5          ||
        ///      3          ..=    ..<
        ///      1          or_else  =  ?    if  when

        // TODO: make a single function for the binding powers
        fn postfix_bind_pow(op: &Operator) -> Option<(u8, ())> {
            None
        }

        fn prefix_bind_pow(op: &Operator) -> Option<((), u8)> {
            let res = match op {
                Operator::Plus | Operator::Minus => ((), 15),
                Operator::Equal => return None,
                _ => panic!("bad prefix op: {op:?}"),
            };

            Some(res)
        }

        fn infix_bind_pow(op: &Operator) -> Option<(u8, u8)> {
            let res = match op {
                Operator::Plus | Operator::Minus => (11, 12),
                Operator::Star | Operator::Slash => (13, 14),
                Operator::Greater => (9, 10),
                Operator::Equal => (1, 2), // TODO(mhs): is equal right associative?
                _ => panic!("bad prefix op: {op:?}"),
            };
            Some(res)
        }

        fn consume(token_iter: &mut TokenIter, expected: TokenKind) -> CompilerResult<Token> {
            if let Some(actual) = token_iter.peek()
                && match actual.kind {
                    // we want to know the exact token
                    TokenKind::Operator(_) | TokenKind::Keyword(_) => actual.kind.eq(&expected),
                    // for variables and other symbols, a discriminant comparison is enough
                    TokenKind::Integer(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Colon
                    | TokenKind::Semicolon
                    | TokenKind::LeftParenthesis
                    | TokenKind::RightParenthesis
                    | TokenKind::EOF => discriminant(&actual.kind) == discriminant(&expected),
                }
            {
                Ok(token_iter.next().unwrap())
            } else {
                let Some(token) = token_iter.peek() else {
                    return Err(CompilerError::Generic(format!(
                        "Expected '{expected}' but got 'None' ",
                    )));
                };
                let location = token.location();
                Err(CompilerError::Generic(format!(
                    "Expected '{expected}' but got '{:?}' on {}",
                    token, location,
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
            Print(Expr),
            Let(String, Option<Expr>),
            Mut(String, Option<Expr>),
            Assignment(String, Expr),
            For(Expr, Vec<Stmt>),
        }

        #[derive(Debug, Clone)]
        pub enum Expr {
            Atom(Atom),
            Parenthesis(Box<Expr>),
            Operation(Operator, Vec<Expr>),
        }

        #[derive(Debug, Clone)]
        pub enum Atom {
            Constant(Constant),
            Identifier(String),
        }

        #[derive(Debug, Clone, Copy, PartialEq)]
        pub enum Constant {
            Integer(i64),
        }

        impl Display for Stmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Stmt::Expr(expr) => writeln!(f, "{expr}"),
                    Stmt::Return(expr) => writeln!(f, "(return {expr})"),
                    Stmt::Print(expr) => writeln!(f, "(print {expr})"),
                    Stmt::Let(id, expr_opt) => writeln!(f, "(let {id} {expr_opt:?})"),
                    Stmt::Mut(id, expr_opt) => writeln!(f, "(mut {id} {expr_opt:?})"),
                    Stmt::Assignment(id, expr) => writeln!(f, "(= {id} {expr:})"),
                    Stmt::For(cond, stmts) => {
                        writeln!(f, "(for {cond}")?;
                        for stmt in stmts {
                            writeln!(f, "\t{stmt}")?;
                        }
                        writeln!(f, ")")
                    }
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
        impl Display for Atom {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Atom::Constant(c) => write!(f, "{c}"),
                    Atom::Identifier(id) => write!(f, "{id}"),
                }
            }
        }

        impl Display for Constant {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Constant::Integer(i) => write!(fmt, "{i}"),
                }
            }
        }
    }

    pub mod lexer {
        pub fn tokenize(source: &str) -> CompilerResult<Vec<Token>> {
            let mut iter = source.chars().peekable();

            let mut line = 1;
            let mut col = 0;

            let mut tokens: Vec<Token> = Vec::new();
            loop {
                let Some(c) = iter.next() else {
                    break;
                };
                col += 1;

                let kind = match c {
                    ' ' => continue,
                    '\t' => {
                        // TODO(mhs): a tab counts as 4 columns, for now
                        col += 3;
                        continue;
                    }
                    '\n' => {
                        col = 0;
                        line += 1;
                        continue;
                    }
                    '=' => TokenKind::Operator(Operator::Equal),
                    '-' => TokenKind::Operator(Operator::Minus),
                    '+' => TokenKind::Operator(Operator::Plus),
                    '*' => TokenKind::Operator(Operator::Star),
                    '>' => TokenKind::Operator(Operator::Greater),
                    '/' => {
                        if iter.peek().is_some_and(|nt| '/'.eq(nt)) {
                            while let Some(nt) = iter.next()
                                && !'\n'.eq(&nt)
                            {}
                            col = 0;
                            line += 1;
                            continue;
                        } else {
                            TokenKind::Operator(Operator::Slash)
                        }
                    }
                    '(' => TokenKind::LeftParenthesis,
                    ')' => TokenKind::RightParenthesis,
                    ':' => TokenKind::Colon,
                    ';' => TokenKind::Semicolon,
                    d if d.is_ascii_digit() => {
                        let mut number = c.to_string();
                        while let Some(c) = iter.peek()
                            && c.is_ascii_digit()
                        {
                            number.push(*c);
                            iter.next();
                            col += 1;
                        }
                        let value = number.parse::<i64>().unwrap();
                        TokenKind::Integer(value)
                    }
                    c if c.is_alphabetic() => {
                        let mut id = c.to_string();
                        while let Some(c) = iter.peek()
                            && (c.is_ascii_alphanumeric() || c.eq(&'_'))
                        {
                            id.push(*c);
                            iter.next();
                            col += 1;
                        }

                        if let Some(keyword) = get_keyword(&id) {
                            keyword
                        } else {
                            TokenKind::Identifier(id)
                        }
                    }
                    _ => panic!("Unexpected character {c:?}"),
                };

                let token = Token { kind, line, col };
                tokens.push(token);
            }

            let token = Token {
                kind: TokenKind::EOF,
                line,
                col,
            };
            tokens.push(token);

            Ok(tokens)
        }

        fn get_keyword(id: &str) -> Option<TokenKind> {
            assert_eq!(std::mem::variant_count::<Keyword>(), 5);
            match id {
                "return" => Some(TokenKind::Keyword(Keyword::Return)),
                "print" => Some(TokenKind::Keyword(Keyword::Print)),
                "let" => Some(TokenKind::Keyword(Keyword::Let)),
                "mut" => Some(TokenKind::Keyword(Keyword::Mut)),
                "for" => Some(TokenKind::Keyword(Keyword::For)),
                _ => None,
            }
        }

        use crate::{
            shared::{Keyword, Operator, Token, TokenKind},
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
        Equal,
        Greater,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Keyword {
        Return,
        Print,
        Let,
        Mut,
        For,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum TokenKind {
        // Operators
        Operator(Operator),
        // Literals
        Integer(i64),
        // Keywords
        Keyword(Keyword),
        // Others
        Identifier(String),
        Colon,
        Semicolon,
        LeftParenthesis,
        RightParenthesis,
        // EOF
        EOF,
    }

    impl Display for TokenKind {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                // Token::Identifier(id) => write!(f, "{id}"),
                TokenKind::Integer(n) => write!(fmt, "{n}"),
                TokenKind::Colon => write!(fmt, ":"),
                TokenKind::Semicolon => write!(fmt, ";"),
                TokenKind::LeftParenthesis => write!(fmt, "("),
                TokenKind::RightParenthesis => write!(fmt, ")"),
                TokenKind::EOF => write!(fmt, "EOF"),
                TokenKind::Operator(o) => o.fmt(fmt),
                TokenKind::Keyword(k) => k.fmt(fmt),
                TokenKind::Identifier(id) => write!(fmt, "'{id}'"),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Token {
        pub kind: TokenKind,
        pub line: usize,
        pub col: usize,
    }

    impl Token {
        pub fn location(&self) -> String {
            format!("[ line:{} ; col:{} ]", self.line, self.col)
        }
    }

    impl Display for Token {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.kind.fmt(fmt)
        }
    }

    impl Display for Keyword {
        fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Keyword::Return => write!(fmt, "return"),
                Keyword::Print => write!(fmt, "print"),
                Keyword::Let => write!(fmt, "let"),
                Keyword::Mut => write!(fmt, "mut"),
                Keyword::For => write!(fmt, "for"),
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
                Operator::Equal => write!(f, "="),
                Operator::Greater => write!(f, ">"),
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
        process::{Command, ExitStatus},
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

                let output_file = test_path.clone().with_extension("output");
                let Ok(expected_output_content) = std::fs::read_to_string(output_file) else {
                    println!(
                        "ERROR --> Skipping {:?} : No '.output' file found for it",
                        filename
                    );
                    return Err(std::io::Error::last_os_error());
                };

                println!("[[ Compiling '{:}' ]]", filename);
                let res = compile_file(&test_path);
                assert_eq!(Ok(()), res);

                println!("--> [ Executing '{:}' ]", filename);
                let actual_output = Command::new(format!("./{}", filename))
                    // .arg(format!(" > {}.test_output", filename))
                    .current_dir(tests_dir.clone())
                    .output()?;
                // .spawn()?
                // .wait()?;
                assert_eq!(
                    actual_output.status,
                    <ExitStatus as std::default::Default>::default()
                );

                println!(
                    "--> [ Comparing execution of '{:}' with '.output' file ]",
                    filename
                );
                let actual_output_content =
                    std::str::from_utf8(&actual_output.stdout).expect("Output should be UTF-8");
                assert_eq!(actual_output_content, expected_output_content);

                println!("--> [ Removing files '{:}' ]", filename);
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
