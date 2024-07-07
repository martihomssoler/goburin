pub use self::parser::AST;
use crate::CompilerResult;

pub fn generate_ast(source: &str) -> CompilerResult<AST> {
    let tokens = lexer::tokenize(source)?;

    let ast = parser::parse(tokens)?;

    semantic::analyze(ast)
}

pub mod semantic {
    use std::collections::HashMap;

    use self::{lamba_calc::*, type_inference::*};

    use super::parser::AST;
    use crate::CompilerResult;

    /// empty for now
    pub fn analyze(ast: AST) -> CompilerResult<AST> {
        Ok(ast)
    }

    pub mod lamba_calc {
        use std::{collections::HashMap, fmt::Display};

        use crate::{
            parser::{Expr, Stmt},
            AST,
        };

        pub struct TypedExpr {
            expr: Expr,
            // typ: Type,
        }

        type Name = String;

        // Expressions
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub struct Variable(pub Name);

        pub enum Expression {
            Variable(Variable),
            Abstraction {
                var: Variable,
                abs: Box<Expression>,
            },
            Application {
                func: Box<Expression>,
                arg: Box<Expression>,
            },
            Let {
                var: Variable,
                value: Box<Expression>,
                body: Box<Expression>,
            },
        }

        // Types
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
        pub struct VariableType(pub Name);

        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum FunctionType {
            Arrow,
            Bool,
            Int,
            List,
        }

        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum MonoType {
            VariableType(VariableType),
            ApplicationType {
                func: FunctionType,
                args: Vec<MonoType>,
            },
        }

        impl Display for MonoType {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    MonoType::VariableType(name) => write!(fmt, "{}", name.0),
                    MonoType::ApplicationType { func, args } => match func {
                        FunctionType::Arrow => {
                            assert!(args.len() >= 2);
                            write!(fmt, "{} ->", args[0])?;

                            for a in args.iter().skip(1) {
                                write!(fmt, " {}", a)?;
                            }

                            Ok(())
                        }
                        FunctionType::Bool => write!(fmt, "Bool"),
                        FunctionType::Int => write!(fmt, "Int"),
                        FunctionType::List => write!(fmt, "List"),
                    },
                }
            }
        }

        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum PolyType {
            MonoType(MonoType),
            QuantifierType { name: Name, sigma: Box<PolyType> },
        }

        impl Display for PolyType {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    PolyType::MonoType(m) => write!(fmt, "{m}"),
                    PolyType::QuantifierType { name, sigma } => write!(fmt, "V{name}. {sigma}"),
                }
            }
        }

        // Context
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub struct Context {
            pub map: HashMap<Variable, PolyType>,
        }

        impl Context {
            pub fn new(mappings: &[(Variable, PolyType)]) -> Context {
                let mut map = HashMap::new();

                for (v, t) in mappings {
                    map.insert(v.clone(), t.clone());
                }

                Context { map }
            }

            pub fn mappings(&self) -> Vec<(Variable, PolyType)> {
                self.map
                    .iter()
                    .map(|(a, b)| (a.clone(), b.clone()))
                    .collect()
            }
        }
    }

    // TODO(mhs): decide which algorithm for type inference works best for Goburin, W or M.
    pub mod type_inference {
        use std::{collections::HashMap, fmt::Display, ops::Deref, process::exit};

        use crate::{CompilerError, CompilerResult};

        use super::{
            lamba_calc::{Context, MonoType, PolyType, Variable},
            Expression, VariableType,
        };

        // top-down approach
        pub fn m(
            ctx: Context,
            expr: &Expression,
            typ: MonoType,
            mut count: usize,
        ) -> CompilerResult<Substitution> {
            match expr {
                Expression::Variable(var) => {
                    let Some(value) = ctx.map.get(var) else {
                        return Err(CompilerError::SemanticError(format!(
                            "Undefined variable '{}'",
                            var.0
                        )));
                    };

                    Ok(unify(typ, instantiate(value, &mut count, [].into())))
                }
                Expression::Abstraction { var, abs } => {
                    let beta1 = MonoType::VariableType(VariableType::new(&mut count));
                    let beta2 = MonoType::VariableType(VariableType::new(&mut count));

                    let s1 = unify(
                        typ,
                        MonoType::ApplicationType {
                            func: super::FunctionType::Arrow,
                            args: [beta1.clone(), beta2.clone()].to_vec(),
                        },
                    );

                    let new_ctx = Context::new(
                        &[
                            s1.apply_ctx(ctx).mappings(),
                            [(var.clone(), PolyType::MonoType(s1.apply_mono(&beta1)))].into(),
                        ]
                        .concat(),
                    );
                    let s2 = m(new_ctx, abs.deref(), s1.apply_mono(&beta2), count)?;

                    Ok(s2.combine(&s1))
                }
                Expression::Application { func, arg } => {
                    let beta = MonoType::VariableType(VariableType::new(&mut count));
                    let s1 = m(
                        ctx.clone(),
                        func.deref(),
                        MonoType::ApplicationType {
                            func: super::FunctionType::Arrow,
                            args: [beta.clone(), typ].to_vec(),
                        },
                        count,
                    )?;

                    let s2 = m(s1.apply_ctx(ctx), arg.deref(), s1.apply_mono(&beta), count)?;

                    Ok(s2.combine(&s1))
                }
                Expression::Let { var, value, body } => {
                    let beta = MonoType::VariableType(VariableType::new(&mut count));
                    let s1 = m(ctx.clone(), value.deref(), beta.clone(), count)?;

                    let new_ctx = Context::new(
                        &[
                            s1.apply_ctx(ctx.clone()).mappings(),
                            [(
                                var.clone(),
                                generalise(&s1.apply_ctx(ctx), s1.apply_mono(&beta)),
                            )]
                            .into(),
                        ]
                        .concat(),
                    );
                    let s2 = m(new_ctx, body.deref(), s1.apply_mono(&typ), count)?;

                    Ok(s2.combine(&s1))
                }
            }
        }

        // bottom-up approach
        pub fn w(
            ctx: Context,
            expr: &Expression,
            mut count: usize,
        ) -> CompilerResult<(Substitution, MonoType)> {
            match expr {
                Expression::Variable(var) => {
                    let Some(value) = ctx.map.get(var) else {
                        return Err(CompilerError::SemanticError(format!(
                            "Undefined variable '{}'",
                            var.0
                        )));
                    };

                    Ok((
                        Substitution::new(&[]),
                        instantiate(value, &mut count, [].into()),
                    ))
                }
                Expression::Abstraction { var, abs } => {
                    let beta = MonoType::VariableType(VariableType::new(&mut count));
                    let new_ctx = Context::new(
                        &[
                            ctx.mappings(),
                            [(var.clone(), PolyType::MonoType(beta.clone()))].into(),
                        ]
                        .concat(),
                    );
                    let (s1, t1) = w(new_ctx, abs.deref(), count)?;

                    Ok((
                        s1.clone(),
                        s1.apply_mono(&MonoType::ApplicationType {
                            func: super::FunctionType::Arrow,
                            args: [beta, t1].to_vec(),
                        }),
                    ))
                }
                Expression::Application { func, arg } => {
                    let (s1, t1) = w(ctx.clone(), func.deref(), count)?;
                    // println!("s: {s1}, t: {t1}");
                    let (s2, t2) = w(s1.apply_ctx(ctx), arg.deref(), count)?;
                    // println!("s: {s2}, t: {t2}");
                    let beta = MonoType::VariableType(VariableType::new(&mut count));

                    let s3 = unify(
                        s2.apply_mono(&t1),
                        MonoType::ApplicationType {
                            func: super::FunctionType::Arrow,
                            args: [t2, beta.clone()].to_vec(),
                        },
                    );

                    Ok((s3.combine(&s2.combine(&s1)), s3.apply_mono(&beta)))
                }
                Expression::Let { var, value, body } => {
                    let (s1, t1) = w(ctx.clone(), value.deref(), count)?;
                    let applied_ctx = s1.apply_ctx(ctx);
                    let new_ctx = Context::new(
                        &[
                            applied_ctx.mappings(),
                            [(var.clone(), generalise(&applied_ctx, t1.clone()))].into(),
                        ]
                        .concat(),
                    );
                    let (s2, t2) = w(new_ctx, body.deref(), count)?;

                    Ok((s2.combine(&s1), t2))
                }
            }
        }

        // substitution -> apply + combine
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub struct Substitution {
            pub map: HashMap<VariableType, MonoType>,
        }

        impl Substitution {
            pub fn new(mappings: &[(VariableType, MonoType)]) -> Substitution {
                let mut map = HashMap::new();

                for (v, t) in mappings {
                    map.insert(v.clone(), t.clone());
                }

                Substitution { map }
            }

            pub fn apply_ctx(&self, value: Context) -> Context {
                Context::new(
                    &value
                        .map
                        .iter()
                        .map(|(k, v)| (k.clone(), self.apply_poly(v)))
                        .collect::<Vec<(Variable, PolyType)>>(),
                )
            }

            pub fn apply_poly(&self, value: &PolyType) -> PolyType {
                match &value {
                    PolyType::MonoType(mono) => PolyType::MonoType(self.apply_mono(mono)),
                    PolyType::QuantifierType { name, sigma } => PolyType::QuantifierType {
                        name: name.to_owned(),
                        sigma: Box::new(self.apply_poly(sigma.as_ref())),
                    },
                }
            }

            pub fn apply_mono(&self, value: &MonoType) -> MonoType {
                match &value {
                    MonoType::VariableType(var) => {
                        // if the substitution contains a mapping for the value, we map it
                        if let Some(value) = self.map.get(var) {
                            value.clone()
                        }
                        // otherwise we return the value as-is
                        else {
                            value.clone()
                        }
                    }
                    MonoType::ApplicationType { func, args } => {
                        let subs_arg = args.iter().map(|arg| self.apply_mono(arg)).collect();

                        MonoType::ApplicationType {
                            func: func.clone(),
                            args: subs_arg,
                        }
                    }
                }
            }

            pub fn combine(&self, value: &Substitution) -> Substitution {
                Substitution::new(
                    &[
                        self.map
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect::<Vec<(VariableType, MonoType)>>(),
                        value
                            .map
                            .iter()
                            .map(|(k, v)| (k.clone(), self.apply_mono(v)))
                            .collect::<Vec<(VariableType, MonoType)>>(),
                    ]
                    .into_iter()
                    .flatten()
                    .collect::<Vec<(VariableType, MonoType)>>(),
                )
            }
        }

        impl Display for Substitution {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                for (var, typ) in &self.map {
                    writeln!(fmt, "{:?} : {:?}", var, typ)?
                }
                Ok(())
            }
        }

        // new type variable
        impl VariableType {
            pub fn new(count: &mut usize) -> VariableType {
                let var = VariableType(format!("__t{count}"));
                *count += 1;
                var
            }
        }

        // instatiate
        pub fn instantiate(
            poly: &PolyType,
            count: &mut usize,
            mut mappings: HashMap<String, VariableType>,
        ) -> MonoType {
            match poly {
                PolyType::MonoType(m) => match m {
                    MonoType::VariableType(var) => {
                        if let Some(typ) = mappings.get(&var.0) {
                            MonoType::VariableType(typ.clone())
                        } else {
                            m.clone()
                        }
                    }
                    MonoType::ApplicationType { func, args } => {
                        let subs_arg = args
                            .iter()
                            .map(|arg| {
                                instantiate(
                                    &PolyType::MonoType(arg.clone()),
                                    count,
                                    mappings.clone(),
                                )
                            })
                            .collect();

                        MonoType::ApplicationType {
                            func: func.clone(),
                            args: subs_arg,
                        }
                    }
                },
                PolyType::QuantifierType { name, sigma } => {
                    mappings.insert(name.to_owned(), VariableType::new(count));
                    instantiate(sigma.deref(), count, mappings)
                }
            }
        }

        // generalise
        pub fn generalise(ctx: &Context, typ: MonoType) -> PolyType {
            let quantifiers = difference(
                &free_vars_poly(PolyType::MonoType(typ.clone())),
                &free_vars_ctx(ctx),
            );

            let mut t = PolyType::MonoType(typ);
            for q in quantifiers {
                t = PolyType::QuantifierType {
                    name: q,
                    sigma: Box::new(t),
                }
            }

            t
        }

        fn free_vars_ctx(ctx: &Context) -> Vec<String> {
            ctx.map
                .iter()
                .flat_map(|(_, v)| free_vars_poly(v.clone()))
                .collect()
        }

        fn free_vars_poly(typ: PolyType) -> Vec<String> {
            match typ {
                PolyType::MonoType(m) => match m {
                    MonoType::VariableType(v) => [v.0].into(),
                    MonoType::ApplicationType { args, .. } => args
                        .iter()
                        .flat_map(|arg| free_vars_poly(PolyType::MonoType(arg.clone())))
                        .collect(),
                },
                PolyType::QuantifierType { name, sigma } => free_vars_poly(sigma.deref().clone())
                    .iter()
                    .filter(|item| !name.eq(*item))
                    .cloned()
                    .collect(),
            }
        }

        fn difference(a: &[String], b: &[String]) -> Vec<String> {
            a.iter().filter(|item| !b.contains(item)).cloned().collect()
        }

        // unify
        // TODO(mhs): as far as I understood, this `MonoType` limitation is what makes this implementation
        // not polymorphic, aka terms do not depend on types
        pub fn unify(typ1: MonoType, typ2: MonoType) -> Substitution {
            match (&typ1, &typ2) {
                (MonoType::VariableType(v1), MonoType::VariableType(v2)) => {
                    // they are the same, so no substitution is needed
                    if v1.0.eq(&v2.0) {
                        Substitution::new(&[])
                    } else {
                        Substitution::new(&[(v1.clone(), typ2)])
                    }
                }
                (MonoType::VariableType(v1), MonoType::ApplicationType { args, .. }) => {
                    // FIXME(mhs): what to do if we have an infinite type definition / recursive type like a binary tree?
                    if contains(args, v1) {
                        eprintln!("ERROR -> Infinite type detected!");
                        exit(1);
                    }

                    Substitution::new(&[(v1.clone(), typ2)])
                }
                (_, MonoType::VariableType(_)) => unify(typ2, typ1),
                (
                    MonoType::ApplicationType {
                        func: func1,
                        args: args1,
                    },
                    MonoType::ApplicationType {
                        func: func2,
                        args: args2,
                    },
                ) => {
                    // FIXME(mhs): should find a better way to handle this cases and exit gracefully
                    if !func1.eq(func2) {
                        eprintln!(
                            "ERROR -> Types cannot be unified! Expected a {func1:?} but got a {func2:?}"
                        );
                        exit(1);
                    } else if args1.len() != args2.len() {
                        eprintln!(
                            "ERROR -> Types do not have the same number of arguments! Expected a {func1:?} with {} args but got {}", args1.len(), args2.len()
                        );
                        exit(1);
                    }
                    let mut s = Substitution::new(&[]);
                    // now we now that both applications have the same type and number of arguments
                    // so we unify each pair of args with one another
                    (0..args1.len()).for_each(|i| {
                        let t1 = s.apply_mono(&args1[i]);
                        let t2 = s.apply_mono(&args2[i]);
                        s = s.combine(&unify(t1, t2));
                    });

                    s
                }
            }
        }

        fn contains(args: &[MonoType], v1: &VariableType) -> bool {
            for arg in args {
                if match arg {
                    MonoType::VariableType(v2) => v1.eq(v2),
                    MonoType::ApplicationType { args, .. } => contains(args, v1),
                } {
                    return true;
                }
            }

            false
        }

        // unification
        // fn unify(a: MonoType, b: MonoType) -> Substitution {
        //     match a {
        //         Variable => match b {
        //             Variable => { // is the same type variable
        //                 return {} // no substitution needed
        //             },

        //         },
        //     }
        // }
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
                    Keyword::Const => {
                        let (ident, expr_opt) = parse_declaration(token_iter)?;
                        Ok(Stmt::Let(ident, expr_opt))
                    }
                    Keyword::Mut => {
                        let (ident, expr_opt) = parse_declaration(token_iter)?;
                        Ok(Stmt::Mut(ident, expr_opt))
                    }
                    Keyword::Let => {
                        let (ident, expr_opt) = parse_declaration(token_iter)?;
                        Ok(Stmt::Let(ident, expr_opt))
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
                // id = ?;
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
                // id[?]
                TokenKind::Identifier(id)
                    if token_iter
                        .peek()
                        .map(|t| t.kind == TokenKind::LeftBracket)
                        .unwrap_or(false) =>
                {
                    consume(token_iter, TokenKind::LeftBracket)?;
                    let expr = parse_expr(token_iter, 0)?;
                    consume(token_iter, TokenKind::RightBracket)?;
                    Ok(Stmt::ArrayIndexing(id, expr))
                }
                TokenKind::Identifier(_)
                | TokenKind::Integer(_)
                | TokenKind::String(_)
                | TokenKind::Operator(_)
                | TokenKind::Colon
                | TokenKind::Semicolon
                | TokenKind::LeftParenthesis
                | TokenKind::RightParenthesis
                | TokenKind::LeftBracket
                | TokenKind::RightBracket
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
            TokenKind::String(s) => Expr::Atom(Atom::String(s)),
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
            Operator::Greater | Operator::Lower => (9, 10),
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
                | TokenKind::String(_)
                | TokenKind::Identifier(_)
                | TokenKind::Colon
                | TokenKind::Semicolon
                | TokenKind::LeftParenthesis
                | TokenKind::RightParenthesis
                | TokenKind::LeftBracket
                | TokenKind::RightBracket
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

    #[derive(Debug, Clone)]
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
        ArrayIndexing(String, Expr),
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
        String(String),
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
                Stmt::ArrayIndexing(_, _) => todo!(),
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
                Atom::String(s) => write!(f, "{s}"),
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
    type PeekableCharIter<'c> = std::iter::Peekable<std::str::Chars<'c>>;

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
                '<' => TokenKind::Operator(Operator::Lower),
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
                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,
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
                '"' => parse_string(c, &mut iter, &mut col),
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

    fn parse_string(c: char, iter: &mut PeekableCharIter, col: &mut usize) -> TokenKind {
        let mut can_escape = false;
        let mut id = String::new();
        while let Some(c) = iter.peek() {
            let ch = if can_escape {
                match c {
                    // replace("\\\\", "\\")
                    '\\' => {
                        id.pop();
                        '\\'
                    }
                    // replace("\\n", "\n")
                    'n' => {
                        id.pop();
                        '\n'
                    }
                    // replace("\\\"", "\"")
                    '\"' => {
                        id.pop();
                        '\"'
                    }
                    _ => *c,
                }
            } else {
                *c
            };

            let is_str_ending = !can_escape && c.eq(&'"');
            can_escape = !can_escape && c.eq(&'\\');
            iter.next();
            *col += 1;

            if is_str_ending {
                return TokenKind::String(id);
            }

            id.push(ch);
        }
        TokenKind::EOF
    }

    fn get_keyword(id: &str) -> Option<TokenKind> {
        assert_eq!(std::mem::variant_count::<Keyword>(), 6);

        match id {
            _ if format!("{}", Keyword::Return).eq(id) => Some(TokenKind::Keyword(Keyword::Return)),
            _ if format!("{}", Keyword::Print).eq(id) => Some(TokenKind::Keyword(Keyword::Print)),
            _ if format!("{}", Keyword::Const).eq(id) => Some(TokenKind::Keyword(Keyword::Const)),
            _ if format!("{}", Keyword::Mut).eq(id) => Some(TokenKind::Keyword(Keyword::Mut)),
            _ if format!("{}", Keyword::Let).eq(id) => Some(TokenKind::Keyword(Keyword::Let)),
            _ if format!("{}", Keyword::For).eq(id) => Some(TokenKind::Keyword(Keyword::For)),
            _ => None,
        }
    }

    use crate::{
        shared::{Keyword, Operator, Token, TokenKind},
        CompilerResult,
    };
    use std::fmt::Display;
}
