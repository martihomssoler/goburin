// use std::fmt::Display;

// pub type CompilerResult<T> = Result<T, CompilerError>;

// pub fn compile(source: &str) -> CompilerResult<String> {
//     let ast = crate::parser::Parser::parse(source);
//     Ok(ast.to_string())
// }

// #[derive(thiserror::Error, Debug)]
// pub enum CompilerError {
//     Generic(String),
// }

// impl Display for CompilerError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             CompilerError::Generic(s) => write!(f, "{}", s),
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::{compile, CompilerResult};

// #[test]
// fn l_var_compiler() -> CompilerResult<()> {
//     let c = compile("x = 12 + 20\nprint(10 + x)")?;
//     assert_eq!(c, "false");

//     Ok(())
// }
// }
