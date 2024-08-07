use super::*;

impl Ast {
    pub fn p2_0_ir(self) -> Result<Ir, String> {
        let tac = ast_to_ir(&self)?;
        println!("TAC: {tac:?}");

        let ir = Ir {
            program: self.program,
            state: IrState {
                symbol_table: self.symbol_table,
                vars: Vec::new(),
                strs: Vec::new(),
            },
        };

        Ok(ir)
    }
}

fn ast_to_ir(ast: &Ast) -> Result<Tac, String> {
    for def in &ast.program {
        if def.id.0.eq("main") {
            let mut instrs = Vec::new();
            let DefinitionValue::FunctionBody(stmts) = &def.val;
            for stmt in stmts {
                match stmt {
                    Statement::Return(expr) => match expr.kind {
                        ExpressionKind::LiteralInteger(_) => {
                            let ExpressionKind::LiteralInteger(i) = expr.kind else {
                                todo!()
                            };
                            instrs.push(Instruction::Return { val: Val::Constant(i) });
                        }
                        ExpressionKind::UnaryOpNeg => todo!(),
                        ExpressionKind::UnaryOpNot => todo!(),
                        ExpressionKind::Identifier(_)
                        | ExpressionKind::LiteralBoolean(_)
                        | ExpressionKind::LiteralCharacter(_)
                        | ExpressionKind::LiteralString(_)
                        | ExpressionKind::BinaryOpAssignment
                        | ExpressionKind::BinaryOpAdd
                        | ExpressionKind::BinaryOpSub
                        | ExpressionKind::BinaryOpMul
                        | ExpressionKind::BinaryOpDiv
                        | ExpressionKind::BinaryOpLower
                        | ExpressionKind::BinaryOpArrayAccess
                        | ExpressionKind::FuncCall
                        | ExpressionKind::FuncArg => todo!(),
                    },
                    Statement::Expression(_)
                    | Statement::Block(_)
                    | Statement::Conditional(_)
                    | Statement::Declaration(_)
                    | Statement::Assignment(_)
                    | Statement::Loop(_)
                    | Statement::Print(_) => todo!(),
                }
            }
            return Ok(Tac {
                program: Program {
                    main: Function {
                        id: "main".to_string(),
                        body: instrs,
                    },
                },
            });
        }
    }
    Err("No function main found!".to_string())
}

#[derive(Debug)]
struct Tac {
    program: Program,
}

#[derive(Debug)]
struct Program {
    main: Function,
}

#[derive(Debug)]
struct Function {
    id: String,
    body: Vec<Instruction>,
}

#[derive(Debug)]
enum Instruction {
    Return { val: Val },
    Unary { op: UnaryOp, src: Val, dst: Var },
}

#[derive(Debug)]
enum Val {
    Constant(i64),
    Var(Var),
}

#[derive(Debug)]
struct Var(String);

#[derive(Debug)]
enum UnaryOp {
    Complement,
    Negate,
}
