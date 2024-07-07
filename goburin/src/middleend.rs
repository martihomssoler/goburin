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
        strs: Vec::new(),
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
                let mut val = get_expr_variable(expr, ir)?;

                if let Variable::String(s) = &val {
                    let str_id = format!("str_{}", ir.strs.len());
                    let var_id = id.clone();
                    ir.strs.push((var_id, (str_id.clone(), s.clone())));
                    val = Variable::String(str_id);
                };

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
            ir.ops.push(Op::JumpIfNotZero {
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
        Stmt::ArrayIndexing(_, _) => todo!(),
    }

    Ok(())
}

fn get_expr_variable(expr: Expr, ir: &mut IR) -> CompilerResult<Variable> {
    let res = match expr {
        Expr::Atom(a) => match a {
            Atom::Constant(c) => Variable::Value(c),
            Atom::Identifier(id) => Variable::Named(id),
            Atom::String(s) => Variable::String(s),
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
            Atom::String(_) => todo!(),
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
        Operator::Lower => Op::Lower {
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
    /// Poor mans map of [ var_name => (str_id, str_data) ]
    pub strs: Vec<(String, (String, String))>, // TODO(mhs): improve this, making it dumb so it is easy to self-host later
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

    pub fn get_string_var(&self, var: &Variable) -> Option<String> {
        let Variable::Named(id) = var else {
            return None;
        };

        let ret = None;

        for (var_id, (str_id, _)) in self.strs.iter() {
            if var_id.eq(id) {
                return Some(str_id.clone());
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
    Lower {
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
    JumpIfNotZero {
        var: Variable,
        true_label: String,
        false_label: String,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Temporary(u8),
    Named(String),
    String(String),
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
            Op::JumpIfNotZero {
                var,
                true_label,
                false_label,
            } => todo!(),
            Op::Greater { res, left, right } => todo!(),
            Op::Lower { res, left, right } => todo!(),
            Op::Jump { dest } => todo!(),
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Temporary(t) => write!(f, "%_t{t}"),
            Variable::Value(v) => write!(f, "{v}"),
            Variable::Named(id) => write!(f, "%var_{id}"),
            Variable::String(s) => write!(f, "${s}"),
        }
    }
}
