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
