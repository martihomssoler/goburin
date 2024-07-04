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

    for op in &ir.ops {
        match op {
            Op::Assignment { res, val } => {
                let res = res.to_string();
                let val = val.to_string();
                writeln!(qbe, "    {res} =w copy {val}");
            }
            Op::Minus { res, left, right } => {
                let res = res.to_string();
                let left = left.to_string();
                let right = right.to_string();

                writeln!(qbe, "    # -- {left} - {right}");
                writeln!(qbe, "    {res} =w sub {left}, {right}");
            }
            Op::Plus { res, left, right } => {
                let res = res.to_string();
                let left = left.to_string();
                let right = right.to_string();

                writeln!(qbe, "    # -- {left} + {right}");
                writeln!(qbe, "    {res} =w add {left}, {right}");
            }
            Op::Star { res, left, right } => {
                let res = res.to_string();
                let left = left.to_string();
                let right = right.to_string();

                writeln!(qbe, "    # -- {left} * {right}");
                writeln!(qbe, "    {res} =w mul {left}, {right}");
            }
            Op::Slash { res, left, right } => {
                let res = res.to_string();
                let left = left.to_string();
                let right = right.to_string();

                writeln!(qbe, "    # -- {left} / {right}");
                writeln!(qbe, "    {res} =w div {left}, {right}");
            }
            Op::Return { var } => {
                let var = var.to_string();

                writeln!(qbe, "    # -- return {var}");
                writeln!(qbe, "    ret {var}");
            }
            Op::Print { var } => {
                writeln!(qbe, "    # -- print {var}");
                if let Some(var) = ir.get_string_var(var) {
                    writeln!(qbe, "    call $printf(l $fmt_str, ..., w ${var})");
                } else {
                    let var = var.to_string();
                    writeln!(qbe, "    call $printf(l $fmt_int, ..., w {var})");
                }
            }
            Op::Label { name } => {
                writeln!(qbe, "@{name}");
            }
            Op::CheckGreater {
                var,
                true_label,
                false_label,
            } => {
                let var = var.to_string();

                writeln!(
                    qbe,
                    "    # -- jump if greater {true_label} else {false_label}"
                );
                writeln!(qbe, "    jnz {var}, @{true_label}, @{false_label}");
            }
            Op::Greater { res, left, right } => {
                let res = res.to_string();
                let left = left.to_string();
                let right = right.to_string();

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
    writeln!(qbe, r#"data $fmt_str = {{ b "%s\n", b 0 }}"#);

    write_all_strs(&mut qbe, &ir);

    qbe
}

fn write_all_strs(qbe: &mut String, ir: &IR) {
    writeln!(qbe);
    for (_, (str_id, str_data)) in &ir.strs {
        writeln!(qbe, r#"data ${str_id}.len = {{ w {} }}"#, str_data.len());
        writeln!(qbe, r#"data ${str_id} = {{ b "{str_data}" }}"#);
    }
}
