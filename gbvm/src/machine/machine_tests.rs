//! VM Tests
#![cfg(test)]

use super::{Instruction::*, Machine, Register::*, SImm4bit, UImm4bit};

// --- MACRO HELPERS ---
macro_rules! run_cases {
    ($cases:expr) => {
        for (init_state, instructions, results) in $cases {
            // Instantiate new Machine
            let mut vm = Machine::new();

            // modify the register content before executing the instruction
            for (reg, init_val) in init_state {
                vm.registers[reg] = init_val;
            }

            // execute the instruction and assert that the result is Ok
            for (instr, expected_res) in instructions.clone() {
                let res = vm.execute_instr(instr);
                assert_eq!(res, expected_res);
            }

            // validate that the desired registers contain the expected intermediate values
            for (reg, expected_val) in results {
                // pretty-printing errors
                assert_eq!(
                    vm.registers[reg],
                    expected_val,
                    "\n{} While executing {}.\nWrong result for {}. Expected {}, got {}.\n",
                    red!("[ERROR]:"),
                    pink!(format!("{:?}", instructions.iter().map(|(i, _)| crate::machine::print_instr(&vm, *i)).collect::<Vec<_>>())),
                    blue!(format!("Register::{reg:?}")),
                    green!(format!("{expected_val}")),
                    yellow!(format!("{}", vm.registers[reg],))
                );
            }
        }
    };
}

macro_rules! case {
    ($init:expr, $instrs:expr, $res:expr) => {
        ($init.to_vec(), $instrs.to_vec(), $res.to_vec())
    };
}

macro_rules! ok {
    () => {
        Ok(())
    };
}

macro_rules! red {
    ($input:expr) => {
        format!("\x1b[91m{}\x1b[0m", $input)
    };
}

macro_rules! green {
    ($input:expr) => {
        format!("\x1b[92m{}\x1b[0m", $input)
    };
}

macro_rules! yellow {
    ($input:expr) => {
        format!("\x1b[93m{}\x1b[0m", $input)
    };
}

macro_rules! blue {
    ($input:expr) => {
        format!("\x1b[94m{}\x1b[0m", $input)
    };
}

macro_rules! pink {
    ($input:expr) => {
        format!("\x1b[95m{}\x1b[0m", $input)
    };
}
// --- ---

#[test]
fn t_arithmetics_integer() {
    let cases = [
        // --- Add ---
        case!([], [(Add { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Add { r: x0, s: x1 }, ok!())], [(xA, 255)]),
        case!([(x1, u16::MAX)], [(Add { r: x1, s: x1 }, ok!())], [(xA, u16::MAX - 1)]),
        // --- Sub ---
        case!([], [(Sub { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Sub { r: x0, s: x1 }, ok!())], [(xA, (-255_i16 as u16))]),
        case!([(x1, u16::MAX)], [(Sub { r: x1, s: x1 }, ok!())], [(xA, 0)]),
        // --- Mul ---
        case!([], [(Mul { r: x1, s: x2 }, ok!())], [(xA, 0)]),
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Mul { r: x1, s: x2 }, ok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Mul { r: x1, s: x2 }, ok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Mul { r: x1, s: x2 }, ok!())], [(xA, (-400_i32 as u16))]),
        case!([(x1, 255), (x2, 2)], [(Mul { r: x1, s: x2 }, ok!())], [(xA, 510)]),
        case!([(x1, (i16::MAX as u16)), (x2, (-2_i16 as u16))], [(Mul { r: x1, s: x2 }, ok!())], [(xA, (-65534_i32 as u16))]),
        case!([(x1, u16::MAX), (x2, 2)], [(Mul { r: x1, s: x2 }, ok!())], [(xA, u16::MAX - 1)]),
        // --- Divs ---
        // case!([], [(Divs { r: x1, s: x2 }, ok!())], [(xA, 0)]), TODO(mhs): handle division by zero
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Divs { r: x1, s: x2 }, ok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Divs { r: x1, s: x2 }, ok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Divs { r: x1, s: x2 }, ok!())], [(xA, (-100_i32 as u16))]),
        case!([(x1, 255), (x2, 2)], [(Divs { r: x1, s: x2 }, ok!())], [(xA, 127)]),
        case!([(x1, (i16::MAX as u16)), (x2, (-2_i16 as u16))], [(Divs { r: x1, s: x2 }, ok!())], [(xA, (-16383_i32 as u16))]),
        case!([(x1, u16::MAX), (x2, 2)], [(Divs { r: x1, s: x2 }, ok!())], [(xA, ((-1_i16) / 2) as u16)]),
        // --- Divu ---
        // case!([], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 0)]), TODO(mhs): handle division by zero
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Divu { r: x1, s: x2 }, ok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 0)]),
        case!([(x1, 255), (x2, 2)], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 127)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 32767)]),
        // --- Addi ---
        case!([], [(Addi { r: x1, val: SImm4bit(0) }, ok!())], [(xA, 0)]),
        case!([], [(Addi { r: x1, val: SImm4bit(1) }, ok!())], [(xA, 1)]),
        case!([(x1, 255)], [(Addi { r: x1, val: SImm4bit(1) }, ok!())], [(xA, 256)]),
        case!([(x1, u16::MAX)], [(Addi { r: x1, val: SImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([], [(Addi { r: x1, val: SImm4bit(-1) }, ok!())], [(xA, u16::MAX)]),
        case!([(x1, 255)], [(Addi { r: x1, val: SImm4bit(-1) }, ok!())], [(xA, 254)]),
        case!([(x1, u16::MAX)], [(Addi { r: x1, val: SImm4bit(-1) }, ok!())], [(xA, u16::MAX - 1)]),
    ];
    run_cases!(cases);
}

#[test]
fn t_bit_logic() {
    let cases = [
        // --- Shl ---
        case!([], [(Shl { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shl { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 1), (x2, 2)], [(Shl { r: x1, s: x2 }, ok!())], [(xA, 1 << 2)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Shl { r: x1, s: x2 }, ok!())], [(xA, u16::MAX << 2)]),
        // --- Shr ---
        case!([], [(Shr { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shr { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 8), (x2, 2)], [(Shr { r: x1, s: x2 }, ok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16)), (x2, 2)], [(Shr { r: x1, s: x2 }, ok!())], [(xA, (-1_i16 as u16) >> 2)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Shr { r: x1, s: x2 }, ok!())], [(xA, u16::MAX >> 2)]),
        // --- Sha ---
        case!([], [(Sha { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Sha { r: x0, s: x1 }, ok!())], [(xA, 0)]),
        case!([(x1, 8), (x2, 2)], [(Sha { r: x1, s: x2 }, ok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16)), (x2, 2)], [(Sha { r: x1, s: x2 }, ok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, (i16::MAX as u16)), (x2, 2)], [(Sha { r: x1, s: x2 }, ok!())], [(xA, (i16::MAX as u16) >> 2)]),
        // --- Shli ---
        case!([], [(Shli { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shli { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 1)], [(Shli { r: x1, val: UImm4bit(2) }, ok!())], [(xA, 1 << 2)]),
        case!([(x1, u16::MAX)], [(Shli { r: x1, val: UImm4bit(2) }, ok!())], [(xA, u16::MAX << 2)]),
        // --- Shri ---
        case!([], [(Shri { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shri { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 8)], [(Shri { r: x1, val: UImm4bit(2) }, ok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16))], [(Shri { r: x1, val: UImm4bit(2) }, ok!())], [(xA, (-1_i16 as u16) >> 2)]),
        case!([(x1, u16::MAX)], [(Shri { r: x1, val: UImm4bit(2) }, ok!())], [(xA, u16::MAX >> 2)]),
        // --- Shai ---
        case!([], [(Shai { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shai { r: x0, val: UImm4bit(1) }, ok!())], [(xA, 0)]),
        case!([(x1, 8)], [(Shai { r: x1, val: UImm4bit(2) }, ok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16))], [(Shai { r: x1, val: UImm4bit(2) }, ok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, (i16::MAX as u16))], [(Shai { r: x1, val: UImm4bit(2) }, ok!())], [(xA, (i16::MAX as u16) >> 2)]),
        // --- Neg ---
        case!([], [(Neg { r: x1, s: x2 }, ok!())], [(x1, 0)]),
        case!([(x2, 255)], [(Neg { r: x1, s: x2 }, ok!())], [(x1, (-255_i16 as u16))]),
        case!([(x2, 0)], [(Neg { r: x1, s: x2 }, ok!())], [(x1, 0)]),
        case!([(x2, 1)], [(Neg { r: x1, s: x2 }, ok!())], [(x1, (-1_i16 as u16))]),
        case!([(x2, -1_i16 as u16)], [(Neg { r: x1, s: x2 }, ok!())], [(x1, 1)]),
        case!([(x2, u16::MAX)], [(Neg { r: x1, s: x2 }, ok!())], [(x1, 1)]),
        // --- And ---
        case!([], [(And { r: x1, s: x2 }, ok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(And { r: x1, s: x2 }, ok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(And { r: x1, s: x2 }, ok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(And { r: x1, s: x2 }, ok!())], [(xA, 0xFF00)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(And { r: x1, s: x2 }, ok!())], [(xA, 0xABCD)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(And { r: x1, s: x2 }, ok!())], [(xA, 0x0000)]),
        // --- Or ---
        case!([], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0xABCD)]),
        case!([(x1, 0xABCD), (x2, 0x0000)], [(Or { r: x1, s: x2 }, ok!())], [(xA, 0xABCD)]),
        // --- Xor ---
        case!([], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0x0000)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0xFF00)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0x5432)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0xABCD)]),
        case!([(x1, 0xABCD), (x2, 0x0000)], [(Xor { r: x1, s: x2 }, ok!())], [(xA, 0xABCD)]),
    ];
    run_cases!(cases);
}
