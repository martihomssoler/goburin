//! VM Tests
#![cfg(test)]

mod macros;

use super::{Instruction::*, Machine, Register::*, SImm4bit, Signal, UImm4bit};
use crate::{case, nok, run_cases};

#[test]
fn t_arithmetics_integer() {
    let cases = [
        // --- Add ---
        case!([], [(Add { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Add { r: x0, s: x1 }, nok!())], [(xA, 255)]),
        case!([(x1, u16::MAX)], [(Add { r: x1, s: x1 }, nok!())], [(xA, u16::MAX - 1)]),
        // --- Sub ---
        case!([], [(Sub { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Sub { r: x0, s: x1 }, nok!())], [(xA, (-255_i16 as u16))]),
        case!([(x1, u16::MAX)], [(Sub { r: x1, s: x1 }, nok!())], [(xA, 0)]),
        // --- Mul ---
        case!([], [(Mul { r: x1, s: x2 }, nok!())], [(xA, 0)]),
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Mul { r: x1, s: x2 }, nok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Mul { r: x1, s: x2 }, nok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Mul { r: x1, s: x2 }, nok!())], [(xA, (-400_i32 as u16))]),
        case!([(x1, 255), (x2, 2)], [(Mul { r: x1, s: x2 }, nok!())], [(xA, 510)]),
        case!([(x1, (i16::MAX as u16)), (x2, (-2_i16 as u16))], [(Mul { r: x1, s: x2 }, nok!())], [(xA, (-65534_i32 as u16))]),
        case!([(x1, u16::MAX), (x2, 2)], [(Mul { r: x1, s: x2 }, nok!())], [(xA, u16::MAX - 1)]),
        // --- Divs ---
        // case!([], [(Divs { r: x1, s: x2 }, ok!())], [(xA, 0)]), TODO(mhs): handle division by zero
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Divs { r: x1, s: x2 }, nok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Divs { r: x1, s: x2 }, nok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Divs { r: x1, s: x2 }, nok!())], [(xA, (-100_i32 as u16))]),
        case!([(x1, 255), (x2, 2)], [(Divs { r: x1, s: x2 }, nok!())], [(xA, 127)]),
        case!([(x1, (i16::MAX as u16)), (x2, (-2_i16 as u16))], [(Divs { r: x1, s: x2 }, nok!())], [(xA, (-16383_i32 as u16))]),
        case!([(x1, u16::MAX), (x2, 2)], [(Divs { r: x1, s: x2 }, nok!())], [(xA, ((-1_i16) / 2) as u16)]),
        // --- Divu ---
        // case!([], [(Divu { r: x1, s: x2 }, ok!())], [(xA, 0)]), TODO(mhs): handle division by zero
        case!([(x1, (-1_i16 as u16)), (x2, (-1_i16 as u16))], [(Divu { r: x1, s: x2 }, nok!())], [(xA, 1)]),
        case!([(x1, (-1_i16 as u16)), (x2, 1)], [(Divu { r: x1, s: x2 }, nok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, 200), (x2, (-2_i16 as u16))], [(Divu { r: x1, s: x2 }, nok!())], [(xA, 0)]),
        case!([(x1, 255), (x2, 2)], [(Divu { r: x1, s: x2 }, nok!())], [(xA, 127)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Divu { r: x1, s: x2 }, nok!())], [(xA, 32767)]),
        // --- Addi ---
        case!([], [(Addi { r: x1, val: SImm4bit(0) }, nok!())], [(xA, 0)]),
        case!([], [(Addi { r: x1, val: SImm4bit(1) }, nok!())], [(xA, 1)]),
        case!([(x1, 255)], [(Addi { r: x1, val: SImm4bit(1) }, nok!())], [(xA, 256)]),
        case!([(x1, u16::MAX)], [(Addi { r: x1, val: SImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([], [(Addi { r: x1, val: SImm4bit(-1) }, nok!())], [(xA, u16::MAX)]),
        case!([(x1, 255)], [(Addi { r: x1, val: SImm4bit(-1) }, nok!())], [(xA, 254)]),
        case!([(x1, u16::MAX)], [(Addi { r: x1, val: SImm4bit(-1) }, nok!())], [(xA, u16::MAX - 1)]),
    ];
    run_cases!(cases);
}

#[test]
fn t_bit_logic() {
    let cases = [
        // --- Shl ---
        case!([], [(Shl { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shl { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 1), (x2, 2)], [(Shl { r: x1, s: x2 }, nok!())], [(xA, 1 << 2)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Shl { r: x1, s: x2 }, nok!())], [(xA, u16::MAX << 2)]),
        // --- Shr ---
        case!([], [(Shr { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shr { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 8), (x2, 2)], [(Shr { r: x1, s: x2 }, nok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16)), (x2, 2)], [(Shr { r: x1, s: x2 }, nok!())], [(xA, (-1_i16 as u16) >> 2)]),
        case!([(x1, u16::MAX), (x2, 2)], [(Shr { r: x1, s: x2 }, nok!())], [(xA, u16::MAX >> 2)]),
        // --- Sha ---
        case!([], [(Sha { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Sha { r: x0, s: x1 }, nok!())], [(xA, 0)]),
        case!([(x1, 8), (x2, 2)], [(Sha { r: x1, s: x2 }, nok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16)), (x2, 2)], [(Sha { r: x1, s: x2 }, nok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, (i16::MAX as u16)), (x2, 2)], [(Sha { r: x1, s: x2 }, nok!())], [(xA, (i16::MAX as u16) >> 2)]),
        // --- Shli ---
        case!([], [(Shli { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shli { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 1)], [(Shli { r: x1, val: UImm4bit(2) }, nok!())], [(xA, 1 << 2)]),
        case!([(x1, u16::MAX)], [(Shli { r: x1, val: UImm4bit(2) }, nok!())], [(xA, u16::MAX << 2)]),
        // --- Shri ---
        case!([], [(Shri { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shri { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 8)], [(Shri { r: x1, val: UImm4bit(2) }, nok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16))], [(Shri { r: x1, val: UImm4bit(2) }, nok!())], [(xA, (-1_i16 as u16) >> 2)]),
        case!([(x1, u16::MAX)], [(Shri { r: x1, val: UImm4bit(2) }, nok!())], [(xA, u16::MAX >> 2)]),
        // --- Shai ---
        case!([], [(Shai { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 255)], [(Shai { r: x0, val: UImm4bit(1) }, nok!())], [(xA, 0)]),
        case!([(x1, 8)], [(Shai { r: x1, val: UImm4bit(2) }, nok!())], [(xA, 8 >> 2)]),
        case!([(x1, (-1_i16 as u16))], [(Shai { r: x1, val: UImm4bit(2) }, nok!())], [(xA, (-1_i16 as u16))]),
        case!([(x1, (i16::MAX as u16))], [(Shai { r: x1, val: UImm4bit(2) }, nok!())], [(xA, (i16::MAX as u16) >> 2)]),
        // --- Neg ---
        case!([], [(Neg { r: x1, s: x2 }, nok!())], [(x1, 0)]),
        case!([(x2, 255)], [(Neg { r: x1, s: x2 }, nok!())], [(x1, (-255_i16 as u16))]),
        case!([(x2, 0)], [(Neg { r: x1, s: x2 }, nok!())], [(x1, 0)]),
        case!([(x2, 1)], [(Neg { r: x1, s: x2 }, nok!())], [(x1, (-1_i16 as u16))]),
        case!([(x2, -1_i16 as u16)], [(Neg { r: x1, s: x2 }, nok!())], [(x1, 1)]),
        case!([(x2, u16::MAX)], [(Neg { r: x1, s: x2 }, nok!())], [(x1, 1)]),
        // --- And ---
        case!([], [(And { r: x1, s: x2 }, nok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(And { r: x1, s: x2 }, nok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(And { r: x1, s: x2 }, nok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(And { r: x1, s: x2 }, nok!())], [(xA, 0xFF00)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(And { r: x1, s: x2 }, nok!())], [(xA, 0xABCD)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(And { r: x1, s: x2 }, nok!())], [(xA, 0x0000)]),
        // --- Or ---
        case!([], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0xFFFF)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0xABCD)]),
        case!([(x1, 0xABCD), (x2, 0x0000)], [(Or { r: x1, s: x2 }, nok!())], [(xA, 0xABCD)]),
        // --- Xor ---
        case!([], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0)]),
        case!([(x1, 0x00FF), (x2, 0x00FF)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0x0000)]),
        case!([(x1, 0xFFFF), (x2, 0x00FF)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0xFF00)]),
        case!([(x1, 0xFFFF), (x2, 0xFF00)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0x00FF)]),
        case!([(x1, 0xFFFF), (x2, 0xABCD)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0x5432)]),
        case!([(x1, 0x0000), (x2, 0xABCD)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0xABCD)]),
        case!([(x1, 0xABCD), (x2, 0x0000)], [(Xor { r: x1, s: x2 }, nok!())], [(xA, 0xABCD)]),
    ];
    run_cases!(cases);
}
