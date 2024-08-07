mod instruction;
mod machine_tests;

use std::ops::{Add, Mul, Shl, Shr};

use instruction::*;

const REGISTER_COUNT: usize = 16;
const MEMORY_SIZE: usize = 1024;
const STACK_SIZE: usize = 1024;
const BASE_IP: u32 = 0x00000000;

pub type VMResult<T> = Result<T, String>;

pub struct Machine {
    registers: [u16; REGISTER_COUNT],
    memory: Box<dyn Memory>,
    stack: Box<dyn Memory>,
}

impl Machine {
    pub fn new() -> Self {
        Self { registers: [0; REGISTER_COUNT], memory: Box::new(LinearMemory::new(MEMORY_SIZE)), stack: Box::new(LinearMemory::new(STACK_SIZE)) }
    }

    pub fn step(&mut self) -> VMResult<()> {
        let addr = self.get_ip();
        let (instr, bytes_read) = self.fetch_and_decode(addr)?;
        self.offset_ip(bytes_read as i16); // harmless cast since the nof bytes read will be lower than 128
        self.execute_instr(instr)?;
        Ok(())
    }

    fn fetch_and_decode(&mut self, addr: u32) -> VMResult<(Instruction, u16)> {
        let mut bytes_read = 0;
        let header = self.memory.read(addr, 1)?;
        bytes_read += 1;
        // FIXME(mhs): return the correct address
        Ok((Instruction::Pop8, bytes_read))
    }

    pub(crate) fn execute_instr(&mut self, instr: Instruction) -> VMResult<()> {
        match instr {
            // Inconditional jump, modifying the Instruction Pointer (IP) by the offset amount.
            Instruction::Jmpr { offset } => {
                // TODO(mhs): handle over/under-flowing case
                let (new_ip, _overflowed) = self.offset_ip(offset.0);
                self.registers[Register::IPlo] = new_ip as u16;
                self.registers[Register::IPhi] = (new_ip >> 16) as u16;
            }
            // If xC is not equal to 0, modify the Instruction Pointer (IP) by the offset amount.
            Instruction::Jnzr { offset } => {
                if self.registers[Register::xC] != 0 {
                    // TODO(mhs): handle over/under-flowing case
                    let (new_ip, _overflowed) = self.offset_ip(offset.0);
                    self.registers[Register::IPlo] = new_ip as u16;
                    self.registers[Register::IPhi] = (new_ip >> 16) as u16;
                }
            }
            // If xC is equal to 0, modify the Instruction Pointer (IP) by the offset amount.
            Instruction::Jzr { offset } => {
                if self.registers[Register::xC] == 0 {
                    // TODO(mhs): handle over/under-flowing case
                    let (new_ip, _overflowed) = self.offset_ip(offset.0);
                    self.registers[Register::IPlo] = new_ip as u16;
                    self.registers[Register::IPhi] = (new_ip >> 16) as u16;
                }
            }
            // Inconditional jump to an absolute address computed as R << 16 | S
            Instruction::Jmpa { r, s } => self.set_ip(self.registers[r], self.registers[s]),
            // Jump if xC is not zero to an absolute address computed as R << 16 | S
            Instruction::Jnza { r, s } => {
                if self.registers[Register::xC] != 0 {
                    self.set_ip(self.registers[r], self.registers[s]);
                }
            }
            // Jump if xC is zero to an absolute address computed as R << 16 | S
            Instruction::Jza { r, s } => {
                if self.registers[Register::xC] == 0 {
                    self.set_ip(self.registers[r], self.registers[s]);
                }
            }
            // Copy the content of register S to register R
            Instruction::Cpy { r, s } => self.registers[r] = self.registers[s],
            // store offset 16bits; [xB << 16 | xC + 7bit] = xA
            Instruction::Sto16 { offset } => {
                let base_addr = (self.registers[Register::xB] as u32) << 16 | (self.registers[Register::xC] as u32);
                // TODO(mhs): handle over/under-flowing case
                let (new_addr, _overflowed) = base_addr.overflowing_add_signed(offset.0 as i32);
                self.memory.write(new_addr, &self.registers[Register::xA].to_le_bytes())?;
            }
            // store offset lower 8bits; [xB << 16 | xC + 7bit] = xA
            Instruction::Sto8 { offset } => {
                let base_addr = (self.registers[Register::xB] as u32) << 16 | (self.registers[Register::xC] as u32);
                // TODO(mhs): handle over/under-flowing case
                let (new_addr, _overflowed) = base_addr.overflowing_add_signed(offset.0 as i32);
                self.memory.write(new_addr, &self.registers[Register::xA].to_le_bytes()[..0])?;
            }
            Instruction::St16 { r, s } => {
                let new_addr = (self.registers[r] as u32) << 16 | (s as u32);
                self.memory.write(new_addr, &self.registers[Register::xA].to_le_bytes())?;
            }
            Instruction::St8 { r, s } => {
                let new_addr = (self.registers[r] as u32) << 16 | (s as u32);
                self.memory.write(new_addr, &self.registers[Register::xA].to_le_bytes()[..0])?;
            }
            Instruction::Ld16a { value } => self.registers[Register::xA] = value,
            Instruction::Ld16b { value } => self.registers[Register::xB] = value,
            Instruction::Ld16c { value } => self.registers[Register::xC] = value,
            // the value is a sign-extended 8bit to 16bt
            Instruction::Ld8a { value } => self.registers[Register::xA] = value,
            Instruction::Ld8b { value } => self.registers[Register::xB] = value,
            Instruction::Ld8c { value } => self.registers[Register::xC] = value,
            Instruction::Ldo16 { offset } => {
                let base_addr = (self.registers[Register::xB] as u32) << 16 | (self.registers[Register::xC] as u32);
                // TODO(mhs): handle over/under-flowing case
                let (new_addr, _overflowed) = base_addr.overflowing_add_signed(offset.0 as i32);
                let bytes = self.memory.read(new_addr, 2)?;
                let new_value = (bytes[1] as u16) << 8 | bytes[0] as u16;
                self.registers[Register::xA] = new_value;
            }
            Instruction::Ldo8 { offset } => {
                let base_addr = (self.registers[Register::xB] as u32) << 16 | (self.registers[Register::xC] as u32);
                // TODO(mhs): handle over/under-flowing case
                let (new_addr, _overflowed) = base_addr.overflowing_add_signed(offset.0 as i32);
                let bytes = self.memory.read(new_addr, 1)?;
                let new_value = bytes[0] as u16;
                self.registers[Register::xA] = new_value;
            }
            Instruction::Ld16 { r, s } => {
                let new_addr = (self.registers[r] as u32) << 16 | (self.registers[s] as u32);
                let bytes = self.memory.read(new_addr, 2)?;
                let new_value = (bytes[1] as u16) << 8 | bytes[0] as u16;
                self.registers[Register::xA] = new_value;
            }
            Instruction::Ld8 { r, s } => {
                let new_addr = (self.registers[r] as u32) << 16 | (self.registers[s] as u32);
                let bytes = self.memory.read(new_addr, 1)?;
                let new_value = bytes[0] as u16;
                self.registers[Register::xA] = new_value;
            }
            Instruction::Add { r, s } => self.registers[Register::xA] = self.registers[r].wrapping_add(self.registers[s]),
            Instruction::Addf { r, s } => self.registers[Register::xA] = (self.registers[r] as f16).add(self.registers[s] as f16) as u16,
            Instruction::Sub { r, s } => self.registers[Register::xA] = self.registers[r].wrapping_sub(self.registers[s]),
            Instruction::Subf { r, s } => self.registers[Register::xA] = (self.registers[r] as f16 - self.registers[s] as f16) as u16,
            Instruction::Mul { r, s } => self.registers[Register::xA] = self.registers[r].wrapping_mul(self.registers[s]),
            Instruction::Mulf { r, s } => self.registers[Register::xA] = (self.registers[r] as f16).mul(self.registers[s] as f16) as u16,
            Instruction::Divs { r, s } => self.registers[Register::xA] = (self.registers[r] as i16 / self.registers[s] as i16) as u16,
            Instruction::Divu { r, s } => self.registers[Register::xA] = self.registers[r].div_floor(self.registers[s]),
            Instruction::Divf { r, s } => self.registers[Register::xA] = (self.registers[r] as f16 / self.registers[s] as f16) as u16,
            Instruction::Addi { r, val } => self.registers[Register::xA] = self.registers[r].wrapping_add(val.0 as u16),
            Instruction::Shl { r, s } => self.registers[Register::xA] = self.registers[r].wrapping_shl(self.registers[s] as u32),
            Instruction::Shr { r, s } => self.registers[Register::xA] = self.registers[r].wrapping_shr(self.registers[s] as u32),
            Instruction::Sha { r, s } => self.registers[Register::xA] = (self.registers[r] as i16).wrapping_shr(self.registers[s] as u32) as u16,
            Instruction::Shli { r, val } => self.registers[Register::xA] = self.registers[r].wrapping_shl(val.0 as u32),
            Instruction::Shri { r, val } => self.registers[Register::xA] = self.registers[r].wrapping_shr(val.0 as u32),
            Instruction::Shai { r, val } => self.registers[Register::xA] = (self.registers[r] as i16).shr(val.0) as u16,
            Instruction::Neg { r, s } => self.registers[r] = (self.registers[s] ^ 0xFFFF).wrapping_add(1),
            Instruction::And { r, s } => self.registers[Register::xA] = self.registers[r] & self.registers[s],
            Instruction::Or { r, s } => self.registers[Register::xA] = self.registers[r] | self.registers[s],
            Instruction::Xor { r, s } => self.registers[Register::xA] = self.registers[r] ^ self.registers[s],
            Instruction::Ps16 { value } => {
                let sp = self.registers[Register::SP];
                let stack_addr = sp as u32;
                self.stack.write(stack_addr, &value.to_le_bytes())?;
                self.registers[Register::SP] = sp + 2;
            }
            Instruction::Ps8 { value } => {
                let sp = self.registers[Register::SP];
                let stack_addr = sp as u32;
                self.stack.write(stack_addr, &value.to_le_bytes()[..0])?;
                self.registers[Register::SP] = sp + 1;
            }
            Instruction::Pop16 => {
                let sp = self.registers[Register::SP];
                let stack_addr = sp as u32 - 2;
                let bytes = self.stack.read(stack_addr, 2)?;
                self.registers[Register::SP] = sp - 2;
                self.registers[Register::xA] = (bytes[1] as u16) << 8 | bytes[0] as u16;
            }
            Instruction::Pop8 => {
                let sp = self.registers[Register::SP];
                let stack_addr = sp as u32 - 1;
                let bytes = self.stack.read(stack_addr, 1)?;
                self.registers[Register::SP] = sp - 1;
                self.registers[Register::xA] = bytes[0] as u16;
            }
            Instruction::Ceq { r, s } => self.registers[Register::xC] = (self.registers[r] == self.registers[s]) as u16,
            Instruction::Clt { r, s } => self.registers[Register::xC] = (self.registers[r] < self.registers[s]) as u16,
            Instruction::Cle { r, s } => self.registers[Register::xC] = (self.registers[r] <= self.registers[s]) as u16,
            // HALT signal
            Instruction::Signal0 => (),
            Instruction::Signal1 => todo!(),
            Instruction::Signal2 => todo!(),
            Instruction::Signal3 => todo!(),
            Instruction::Signal4 => todo!(),
            Instruction::Signal5 => todo!(),
            Instruction::Signal6 => todo!(),
            Instruction::Signal7 => todo!(),
            Instruction::Signal8 => todo!(),
            Instruction::Signal9 => todo!(),
            Instruction::SignalA => todo!(),
            Instruction::SignalB => todo!(),
            Instruction::SignalC => todo!(),
            Instruction::SignalD => todo!(),
            Instruction::SignalE => todo!(),
            Instruction::SignalF => todo!(),
        }
        Ok(())
    }

    fn get_ip(&self) -> u32 { (self.registers[Register::IPhi] as u32) << 16 | (self.registers[Register::IPlo] as u32) }

    fn set_ip(&mut self, high: u16, low: u16) {
        self.registers[Register::IPhi] = high;
        self.registers[Register::IPlo] = low;
    }

    fn offset_ip(&mut self, offset: i16) -> (u32, bool) {
        let ip = self.get_ip();
        ip.overflowing_add_signed(offset as i32)
    }
}

pub trait Memory {
    fn read(&self, addr: u32, bytes: usize) -> VMResult<Vec<u8>>;
    fn write(&mut self, addr: u32, bytes: &[u8]) -> VMResult<()>;
}

struct LinearMemory {
    bytes: Vec<u8>,
}

impl LinearMemory {
    pub fn new(size: usize) -> Self { Self { bytes: vec![0; size] } }
}

impl Memory for LinearMemory {
    fn read(&self, addr: u32, bytes: usize) -> VMResult<Vec<u8>> {
        let mut res = Vec::with_capacity(bytes);
        for i in 0..bytes {
            let index = addr as usize + i;
            let Some(byte) = self.bytes.get(index) else {
                return Err(format!("Invalid memory address @{}", index));
            };
            res[i] = *byte;
        }

        Ok(res)
    }

    fn write(&mut self, addr: u32, bytes: &[u8]) -> VMResult<()> {
        for i in 0..bytes.len() {
            let index = addr as usize + i;
            let Some(byte) = self.bytes.get_mut(index) else {
                return Err(format!("Invalid memory address @{}", index));
            };
            *byte = bytes[i];
        }

        Ok(())
    }
}

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    // register that ALWAYS equals 0x00. Any writes to it are discarded, basically a NO-OP.
    x0,
    /// general register
    x1,
    /// general register
    x2,
    /// general register
    x3,
    /// general register
    x4,
    /// general register
    x5,
    /// general register
    x6,
    /// general register
    x7,
    /// general register
    x8,
    /// semi-general "accumulator" register, frequently used by arithmetic and logic instructions.
    xA,
    /// semi-general "branch" register, frequently used by jump instructions.
    xB,
    /// semi-general "comparator" register, frequently used by comparison and conditional instructions.
    xC,
    /// base pointer
    BP,
    /// stack pointer
    SP,
    /// instruction pointer (lower 16bits)
    IPlo,
    /// instruction pointer (higher 16bits)
    IPhi,
}

impl std::ops::Index<Register> for [u16] {
    type Output = u16;

    fn index(&self, index: Register) -> &Self::Output { &self[index as usize] }
}

impl std::ops::IndexMut<Register> for [u16] {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output { &mut self[index as usize] }
}

fn print_instr(vm: &Machine, instr: Instruction) -> String {
    match instr {
        Instruction::Jmpr { offset } => todo!(),
        Instruction::Jnzr { offset } => todo!(),
        Instruction::Jzr { offset } => todo!(),
        Instruction::Jmpa { r, s } => todo!(),
        Instruction::Jnza { r, s } => todo!(),
        Instruction::Jza { r, s } => todo!(),
        Instruction::Cpy { r, s } => todo!(),
        Instruction::Sto16 { offset } => todo!(),
        Instruction::Sto8 { offset } => todo!(),
        Instruction::St16 { r, s } => todo!(),
        Instruction::St8 { r, s } => todo!(),
        Instruction::Ld16a { value } => todo!(),
        Instruction::Ld16b { value } => todo!(),
        Instruction::Ld16c { value } => todo!(),
        Instruction::Ld8a { value } => todo!(),
        Instruction::Ld8b { value } => todo!(),
        Instruction::Ld8c { value } => todo!(),
        Instruction::Ldo16 { offset } => todo!(),
        Instruction::Ldo8 { offset } => todo!(),
        Instruction::Ld16 { r, s } => todo!(),
        Instruction::Ld8 { r, s } => todo!(),
        Instruction::Add { r, s } => todo!(),
        Instruction::Addf { r, s } => todo!(),
        Instruction::Sub { r, s } => todo!(),
        Instruction::Subf { r, s } => todo!(),
        Instruction::Mul { r, s } => todo!(),
        Instruction::Mulf { r, s } => todo!(),
        Instruction::Divs { r, s } => format!("Divs: {} / {}", vm.registers[r], vm.registers[s]),
        Instruction::Divu { r, s } => format!("Divu: {} / {}", vm.registers[r], vm.registers[s]),
        Instruction::Divf { r, s } => todo!(),
        Instruction::Addi { r, val } => format!("Addi: {} + {}", vm.registers[r], val.0),
        Instruction::Shl { r, s } => format!("Shl : {} << {}", vm.registers[r], vm.registers[s]),
        Instruction::Shr { r, s } => format!("Shr : {} >> {}", vm.registers[r], vm.registers[s]),
        Instruction::Sha { r, s } => format!("Sha : {} >> {}", vm.registers[r], vm.registers[s]),
        Instruction::Shli { r, val } => format!("Shli : {} << {}", vm.registers[r], val.0),
        Instruction::Shri { r, val } => format!("Shri : {} >> {}", vm.registers[r], val.0),
        Instruction::Shai { r, val } => format!("Shai : {} >> {}", vm.registers[r], val.0),
        Instruction::Neg { r: _, s } => format!("Neg : {}", vm.registers[s]),
        Instruction::And { r, s } => format!("And : {:X} & {:X}", vm.registers[r], vm.registers[s]),
        Instruction::Or { r, s } => format!("Or : {:X} | {:X}", vm.registers[r], vm.registers[s]),
        Instruction::Xor { r, s } => format!("Xor : {} ^ {}", vm.registers[r], vm.registers[s]),
        Instruction::Ps16 { value } => todo!(),
        Instruction::Ps8 { value } => todo!(),
        Instruction::Pop16 => todo!(),
        Instruction::Pop8 => todo!(),
        Instruction::Ceq { r, s } => todo!(),
        Instruction::Clt { r, s } => todo!(),
        Instruction::Cle { r, s } => todo!(),
        Instruction::Signal0 => todo!(),
        Instruction::Signal1 => todo!(),
        Instruction::Signal2 => todo!(),
        Instruction::Signal3 => todo!(),
        Instruction::Signal4 => todo!(),
        Instruction::Signal5 => todo!(),
        Instruction::Signal6 => todo!(),
        Instruction::Signal7 => todo!(),
        Instruction::Signal8 => todo!(),
        Instruction::Signal9 => todo!(),
        Instruction::SignalA => todo!(),
        Instruction::SignalB => todo!(),
        Instruction::SignalC => todo!(),
        Instruction::SignalD => todo!(),
        Instruction::SignalE => todo!(),
        Instruction::SignalF => todo!(),
    }
}
