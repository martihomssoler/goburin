mod instruction;

use instruction::*;

const REGISTER_COUNT: usize = 16;
const MEMORY_SIZE: usize = 1024;
const BASE_IP: u32 = 0x00000000;

pub type VMResult<T> = Result<T, String>;

pub struct Machine {
    registers: [u16; REGISTER_COUNT],
    memory: Box<dyn Memory>,
}

impl Machine {
    pub fn new() -> Self {
        Self { registers: [0; REGISTER_COUNT], memory: Box::new(LinearMemory::new(MEMORY_SIZE)) }
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

    fn execute_instr(&mut self, instr: Instruction) -> VMResult<()> {
        match instr {
            Instruction::Jmpr { offset } => {
                // TODO(mhs): handle over/under-flowing case
                let (new_ip, _overflowed) = self.offset_ip(offset.0);
                self.registers[Register::IPlo] = new_ip as u16;
                self.registers[Register::IPhi] = (new_ip >> 16) as u16;
            }
            Instruction::Jnzr { offset } => {
                if self.registers[Register::xC] != 0 {
                    // TODO(mhs): handle over/under-flowing case
                    let (new_ip, _overflowed) = self.offset_ip(offset.0);
                    self.registers[Register::IPlo] = new_ip as u16;
                    self.registers[Register::IPhi] = (new_ip >> 16) as u16;
                }
            }
            // TODO(mhs): handle over/under-flowing case
            Instruction::Jzr { offset } => {
                if self.registers[Register::xC] == 0 {
                    // TODO(mhs): handle over/under-flowing case
                    let (new_ip, _overflowed) = self.offset_ip(offset.0);
                    self.registers[Register::IPlo] = new_ip as u16;
                    self.registers[Register::IPhi] = (new_ip >> 16) as u16;
                }
            }
            Instruction::Jmpa { r, s } => self.set_ip(self.registers[r], self.registers[s]),
            Instruction::Jnza { r, s } => {
                if self.registers[Register::xC] != 0 {
                    self.set_ip(self.registers[r], self.registers[s])
                }
            }
            Instruction::Jza { r, s } => {
                if self.registers[Register::xC] == 0 {
                    self.set_ip(self.registers[r], self.registers[s])
                }
            }
            Instruction::Cpy { r, s } => self.registers[r] = self.registers[s],
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
            Instruction::Muls { r, s } => todo!(),
            Instruction::Mulu { r, s } => todo!(),
            Instruction::Mulf { r, s } => todo!(),
            Instruction::Divs { r, s } => todo!(),
            Instruction::Divu { r, s } => todo!(),
            Instruction::Divf { r, s } => todo!(),
            Instruction::Addi { r, val } => todo!(),
            Instruction::Subi { r, val } => todo!(),
            Instruction::Shl { r, s } => todo!(),
            Instruction::Shr { r, s } => todo!(),
            Instruction::Sha { r, s } => todo!(),
            Instruction::Shli { r, val } => todo!(),
            Instruction::Shri { r, val } => todo!(),
            Instruction::Shai { r, val } => todo!(),
            Instruction::Neg { r, s } => todo!(),
            Instruction::And { r, s } => todo!(),
            Instruction::Or { r, s } => todo!(),
            Instruction::Xor { r, s } => todo!(),
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
        Ok(())
    }

    fn get_ip(&self) -> u32 {
        (self.registers[Register::IPhi] as u32) << 16 | (self.registers[Register::IPlo] as u32)
    }

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
    pub fn new(size: usize) -> Self {
        Self { bytes: vec![0; size] }
    }
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

    fn index(&self, index: Register) -> &Self::Output {
        &self[index as usize]
    }
}

impl std::ops::IndexMut<Register> for [u16] {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self[index as usize]
    }
}
