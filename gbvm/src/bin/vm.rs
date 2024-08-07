use gbvm::Register::*;
use gbvm::*;

fn main() -> VMResult<()> {
    let program = [
        // fibonacci 10
        // initialization
        Instruction::Ld8a { value: 0 },
        Instruction::Cpy { r: x1, s: xA },
        Instruction::Ld8a { value: 1 },
        Instruction::Cpy { r: x2, s: xA },
        Instruction::Ld8a { value: 10 },
        Instruction::Addi { r: xA, val: SImm4bit(-1) },
        Instruction::Cpy { r: x3, s: xA },
        // condition
        Instruction::Cle { r: x3, s: x0 },
        Instruction::Jnzr { offset: Imm7bit(13) },
        // body
        Instruction::Add { r: x1, s: x2 },
        Instruction::Signal1,
        Instruction::Cpy { r: x1, s: x2 },
        Instruction::Cpy { r: x2, s: xA },
        Instruction::Addi { r: x3, val: SImm4bit(-1) },
        Instruction::Cpy { r: x3, s: xA },
        Instruction::Jmpr { offset: Imm7bit(-17) },
        // over
        Instruction::Cpy { r: xA, s: x2 },
        Instruction::Signal1,
        Instruction::Signal0,
    ];
    let mut vm = Machine::new(&program)?;
    loop {
        println!("IP: 0x{:04}", vm.get_ip());
        if let Some(signal) = vm.step()? {
            match signal {
                Signal::Halt => break,
                Signal::Print => println!("{}", vm.get_register_value(Register::xA)),
                Signal::None => todo!(),
            }
        }
    }

    println!("Success!");
    Ok(())
}
