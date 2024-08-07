// --- MACRO HELPERS ---
#[macro_export]
macro_rules! run_cases {
    ($cases:expr) => {
        for (init_state, instructions, results) in $cases {
            // Instantiate new Machine
            let mut vm = Machine::default();

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
                    $crate::red!("[ERROR]:"),
                    $crate::pink!(format!("{:?}", instructions.iter().map(|(i, _)| $crate::machine::print_instr(&vm, *i)).collect::<Vec<_>>())),
                    $crate::blue!(format!("Register::{reg:?}")),
                    $crate::green!(format!("{expected_val}")),
                    $crate::yellow!(format!("{}", vm.registers[reg],))
                );
            }
        }
    };
}

#[macro_export]
macro_rules! case {
    ($init:expr, $instrs:expr, $res:expr) => {
        ($init.to_vec(), $instrs.to_vec(), $res.to_vec())
    };
}

#[macro_export]
macro_rules! nok {
    () => {
        Ok(None)
    };
}

#[macro_export]
macro_rules! red {
    ($input:expr) => {
        format!("\x1b[91m{}\x1b[0m", $input)
    };
}

#[macro_export]
macro_rules! green {
    ($input:expr) => {
        format!("\x1b[92m{}\x1b[0m", $input)
    };
}

#[macro_export]
macro_rules! yellow {
    ($input:expr) => {
        format!("\x1b[93m{}\x1b[0m", $input)
    };
}

#[macro_export]
macro_rules! blue {
    ($input:expr) => {
        format!("\x1b[94m{}\x1b[0m", $input)
    };
}

#[macro_export]
macro_rules! pink {
    ($input:expr) => {
        format!("\x1b[95m{}\x1b[0m", $input)
    };
}
// --- ---
