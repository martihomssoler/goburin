use super::*;

impl Ast {
    pub fn p2_0_ir(self) -> Result<Ir, String> {
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
