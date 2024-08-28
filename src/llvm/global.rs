use crate::mir;

use super::LLVMCodegen;

impl LLVMCodegen<'_, '_> {
    pub fn compile_global(&mut self, global: &mir::Global) {
        match global {
            mir::Global::Function { name, params, body } => todo!(),
            mir::Global::Variable { name, body } => todo!(),
        }
    }
}
