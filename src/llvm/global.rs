use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    pub fn compile_global(&mut self, global: &mir::Global) {
        match global {
            mir::Global::Function { name, params, body, is_main: _ } => {
                let fn_type = self.ptr_type().fn_type(&[self.ptr_type().into()].repeat(params.len()), false);
                let fn_value = self.module.add_function(name.as_str(), fn_type, None);

                self.builder.position_at_end(self.context.append_basic_block(fn_value, "entry"));
                self.builder.build_return(Some(&self.compile_expr(body))).unwrap();
            }

            mir::Global::Variable { name, body } => {
                let global_value = self.module.add_global(self.ptr_type(), None, name.as_str());
                let init_value = self.compile_expr(body);

                self.add_global_init(global_value, init_value);
            }
        }
    }
}
