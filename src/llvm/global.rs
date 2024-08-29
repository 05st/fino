use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    pub fn compile_global(&mut self, global: &mir::Global) {
        match global {
            mir::Global::Function { name, params, body, is_main } => {
                let fn_type = self.ptr_type().fn_type(&[self.ptr_type().into()].repeat(params.len()), false);
                let fn_value = self.module.add_function(name.as_str(), fn_type, None);

                self.functions.insert(name.clone());

                // Set and register parameter names
                for (i, arg) in fn_value.get_param_iter().enumerate() {
                    let ptr_value = arg.into_pointer_value();
                    self.add_variable(params[i].clone(), ptr_value);
                    ptr_value.set_name(params[i].as_str());
                }

                self.builder.position_at_end(self.context.append_basic_block(fn_value, "entry"));
                self.builder.build_return(Some(&self.compile_expr(body))).unwrap();

                // If entry point, build call from main function
                if *is_main {
                    self.builder.position_at_end(self.main_fn_entry_block);
                    self.builder.build_call(fn_value, &[], "main_call").unwrap();
                }
            }

            mir::Global::Variable { name, body } => {
                let global_value = self.module.add_global(self.ptr_type(), None, name.as_str());
                let init_value = self.compile_expr(body);

                self.add_global_init(global_value, init_value);
            }
        }
    }
}
