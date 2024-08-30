use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx, 'm> LLVMCodegen<'a, 'ctx, 'm> {
    pub fn compile_global(&mut self, global: &'m mir::Global) {
        match global {
            mir::Global::Function { name, param, env, body, is_main } => {
                let fn_type = self.ptr_type().fn_type(&[self.ptr_type().into()].repeat(2), false);
                let fn_value = self.module.add_function(name.as_str(), fn_type, None);

                self.functions.insert(name.clone());

                // Set and register parameter names
                let env_ptr = fn_value.get_param_iter().nth(0).unwrap();
                env_ptr.set_name("_env");

                let param_ptr = fn_value.get_param_iter().nth(1).unwrap();
                param_ptr.set_name(param.as_str());

                self.add_variable(param.clone(), param_ptr.into_pointer_value());

                self.enter_fn_block(fn_value);

                // Load environment
                let env_struct = self.ptr_struct(env.len());
                for (i, var_name) in env.iter().enumerate() {
                    // let var_ptr = self.builder.build_alloca(self.ptr_type(), var_name.as_str()).unwrap();

                    let env_field_ptr = self.builder.build_struct_gep(
                        env_struct, 
                        env_ptr.into_pointer_value(), 
                        i as u32,
                        "_env_field",
                    ).unwrap();

                    let var_ptr = self.builder.build_load(self.ptr_type(), env_field_ptr, var_name.as_str()).unwrap();

                    // self.builder.build_store(var_ptr, env_field_ptr.as_basic_value_enum()).unwrap();
                    self.add_variable(var_name.clone(), var_ptr.into_pointer_value());
                }

                self.builder.build_return(Some(&self.compile_expr(body))).unwrap();

                // If entry point, set self.entry_point
                if *is_main {
                    self.entry_point = Some(fn_value);
                }
            }

            mir::Global::Variable { name, body } => {
                let global_value = self.module.add_global(self.ptr_type(), None, name.as_str());
                self.add_global_init(global_value, body);
            }
        }
    }
}
