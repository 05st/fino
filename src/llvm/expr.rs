use inkwell::values::{AnyValue, BasicValue, BasicValueEnum};

use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    pub fn compile_expr(&mut self, expr: &mir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            mir::Expr::Lit(literal) => self.compile_literal(literal),

            mir::Expr::Var(var_name) => match self.variables.get(var_name) {
                Some(ptr) => (*ptr).as_basic_value_enum(),
                None => {
                    if self.functions.contains(var_name) {
                        self.get_function(var_name.as_str())
                            .as_global_value()
                            .as_basic_value_enum()
                    } else {
                        let var_fn = self.get_function(var_name.as_str());
                        self.builder
                            .build_call(var_fn, &[], format!("_{}_call", var_name).as_str())
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left()
                    }
                }
            },

            mir::Expr::TypeInst { tag, params } => {
                // First field is tag
                let type_struct = self.ptr_struct_type(params.len() + 1);
                let type_struct_size = type_struct.size_of().unwrap();

                let tag_value = self.compile_literal(&crate::literal::Literal::Int(*tag));
                let values = std::iter::once(tag_value).chain(
                    params
                        .into_iter()
                        .map(|param_name| self.variables[param_name].as_basic_value_enum()),
                );

                let type_struct_ptr = self
                    .builder
                    .build_call(
                        self.get_function("GC_malloc"),
                        &[type_struct_size.into()],
                        "_type_inst_alloc",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left();

                // Store values
                for (i, value) in values.into_iter().enumerate() {
                    let val_ptr = self
                        .builder
                        .build_struct_gep(
                            type_struct,
                            type_struct_ptr.into_pointer_value(),
                            i as u32,
                            "_env_field",
                        )
                        .unwrap();

                    self.builder.build_store(val_ptr, value).unwrap();
                }

                type_struct_ptr.into_pointer_value().as_basic_value_enum()
            }

            mir::Expr::Closure { fun_name, env } => {
                let fun_value = self.get_function(&fun_name);

                // Create environment struct
                let env_type = self.ptr_struct_type(env.len());
                let env_type_size = env_type.size_of().unwrap();

                let env_values = env
                    .into_iter()
                    .map(|v| self.variables[v].as_basic_value_enum())
                    .collect::<Vec<_>>();

                let env_ptr = self
                    .builder
                    .build_call(
                        self.get_function("GC_malloc"),
                        &[env_type_size.into()],
                        "_env_malloc",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left();

                // Store closed over variables
                for (i, env_value) in env_values.into_iter().enumerate() {
                    let val_ptr = self
                        .builder
                        .build_struct_gep(
                            env_type,
                            env_ptr.into_pointer_value(),
                            i as u32,
                            "_env_field",
                        )
                        .unwrap();

                    self.builder.build_store(val_ptr, env_value).unwrap();
                }

                // Create closure struct
                let closure_type = self.ptr_struct_type(2);
                let closure_type_size = closure_type.size_of().unwrap();

                let closure_ptr = self
                    .builder
                    .build_call(
                        self.get_function("GC_malloc"),
                        &[closure_type_size.into()],
                        "_closure_malloc",
                    )
                    .unwrap()
                    .as_any_value_enum()
                    .into_pointer_value();

                // Store closure fields (this is just env, and function)
                let closure_env_ptr = self
                    .builder
                    .build_struct_gep(closure_type, closure_ptr, 0, "_closure_env")
                    .unwrap();
                let closure_fn_ptr = self
                    .builder
                    .build_struct_gep(closure_type, closure_ptr, 1, "_closure_fn")
                    .unwrap();

                self.builder
                    .build_store(closure_env_ptr, env_ptr.as_basic_value_enum())
                    .unwrap();
                self.builder
                    .build_store(closure_fn_ptr, fun_value.as_global_value())
                    .unwrap();

                closure_ptr.as_basic_value_enum()
            }

            mir::Expr::App { closure, arg } => {
                // Compile closure expression first so we have left-to-right evaluation
                let compiled_closure = self.compile_expr(closure);
                let compiled_arg = self.compile_expr(arg);

                let closure_type = self.ptr_struct_type(2);
                let closure_ptr = compiled_closure.as_any_value_enum().into_pointer_value();

                let fun_type = self.ptr_fn_type(2);

                let env_ptr = self
                    .builder
                    .build_struct_gep(closure_type, closure_ptr, 0, "_closure_env_ind")
                    .unwrap();
                let fun_ptr = self
                    .builder
                    .build_struct_gep(closure_type, closure_ptr, 1, "_closure_fn_ind")
                    .unwrap();

                let env_value = self
                    .builder
                    .build_load(self.ptr_type(), env_ptr, "_closure_env")
                    .unwrap();
                let fun_value = self
                    .builder
                    .build_load(self.ptr_type(), fun_ptr, "_closure_fun")
                    .unwrap();

                self.builder
                    .build_indirect_call(
                        fun_type,
                        fun_value.into_pointer_value(),
                        &[env_value.into(), compiled_arg.into()],
                        "_call",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left()
            }

            mir::Expr::Extern { fun_name, args } => {
                // Just a normal, uncurried function call
                self.builder
                    .build_call(
                        self.get_function(fun_name.as_str()),
                        args.into_iter()
                            .map(|arg| self.compile_expr(arg).into())
                            .collect::<Vec<_>>()
                            .as_slice(),
                        "_extern_call",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left()
            }

            mir::Expr::If { cond, texpr, fexpr } => {
                let then_block = self.append_block("if_then");
                let else_block = self.append_block("if_else");
                let merge_block = self.append_block("if_merge");

                let compiled_cond = self.compile_expr(cond);

                let unwrapped_cond = self
                    .builder
                    .build_call(
                        self.get_function("_fino_bool_get"),
                        &[compiled_cond.into()],
                        "_cond_unwrap_call",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left();

                self.builder
                    .build_conditional_branch(
                        unwrapped_cond.into_int_value(),
                        then_block,
                        else_block,
                    )
                    .unwrap();

                self.builder.position_at_end(then_block);
                let compiled_then = self.compile_expr(texpr);
                self.builder
                    .build_unconditional_branch(merge_block)
                    .unwrap();

                self.builder.position_at_end(else_block);
                let compiled_else = self.compile_expr(fexpr);
                self.builder
                    .build_unconditional_branch(merge_block)
                    .unwrap();

                self.builder.position_at_end(merge_block);
                let phi = self.builder.build_phi(self.ptr_type(), "_phi").unwrap();

                phi.add_incoming(&[(&compiled_then, then_block), (&compiled_else, else_block)]);

                phi.as_basic_value()
            }

            mir::Expr::Let { name, aexpr, body } => {
                // Allocate variable
                let var_ptr = self
                    .builder
                    .build_alloca(self.ptr_type(), format!("_{}_store", name).as_str())
                    .unwrap();

                let compiled_aexpr = self.compile_expr(aexpr);
                self.builder.build_store(var_ptr, compiled_aexpr).unwrap();

                let var_value = self
                    .builder
                    .build_load(self.ptr_type(), var_ptr, name.as_str())
                    .unwrap();
                self.add_variable(name.clone(), var_value.into_pointer_value());

                self.compile_expr(body)
            }
        }
    }
}
