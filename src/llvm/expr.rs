use inkwell::values::{BasicValue, BasicValueEnum};

use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx, 'm> LLVMCodegen<'a, 'ctx, 'm> {
    pub fn compile_expr(&mut self, expr: &mir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            mir::Expr::Lit(literal) => self.compile_literal(literal),

            mir::Expr::Var(var_name) => {
                match self.variables.get(var_name) {
                    Some(ptr) => {
                        self.builder.build_load(
                            self.ptr_type(),
                            *ptr,
                            var_name.as_str(),
                        ).unwrap().as_basic_value_enum()
                    }

                    // If it's not a local variable, it must be a function or global
                    None => {
                        if self.functions.contains(var_name) {
                            self.get_function(var_name.as_str()).as_global_value().as_basic_value_enum()
                        } else {
                            self.get_global(var_name.as_str()).as_basic_value_enum()
                        }
                    }
                }
            }

            mir::Expr::Closure { fun_name, free_vars } => todo!(),

            mir::Expr::App { fun, arg } => todo!(),

            mir::Expr::If { cond, texpr, fexpr } => {
                let then_block = self.append_block();
                let else_block = self.append_block();
                let merge_block = self.append_block();

                let compiled_cond = self.compile_expr(cond);

                let unwrapped_cond = self.builder.build_call(
                    self.get_function("_fino_bool_get"),
                    &[compiled_cond.into()],
                    "call"
                ).unwrap().try_as_basic_value().unwrap_left();

                self.builder.build_conditional_branch(unwrapped_cond.into_int_value(), then_block, else_block).unwrap();

                self.builder.position_at_end(then_block);
                let compiled_then = self.compile_expr(texpr);
                self.builder.build_unconditional_branch(merge_block).unwrap();

                self.builder.position_at_end(else_block);
                let compiled_else = self.compile_expr(fexpr);
                self.builder.build_unconditional_branch(merge_block).unwrap();

                self.builder.position_at_end(merge_block);
                let phi = self.builder.build_phi(self.ptr_type(), "phi").unwrap();

                phi.add_incoming(&[
                    (&compiled_then, then_block),
                    (&compiled_else, else_block),
                ]);

                phi.as_basic_value()
            }

            mir::Expr::Let { name, aexpr, body } => todo!(),
        }
    }
}
