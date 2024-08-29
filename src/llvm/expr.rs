use inkwell::values::{BasicValue, BasicValueEnum};

use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
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
                todo!()
            }

            mir::Expr::Let { name, aexpr, body } => todo!(),
        }
    }
}
