use inkwell::values::{BasicValue, BasicValueEnum};

use crate::mir;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    pub fn compile_expr(&mut self, expr: &mir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            mir::Expr::Lit(literal) => self.compile_literal(literal),

            mir::Expr::Var(var_name) => {
                self.builder.build_load(
                    self.context.i32_type(),
                    self.variables[var_name.as_str()],
                    var_name.as_str(),
                ).unwrap().as_basic_value_enum()
            }

            mir::Expr::Closure { fun_name, free_vars } => todo!(),
            mir::Expr::App { fun, arg } => todo!(),
            mir::Expr::If { cond, texpr, fexpr } => todo!(),
            mir::Expr::Let { name, aexpr, body } => todo!(),
        }
    }
}
