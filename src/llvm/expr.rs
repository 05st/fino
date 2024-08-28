use crate::mir;

use super::LLVMCodegen;

impl LLVMCodegen<'_, '_> {
    pub fn compile_expr(&mut self, expr: &mir::Expr) {
        match expr {
            mir::Expr::Lit(_) => todo!(),
            mir::Expr::Var(_) => todo!(),
            mir::Expr::Closure { fun_name, free_vars } => todo!(),
            mir::Expr::App { fun, arg } => todo!(),
            mir::Expr::If { cond, texpr, fexpr } => todo!(),
            mir::Expr::Let { name, aexpr, body } => todo!(),
        }
    }
}
