use inkwell::{builder::Builder, context::Context, module::Module, values::BasicValueEnum};

use crate::{cache::CompilerCache, literal::Literal, mir};

struct Codegen<'a, 'ctx> {
    compiler_cache: &'a mut CompilerCache,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn new(compiler_cache: &'a mut CompilerCache, context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> Codegen<'a, 'ctx> {
        Codegen {
            compiler_cache,
            context,
            builder,
            module,
        }
    }

    fn compile_literal(&self, literal: &Literal) -> BasicValueEnum {
        match literal {
            Literal::Int(_) => {
                todo!()
            }
            Literal::Float(_) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Char(_) => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Unit => todo!(),
        }
    }

    fn compile_expr(&self, expr: &mir::Expr) -> BasicValueEnum {
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

pub fn compile_program(compiler_cache: &mut CompilerCache) {
    let llvm_context = Context::create();
    let builder = llvm_context.create_builder();
    let module = llvm_context.create_module("fino");

    let codegen = Codegen::new(compiler_cache, &llvm_context, &builder, &module);


}
