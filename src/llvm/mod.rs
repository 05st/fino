use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::mir;

mod expr;
mod global;

struct LLVMCodegen<'a, 'ctx> {
    context: &'a Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
}

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    fn new(context: &'a Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> LLVMCodegen<'a, 'ctx> {
        LLVMCodegen {
            context,
            builder,
            module,
        }
    }

    fn get_function(&self, name: &str) -> FunctionValue {
        self.module.get_function(name).expect(format!("Unknown LLVM function {}", name).as_str())
    }
}

pub fn compile_llvm(mir: Vec<mir::Global>) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("fino_llvm");

    let mut codegen = LLVMCodegen::new(&context, &builder, &module);

    for global in mir {
        codegen.compile_global(&global);
    }
}
