use std::ops::Add;

use inkwell::{builder::Builder, context::Context, module::{Linkage, Module}, types::{AnyTypeEnum, BasicType, BasicTypeEnum}, values::{BasicValue, BasicValueEnum, InstructionValue, PointerMathValue, PointerValue}, AddressSpace};

use crate::{cache::CompilerCache, literal::Literal, mir};

struct Codegen<'a, 'ctx, 'cache> {
    compiler_cache: &'cache mut CompilerCache,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
}

impl<'a, 'ctx, 'cache> Codegen<'a, 'ctx, 'cache> {
    fn new(compiler_cache: &'cache mut CompilerCache, context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> Codegen<'a, 'ctx, 'cache> {
        Codegen {
            compiler_cache,
            context,
            builder,
            module,
        }
    }

    fn output(&self) {
        self.module.print_to_file("test.ll").unwrap();
    }

    fn declare_runtime(&self) {
        macro_rules! declare_wrapper_fns {
            ( $prefix:literal, $llvm_type:ident ) => {
                self.module.add_function(
                    concat!($prefix, "_new"),
                    self.context.ptr_type(AddressSpace::default()).fn_type(&[self.context.$llvm_type().into()], false),
                    None,
                );
                self.module.add_function(
                    concat!($prefix, "_get"),
                    self.context.$llvm_type().fn_type(&[self.context.ptr_type(AddressSpace::default()).into()], false),
                    None,
                );
            };
        }

        self.module.add_global(self.context.ptr_type(AddressSpace::default()), None, "fino__unit_val");

        declare_wrapper_fns!("fino__bool", bool_type);
        declare_wrapper_fns!("fino__char", i8_type);
        declare_wrapper_fns!("fino__int", i32_type);
        declare_wrapper_fns!("fino__float", f32_type);
    }

    fn compile_literal(&self, literal: &Literal) -> BasicValueEnum {
        match literal {
            Literal::Int(_) => todo!(),
            Literal::Float(_) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Char(_) => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Unit => {
                self.module.get_global("fino__unit_val").unwrap().as_basic_value_enum()
            }
        }
    }

    fn compile_expr(&self, expr: &mir::Expr) -> BasicValueEnum {
        match expr {
            mir::Expr::Lit(literal) => self.compile_literal(literal),
            mir::Expr::Var(name) => self.module.get_global(name).unwrap().as_basic_value_enum(),
            mir::Expr::Closure { fun_name, free_vars } => todo!(),
            mir::Expr::App { fun, arg } => todo!(),
            mir::Expr::If { cond, texpr, fexpr } => todo!(),
            mir::Expr::Let { name, aexpr, body } => todo!(),
        }
    }

    fn compile_global(&self, global: &mir::Global) {
        let mut init_queue = Vec::new();

        match global {
            mir::Global::Function { name, params, body } => {
                let fn_value = self.module.add_function(&name, self.context.ptr_type(AddressSpace::default()).fn_type(&[self.context.ptr_type(AddressSpace::default()).into()], false), None);
                let fn_entry = self.context.append_basic_block(fn_value, "entry");
                self.builder.position_at_end(fn_entry);
                self.builder.build_return(Some(&self.compile_expr(body))).unwrap();
            }
            mir::Global::Variable { name, body } => {
                let global_value = self.module.add_global(self.context.ptr_type(AddressSpace::default()), None, name.as_str());
                init_queue.push((global_value, body));
            }
        }

        let init_fn = self.module.add_function("_fino__init_globals", self.context.void_type().fn_type(&[], false), None);
        let init_fn_entry = self.context.append_basic_block(init_fn, "entry");
        self.builder.position_at_end(init_fn_entry);
        for (global_value, body) in init_queue {
            global_value.set_initializer(&BasicValueEnum::PointerValue(self.context.ptr_type(AddressSpace::default()).const_null()));
            self.builder.build_store(global_value.as_pointer_value(), self.compile_expr(body)).unwrap();
        }
        self.builder.build_return(None).unwrap();
    }
}

pub fn compile_program(compiler_cache: &mut CompilerCache, mir: Vec<mir::Global>) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("fino_llvm");

    let codegen = Codegen::new(compiler_cache, &context, &builder, &module);
    codegen.declare_runtime();

    for global in mir {
        codegen.compile_global(&global);
    }

    codegen.output();
}
