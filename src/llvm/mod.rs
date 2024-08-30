use std::collections::{HashMap, HashSet};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{PointerType, StructType},
    values::{
        FunctionValue, GlobalValue, PointerValue
    },
    AddressSpace
};

use crate::mir;

mod expr;
mod global;
mod literal;

struct LLVMCodegen<'a, 'ctx, 'm> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashSet<String>,

    cur_function: Option<FunctionValue<'ctx>>,

    entry_point: Option<FunctionValue<'ctx>>,
    global_inits: Vec<(GlobalValue<'ctx>, &'m mir::Expr)>,
}

impl<'a, 'ctx, 'm> LLVMCodegen<'a, 'ctx, 'm> {
    fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> LLVMCodegen<'a, 'ctx, 'm> {
        LLVMCodegen {
            context,
            builder,
            module,
            variables: HashMap::new(),
            functions: HashSet::new(),
            cur_function: None,
            entry_point: None,
            global_inits: Vec::new(),
        }
    }

    fn get_function(&self, name: &str) -> FunctionValue<'ctx> {
        self.module.get_function(name).expect(format!("Unknown LLVM function {}", name).as_str())
    }

    fn get_global(&self, name: &str) -> GlobalValue<'ctx> {
        self.module.get_global(name).expect(format!("Unknown LLVM global {}", name).as_str())
    }

    fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    fn ptr_struct(&self, size: usize) -> StructType<'ctx> {
        self.context.struct_type(&[self.ptr_type().into()].repeat(size), false)
    }

    fn add_variable(&mut self, name: String, value: PointerValue<'ctx>) {
        self.variables.insert(name, value);
    }

    fn enter_fn_block(&mut self, function: FunctionValue<'ctx>) -> BasicBlock<'ctx> {
        let block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(block);
        self.cur_function = Some(function);
        block
    }

    fn append_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.context.append_basic_block(self.cur_function.expect("Not inside a function"), name)
    }

    fn add_global_init(&mut self, global_value: GlobalValue<'ctx>, init_expr: &'m mir::Expr) {
        global_value.set_initializer(&self.ptr_type().const_null());
        self.global_inits.push((global_value, init_expr));
    }

    fn declare_runtime(&self) {
        // Utility macro to help define _new and _get functions
        macro_rules! declare_wrapper_fns {
            ( $prefix:literal, $llvm_type:ident ) => {
                self.module.add_function(
                    concat!($prefix, "_new"),
                    self.ptr_type().fn_type(&[self.context.$llvm_type().into()], false),
                    None,
                );
                self.module.add_function(
                    concat!($prefix, "_get"),
                    self.context.$llvm_type().fn_type(&[self.ptr_type().into()], false),
                    None,
                );
                self.module.add_function(
                    concat!($prefix, "_print"),
                    self.ptr_type().fn_type(&[self.context.$llvm_type().into()], false),
                    None,
                );
            };
        }

        declare_wrapper_fns!("_fino_bool", bool_type);
        declare_wrapper_fns!("_fino_char", i8_type);
        declare_wrapper_fns!("_fino_int", i32_type);
        declare_wrapper_fns!("_fino_float", f32_type);

        self.module.add_function(
            "GC_malloc",
            self.ptr_type().fn_type(&[self.context.i64_type().into()], false),
            None,
        );
    }

    fn finish(&mut self) {
        // Add main function
        let main_fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);

        self.enter_fn_block(main_fn);

        // Set up globals
        for (global_value, init_expr) in &self.global_inits.clone() { // TODO: Get rid of that .clone()
            let init_value = self.compile_expr(init_expr);
            self.builder.build_store(global_value.as_pointer_value(), init_value).unwrap();
        }

        // If there is an entry point
        if let Some(entry_fn) = self.entry_point {
            // Call fino main function
            self.builder.build_call(
                entry_fn,
                &[self.ptr_type().const_null().into()],
                "_main_call"
            ).unwrap();
        }

        // Build return for main function
        self.builder.build_return(None).unwrap();

        // Output to file
        self.module.print_to_file("test.ll").unwrap();
    }
}

pub fn compile_llvm(mir: Vec<mir::Global>) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("fino_llvm");

    let mut codegen = LLVMCodegen::new(&context, &builder, &module);

    codegen.declare_runtime();

    for global in mir.iter() {
        codegen.compile_global(global);
    }

    codegen.finish();
}
