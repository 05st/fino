use std::collections::{HashMap, HashSet};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::PointerType,
    values::{
        BasicValueEnum,
        FunctionValue,
        GlobalValue,
        PointerValue
    },
    AddressSpace
};

use crate::mir;

mod expr;
mod global;
mod literal;

struct LLVMCodegen<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashSet<String>,

    main_fn_entry_block: BasicBlock<'ctx>,
    global_init_block: BasicBlock<'ctx>,
}

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> LLVMCodegen<'a, 'ctx> {
        let main_fn_type = context.void_type().fn_type(&[], false);
        let main_fn = module.add_function("main", main_fn_type, None);

        let global_init_block = context.append_basic_block(main_fn, "entry");
        let main_fn_entry_block = context.append_basic_block(main_fn, "run");

        LLVMCodegen {
            context,
            builder,
            module,
            variables: HashMap::new(),
            functions: HashSet::new(),
            main_fn_entry_block,
            global_init_block,
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

    fn add_variable(&mut self, name: String, value: PointerValue<'ctx>) {
        self.variables.insert(name, value);
    }

    fn add_global_init(&mut self, global_value: GlobalValue<'ctx>, init_value: BasicValueEnum<'ctx>) {
        global_value.set_initializer(&self.ptr_type().const_null());

        self.builder.position_at_end(self.global_init_block);
        self.builder.build_store(global_value.as_pointer_value(), init_value).unwrap();
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
            };
        }

        self.module.add_global(self.ptr_type(), None, "_fino_unit_val");

        declare_wrapper_fns!("_fino_bool", bool_type);
        declare_wrapper_fns!("_fino_char", i8_type);
        declare_wrapper_fns!("_fino_int", i32_type);
        declare_wrapper_fns!("_fino_float", f32_type);
    }

    fn finish(&mut self) {
        // Add terminator instruction for global_init_block to branch to main_fn_entry_block
        self.builder.position_at_end(self.global_init_block);
        self.builder.build_unconditional_branch(self.main_fn_entry_block).unwrap();

        // Build return for main function
        self.builder.position_at_end(self.main_fn_entry_block);
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

    for global in mir {
        codegen.compile_global(&global);
    }

    codegen.finish();
}
