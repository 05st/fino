use std::{collections::{HashMap, HashSet}, path::Path};

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
mod toplevel;
mod literal;

struct LLVMCodegen<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashSet<String>,

    cur_function: Option<FunctionValue<'ctx>>,
    entry_point: Option<FunctionValue<'ctx>>,

    string_lits: HashSet<String>,
    string_lit_globals: HashMap<String, GlobalValue<'ctx>>,
}

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    fn new(context: &'ctx Context, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> LLVMCodegen<'a, 'ctx> {
        LLVMCodegen {
            context,
            builder,
            module,
            variables: HashMap::new(),
            functions: HashSet::new(),
            cur_function: None,
            entry_point: None,
            string_lits: HashSet::new(),
            string_lit_globals: HashMap::new(),
        }
    }

    fn get_function(&self, name: &str) -> FunctionValue<'ctx> {
        self.module.get_function(name).expect(format!("Unknown LLVM function {}", name).as_str())
    }

    fn get_string_literal(&mut self, lit: impl Into<String>) -> GlobalValue<'ctx> {
        let str: String = lit.into();
        if self.string_lits.contains(&str) {
            self.string_lit_globals[&str].clone()
        } else {
            let mut charcodes: Vec<_> = str.clone().chars().map(|c| c as u8).collect();
            charcodes.push(0);

            let array_type = self.context.i8_type().array_type(charcodes.len() as u32);
            let array_values: Vec<_> = charcodes
                .iter()
                .map(|c| self.context.i8_type().const_int((*c).into(), false))
                .collect();
            
            let global = self.module.add_global(array_type, None, str.as_str());
            global.set_initializer(&self.context.i8_type().const_array(&array_values));

            self.string_lits.insert(str.clone());
            self.string_lit_globals.insert(str, global);

            global
        }
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

    fn declare_runtime(&self) {
        // Utility functions to help define builtin functions
        let declare_wrapper_fns = |prefix, param_types: Vec<_>| {
            self.module.add_function(
                format!("{}_new", prefix).as_str(),
                self.ptr_type().fn_type(param_types.as_slice(), false),
                None,
            );
            self.module.add_function(
                format!("{}_print", prefix).as_str(),
                self.ptr_type().fn_type(&[self.ptr_type().into()], false),
                None,
            );
        };

        declare_wrapper_fns("_fino_bool", vec![self.context.bool_type().into()]);
        declare_wrapper_fns("_fino_char", vec![self.context.i8_type().into()]);
        declare_wrapper_fns("_fino_int", vec![self.context.i32_type().into()]);
        declare_wrapper_fns("_fino_float", vec![self.context.f32_type().into()]);
        declare_wrapper_fns("_fino_string", vec![self.ptr_type().into(), self.context.i32_type().into()]);

        let binary_oper_fns = vec!["_fino_string_concat"];
        for fn_name in binary_oper_fns {
            self.module.add_function(
                fn_name,
                self.ptr_type().fn_type(&[self.ptr_type().into()].repeat(2), false),
                None,
            );
        }

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
        self.module.print_to_file("a.ll").unwrap();
        self.module.write_bitcode_to_path(Path::new("a.bc"));
    }
}

pub fn compile_llvm(mir: Vec<mir::Toplevel>) {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("fino_llvm");

    let mut codegen = LLVMCodegen::new(&context, &builder, &module);

    codegen.declare_runtime();

    for toplevel in mir.iter() {
        codegen.declare_toplevel(toplevel);
    }

    for toplevel in mir.iter() {
        codegen.compile_toplevel(toplevel);
    }

    codegen.finish();
}
