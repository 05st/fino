use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::literal::Literal;

use super::LLVMCodegen;

impl<'a, 'ctx, 'm> LLVMCodegen<'a, 'ctx, 'm> {
    fn build_new_literal_call(&self, fn_name: &str, args: &[BasicMetadataValueEnum<'ctx>]) -> BasicValueEnum<'ctx> {
        self.builder
            .build_call(self.get_function(fn_name), args, "call")
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
    }

    pub fn compile_literal(&mut self, literal: &Literal) -> BasicValueEnum<'ctx> {
        match literal {
            Literal::Int(i) => {
                let i32_value = self.context.i32_type().const_int(*i as u64, true);
                self.build_new_literal_call("_fino_int_new", &[i32_value.into()])
            }

            Literal::Float(f) => {
                let f32_value = self.context.f32_type().const_float(*f);
                self.build_new_literal_call("_fino_float_new", &[f32_value.into()])
            }

            Literal::String(_) => {
                todo!()
            }

            Literal::Char(c) => {
                let char_value = self.context.i8_type().const_int(*c as u64, true);
                self.build_new_literal_call("_fino_char_new", &[char_value.into()])
            }

            Literal::Bool(b) => {
                let bool_value = self.context.bool_type().const_int(*b as u64, true);
                self.build_new_literal_call("_fino_bool_new", &[bool_value.into()])
            }

            Literal::Unit => self.get_global("_fino_unit_val").as_basic_value_enum(),
        }
    }
}
