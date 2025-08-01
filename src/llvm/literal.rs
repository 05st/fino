use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::literal::Literal;

use super::LLVMCodegen;

impl<'a, 'ctx> LLVMCodegen<'a, 'ctx> {
    fn build_new_literal_call(
        &self,
        fn_name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        self.builder
            .build_call(
                self.get_function(fn_name),
                args,
                format!("_{}_call", fn_name).as_str(),
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_left()
    }

    pub fn compile_literal(&mut self, literal: &Literal) -> BasicValueEnum<'ctx> {
        match literal {
            Literal::Int(i) => {
                let i64_value = self.context.i64_type().const_int(*i as u64, true);
                self.build_new_literal_call("_fino_int_new", &[i64_value.into()])
            }

            Literal::Float(f) => {
                let f64_value = self.context.f64_type().const_float(*f);
                self.build_new_literal_call("_fino_float_new", &[f64_value.into()])
            }

            Literal::String(s) => {
                let len_value = self.context.i64_type().const_int(s.len() as u64, false);
                let string_ptr = self.get_string_literal(s).as_pointer_value();
                self.build_new_literal_call(
                    "_fino_string_new",
                    &[string_ptr.into(), len_value.into()],
                )
            }

            Literal::Char(c) => {
                let char_value = self.context.i8_type().const_int(*c as u64, true);
                self.build_new_literal_call("_fino_char_new", &[char_value.into()])
            }

            Literal::Bool(b) => {
                let bool_value = self.context.bool_type().const_int(*b as u64, true);
                self.build_new_literal_call("_fino_bool_new", &[bool_value.into()])
            }

            Literal::Unit => self.ptr_type().const_null().as_basic_value_enum(),
        }
    }
}
