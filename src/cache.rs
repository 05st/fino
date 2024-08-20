use std::collections::{HashMap, VecDeque};

use crate::{ast::Module, location::Location, parser::Precedence};

#[derive(Default)]
pub struct CompilerCache {
    // Indexed by ModuleIds
    pub modules: VecDeque<Module>,
    // Indexed by DefinitionIds
    pub definitions: Vec<Definition>,
    // Maps operator name to operator precedence info
    pub operator_precedences: HashMap<String, Precedence>,
    // Maps module path to module id
    pub module_ids: HashMap<Vec<String>, ModuleId>,
}

#[derive(Debug)]
pub struct Definition {
    pub location: Location,
    pub mangled_name: Vec<String>,
    pub local: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId(pub usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefinitionId(pub usize);

macro_rules! impl_index {
    ( $index_type:ty, $elem_type:ty, $field_name:ident ) => {
        impl std::ops::Index<$index_type> for CompilerCache {
            type Output = $elem_type;

            fn index(&self, index: $index_type) -> &Self::Output {
                &self.$field_name[index.0]
            }
        }

        impl std::ops::IndexMut<$index_type> for CompilerCache {
            fn index_mut(&mut self, index: $index_type) -> &mut Self::Output {
                &mut self.$field_name[index.0]
            }
        }
    };
}

impl_index!(&ModuleId, Module, modules);
impl_index!(&DefinitionId, Definition, definitions);
