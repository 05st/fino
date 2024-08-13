use std::collections::HashMap;

use crate::{
    ast::ExprId,
    types::Type,
};

pub struct CompilerCache {
    pub expr_type_map: HashMap<ExprId, Type>,
}

impl CompilerCache {
    pub fn new() -> CompilerCache {
        CompilerCache {
            expr_type_map: HashMap::new(),
        }
    }
}
