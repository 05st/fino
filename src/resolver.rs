use std::collections::HashMap;

use crate::{
    ast::*,
    cache::{CompilerCache, DefinitionId, ModuleId},
    error::{Error, ErrorKind}
};

struct NameResolver<'a> {
    compiler_cache: &'a mut CompilerCache,
    exported_scopes: HashMap<ModuleId, Scope>,
}

struct Scope {
    items: HashMap<Vec<String>, DefinitionId>,
    local: HashMap<String, DefinitionId>,
}

impl<'a> NameResolver<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> NameResolver {
        NameResolver {
            compiler_cache,
            exported_scopes: HashMap::new(),
        }
    }
}

pub fn resolve_program(compiler_cache: &mut CompilerCache) -> Result<(), Error> {
    let mut resolver = NameResolver::new(compiler_cache);
    Ok(())
}
