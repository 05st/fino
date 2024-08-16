// Topologically sort modules
// create and assign module ids based on ordering

use std::collections::HashMap;

use crate::{
    ast::{Location, Module}, cache::{CompilerCache, ModuleId}, error::{Error, ErrorKind}
};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSorter<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_state_map: HashMap<ModuleId, ModuleState>,
    sorted_modules: Vec<Module>,
}

impl<'a> ModuleSorter<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> ModuleSorter {
        ModuleSorter {
            compiler_cache,
            module_state_map: HashMap::new(),
            sorted_modules: Vec::new(),
        }
    }

    // Topological sort by DFS
    // TODO:
    // Recover and report the cycle when one is detected
    fn process_module(&mut self, module_id: ModuleId, import_location: Option<&Location>) -> Result<(), Error> {
        use ModuleState::*;

        match self.module_state_map.get(&module_id) {
            Some(state) => match state {
                Processing => return Err(Error::new(ErrorKind::CircularDependency, import_location.unwrap().clone())),
                Processed => return Ok(()),
            },
            None => self.module_state_map.insert(module_id.clone(), Processing),
        };

        Ok(())
    }
}

pub fn sort_program(compiler_cache: &mut CompilerCache) -> Result<(), Error> {
    let module_sorter = ModuleSorter::new(compiler_cache);

    compiler_cache.modules = module_sorter.sorted_modules;

    Ok(())
}
