use std::collections::HashMap;

use crate::{
    ast::{Location, Module},
    cache::{CompilerCache, ModuleId},
    error::{Error, ErrorKind}
};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSorter<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_map: HashMap<Vec<String>, Module>,
    module_state: HashMap<Vec<String>, ModuleState>,
    sorted_modules: Vec<Module>,
    processing_queue: Vec<Vec<String>>,
}

impl<'a> ModuleSorter<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> ModuleSorter {
        let mut module_map = HashMap::new();
        let mut processing_queue = Vec::new();

        // Initialize processing_queue with all module paths
        // Move modules from compiler_cache.modules to module_map
        while let Some(module) = compiler_cache.modules.pop() {
            processing_queue.push(module.module_path.clone());
            module_map.insert(module.module_path.clone(), module);
        }

        ModuleSorter {
            compiler_cache,
            module_map,
            module_state: HashMap::new(),
            sorted_modules: Vec::new(),
            processing_queue,
        }
    }

    // Topological sort by depth-first-search
    fn process_module(&mut self, module_path: Vec<String>, import_location: Option<Location>) -> Result<ModuleId, Error> {
        use ModuleState::*;

        match self.module_state.get(&module_path) {
            Some(state) => match state {
                Processing => return Err(Error::new(ErrorKind::CircularDependency, import_location.unwrap().clone())),
                Processed => return Ok(self.compiler_cache.module_ids[&module_path].clone()),
            },
            None => self.module_state.insert(module_path.clone(), Processing),
        };

        // The module hasn't begun processing yet, so it should be in self.module_map
        // otherwise, it means an undefined module was attempted to be imported
        // Note we use a thunk with ok_or_else since ok_or is eagerly evaluated and
        // panics for base calls of process_module when import_location is None
        let mut module = self.module_map
            .remove(&module_path)
            .ok_or_else(|| Error::new(ErrorKind::UnknownModule, import_location.unwrap().clone()))?;

        for import in module.imports.iter_mut() {
            import.module_id = Some(self.process_module(import.module_path.clone(), Some(import.location.clone()))?);
        }

        *self.module_state.get_mut(&module.module_path).unwrap() = ModuleState::Processed;

        let module_id = ModuleId(self.sorted_modules.len());
        module.module_id = Some(module_id.clone());
        self.sorted_modules.push(module);

        self.compiler_cache.module_ids.insert(module_path, module_id.clone());

        Ok(module_id)
    }

    fn process(&mut self) -> Result<(), Error> {
        while let Some(module_path) = self.processing_queue.pop() {
            self.process_module(module_path, None)?;
        }

        Ok(())
    }
}

pub fn sort_program(compiler_cache: &mut CompilerCache) -> Result<(), Error> {
    let mut module_sorter = ModuleSorter::new(compiler_cache);
    module_sorter.process()?;

    compiler_cache.modules = module_sorter.sorted_modules;

    Ok(())
}
