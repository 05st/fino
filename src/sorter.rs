use std::collections::HashMap;

use crate::{ast::{Module, NodeId}, cache::CompilerCache, error::{Error, ErrorKind}};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSorter<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_map: HashMap<Vec<String>, Module>,
    module_state_map: HashMap<Vec<String>, ModuleState>,
    sorted: Vec<Module>,
}

impl ModuleSorter<'_> {
    fn new(compiler_cache: &mut CompilerCache, module_map: HashMap<Vec<String>, Module>) -> ModuleSorter {
        ModuleSorter {
            compiler_cache,
            module_map,
            module_state_map: HashMap::new(),
            sorted: Vec::new(),
        }
    }

    // TODO:
    // Recover and report the cycle when one is detected
    fn dfs(&mut self, module_name: Vec<String>, import_node_id: Option<&NodeId>) -> Result<(), Error> {
        use ModuleState::*;

        match self.module_state_map.get(&module_name) {
            Some(state) => match state {
                Processing => return Err(self.compiler_cache.make_error(ErrorKind::CircularDependency, import_node_id.unwrap())),
                Processed => return Ok(()),
            },
            None => self.module_state_map.insert(module_name.clone(), Processing),
        };

        let module = self
            .module_map
            .remove(&module_name)
            // Need to use ok_or_else() with a thunk since ok_or() is eagerly evaluated and
            // will panic on the unwrap() when import_node_id is None for top-level dfs()
            // calls.
            .ok_or_else(|| self.compiler_cache.make_error(ErrorKind::UnknownModule, import_node_id.unwrap()))?;

        for import in &module.imports {
            self.dfs(import.module_name.clone(), Some(&import.node_id))?;
        }

        self.sorted.push(module);
        self.module_state_map.insert(module_name, Processed);

        Ok(())
    }
}

pub fn sort_program(compiler_cache: &mut CompilerCache, modules: &mut Vec<Module>) -> Result<(), Error> {
    let mut module_map: HashMap<Vec<String>, Module> = HashMap::new();
    let mut queue = Vec::new();

    // Move all modules to module_name_map
    while let Some(module) = modules.pop() {
        queue.push(module.module_name.clone());
        module_map.insert(module.module_name.clone(), module);
    }

    let mut sorter = ModuleSorter::new(compiler_cache, module_map);

    for module_name in queue {
        sorter.dfs(module_name, None)?;
    }

    *modules = sorter.sorted;

    Ok(())
}
