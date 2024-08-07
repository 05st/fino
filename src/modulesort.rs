use std::collections::HashMap;

use crate::{ast::{Module, NodeId}, cache::CompilerCache, error::{Error, ErrorKind}};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSort<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_map: HashMap<Vec<String>, Module>,
    module_state_map: HashMap<Vec<String>, ModuleState>,
    sorted: Vec<Module>,
}

impl ModuleSort<'_> {
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

pub fn toposort_modules(compiler_cache: &mut CompilerCache, modules: Vec<Module>) -> Result<Vec<Module>, Error> {
    let mut module_map: HashMap<Vec<String>, Module> = HashMap::new();
    let mut queue = Vec::new();

    // Move all modules to module_name_map
    for module in modules {
        queue.push(module.module_name.clone());
        module_map.insert(module.module_name.clone(), module);
    }

    let mut module_sort = ModuleSort {
        compiler_cache,
        module_map: module_map,
        module_state_map: HashMap::new(),
        sorted: Vec::new(),
    };

    for module_name in queue {
        module_sort.dfs(module_name, None)?;
    }

    Ok(module_sort.sorted)
}
