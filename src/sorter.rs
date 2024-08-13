use std::collections::HashMap;

use crate::{
    ast::{Location, Module, ModuleId},
    error::{Error, ErrorKind},
};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSorter {
    module_map: HashMap<ModuleId, Module>,
    module_id_map: HashMap<Vec<String>, ModuleId>,
    module_state_map: HashMap<ModuleId, ModuleState>,
    sorted_program: Vec<Module>,
}

impl ModuleSorter {
    fn new(module_map: HashMap<ModuleId, Module>, module_id_map: HashMap<Vec<String>, ModuleId>) -> ModuleSorter {
        ModuleSorter {
            module_map,
            module_id_map,
            module_state_map: HashMap::new(),
            sorted_program: Vec::new(),
        }
    }

    // TODO:
    // Recover and report the cycle when one is detected
    fn dfs(&mut self, module_id: ModuleId, import_location: Option<&Location>) -> Result<(), Error> {
        use ModuleState::*;

        match self.module_state_map.get(&module_id) {
            Some(state) => match state {
                Processing => return Err(Error::new(ErrorKind::CircularDependency, import_location.unwrap().clone())),
                Processed => return Ok(()),
            },
            None => self.module_state_map.insert(module_id.clone(), Processing),
        };

        let module = self
            .module_map
            .remove(&module_id)
            // Need to use ok_or_else() with a thunk since ok_or() is eagerly evaluated and
            // will panic on the unwrap() when import_node_id is None for top-level dfs()
            // calls.
            .ok_or_else(|| Error::new(ErrorKind::UnknownModule, import_location.unwrap().clone()))?;

        for import in &module.imports {
            self.dfs(self.module_id_map[&import.module_name].clone(), Some(&import.location))?;
        }

        self.sorted_program.push(module);
        self.module_state_map.insert(module_id.clone(), Processed);

        Ok(())
    }
}

pub fn sort_program(program: &mut Vec<Module>) -> Result<(), Error> {
    let mut module_map = HashMap::new();
    let mut module_id_map = HashMap::new();
    let mut queue = Vec::new();

    // Move all modules to module_name_map
    while let Some(module) = program.pop() {
        queue.push(module.module_id.clone());
        module_id_map.insert(module.module_name.clone(), module.module_id.clone());
        module_map.insert(module.module_id.clone(), module);
    }

    let mut sorter = ModuleSorter::new(module_map, module_id_map);
    for module_id in queue {
        sorter.dfs(module_id, None)?;
    }

    *program = sorter.sorted_program;

    Ok(())
}
