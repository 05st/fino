use std::collections::HashMap;

use crate::{ast::Module, error::Error};

enum ModuleState {
    Processing,
    Processed,
}

struct ModuleSort {
    module_map: HashMap<Vec<String>, Module>,
    module_state_map: HashMap<Vec<String>, ModuleState>,
    sorted: Vec<Module>,
}

impl ModuleSort {
    fn dfs(&mut self, module_name: Vec<String>) -> Result<(), Error> {
        use ModuleState::*;

        match self.module_state_map.get(&module_name) {
            Some(state) => match state {
                Processing => panic!("circular depedency"),
                Processed => return Ok(()),
            },
            None => self.module_state_map.insert(module_name.clone(), Processing),
        };

        let module = self.module_map.remove(&module_name).expect("undefined module");
        for import in &module.imports {
            self.dfs(import.module_name.clone())?;
        }

        self.sorted.push(module);
        self.module_state_map.insert(module_name, Processed);

        Ok(())
    }
}

pub fn toposort_modules(modules: Vec<Module>) -> Result<Vec<Module>, Error> {
    let mut module_map: HashMap<Vec<String>, Module> = HashMap::new();
    let mut queue = Vec::new();

    // Move all modules to module_name_map
    for module in modules {
        queue.push(module.module_name.clone());
        module_map.insert(module.module_name.clone(), module);
    }

    let mut module_sort = ModuleSort {
        module_map: module_map,
        module_state_map: HashMap::new(),
        sorted: Vec::new(),
    };

    for module_name in queue {
        module_sort.dfs(module_name)?;
    }

    Ok(module_sort.sorted)
}
