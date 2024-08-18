use std::collections::{BTreeMap, HashMap, VecDeque};

use crate::{
    ast::*,
    cache::{CompilerCache, Definition, DefinitionId, ModuleId},
    error::{Error, ErrorKind},
    location::Location
};

struct NameResolver<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_exports: HashMap<ModuleId, Environment>,
    definition_count: usize,
    environment: Environment,
}

#[derive(Clone, Debug)]
struct Environment {
    // BTreeMap is faster for iteration
    items: BTreeMap<Vec<String>, DefinitionId>,
    // Stack of local scopes
    scopes: VecDeque<im::HashMap<String, DefinitionId>>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            items: BTreeMap::new(),
            scopes: VecDeque::new(),
        }
    }

    // Move all top-level definitions from other environment into self
    fn extend(&mut self, other: Environment) {
        self.items.extend(other.items);
    }

    fn insert_item(&mut self, module_path: &Vec<String>, item_name: String, definition_id: DefinitionId, location: &Location) -> Result<(), Error> {
        let mut qualified_name = module_path.clone();
        qualified_name.push(item_name);

        match self.items.get(&qualified_name) {
            Some(_) => Err(Error::new(ErrorKind::Redefinition(qualified_name), location.clone())),
            None => {
                self.items.insert(qualified_name, definition_id);
                Ok(())
            }
        }
    }

    // Allows shadowing
    fn push_scope(&mut self, name: String, definition_id: DefinitionId) {
        match self.scopes.back() {
            Some(previous) => self.scopes.push_back(previous.update(name, definition_id)),
            None => {
                let mut top_scope = im::HashMap::new();
                top_scope.insert(name, definition_id);
                self.scopes.push_back(top_scope);
            }
        }
    }
    
    fn pop_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn lookup_qualified_name(&self, name: &Vec<String>, location: &Location) -> Result<DefinitionId, Error> {
        match self.items.get(name) {
            Some(definition_id) => Ok(definition_id.clone()),
            None => Err(Error::new(ErrorKind::UnknownVariable(name.clone()), location.clone())),
        }
    }

    fn lookup_unqualified_name(&self, name: &String, location: &Location) -> Result<DefinitionId, Error> {
        // Check local definitions first
        for scope in &self.scopes {
            match scope.get(name) {
                Some(definition_id) => return Ok(definition_id.clone()),
                None => (),
            }
        }

        // Otherwise, iterate through all items in scope. This is suboptimal, since
        // we only need to check items that have the same unqualified portion of their
        // qualified name as the unqualified name we are searching for.
        let candidates = self.items
            .iter()
            .filter(|(qualified_name, _)| qualified_name.last().unwrap() == name)
            .collect::<Vec<_>>();

        let num_candidates = candidates.len();
        if num_candidates > 1 {
            // If there are multiple possible definitions, throw an error
            Err(Error::new(ErrorKind::MultipleDefinitions(name.clone()), location.clone()))
        } else if num_candidates == 0 {
            // If there are no possible definitions, throw an error
            Err(Error::new(ErrorKind::UnknownVariable(vec![name.clone()]), location.clone()))
        } else {
            Ok(candidates.first().unwrap().1.clone())
        }
    }
}

impl<'a> NameResolver<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> NameResolver {
        NameResolver {
            compiler_cache,
            module_exports: HashMap::new(),
            definition_count: 0,
            environment: Environment::new(),
        }
    }

    fn new_definition(&mut self, location: Location) -> DefinitionId {
        let definition_id = DefinitionId(self.definition_count);
        self.compiler_cache.definitions.push(Definition {
            location,
        });
        self.definition_count += 1;
        definition_id
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), Error> {
        match &mut expr.kind {
            ExprKind::Lit(_) => Ok(()),
            ExprKind::Var { name, definition_id } => {
                *definition_id = Some(match name {
                    Name::Unqualified(ident) => self.environment.lookup_unqualified_name(ident, &expr.location)?,
                    Name::Qualified(qual) => self.environment.lookup_qualified_name(qual, &expr.location)?,
                });

                Ok(())
            }
            ExprKind::App { ref mut fun, ref mut arg } => {
                self.resolve_expr(fun)?;
                self.resolve_expr(arg)
            }
            ExprKind::Lam { param_name, ref mut body, param_definition_id } => {
                let new_definition_id = self.new_definition(expr.location.clone());
                *param_definition_id = Some(new_definition_id.clone());

                self.environment.push_scope(param_name.clone(), new_definition_id);
                self.resolve_expr(body)?;
                self.environment.pop_scope();

                Ok(())
            }
            ExprKind::Let { name, ref mut aexpr, ref mut body, definition_id } => {
                self.resolve_expr(aexpr)?;

                let new_definition_id = self.new_definition(expr.location.clone());
                *definition_id = Some(new_definition_id.clone());

                self.environment.push_scope(name.clone(), new_definition_id);
                self.resolve_expr(body)?;
                self.environment.pop_scope();

                Ok(())
            }
            ExprKind::If { cond, ref mut texpr, ref mut fexpr } => {
                self.resolve_expr(cond)?;
                self.resolve_expr(texpr)?;
                self.resolve_expr(fexpr)
            }
        }
    }

    // Adds all exported definitions from imported modules into self.current_scope
    fn process_module_imports(&mut self, module: &Module) {
        for import in &module.imports {
            self.environment.extend(self.module_exports[import.module_id.as_ref().unwrap()].clone())
        }
    }

    // Insert a scope into module_exports which contains all top-level definitions
    // exported by the module. Assumes all definition ids have been resolved in the
    // module.
    fn setup_module_exports(&mut self, module: &Module) -> Result<(), Error> {
        let mut exports_env = Environment::new();

        for export in &module.exports {
            match export {
                Export::Item { item_name, location, definition_id } => {
                    exports_env.insert_item(
                        &module.module_path,
                        item_name.clone(),
                        definition_id.clone().unwrap(),
                        location,
                    )?;
                }
                Export::Module { module_path: _, location: _, module_id } => {
                    exports_env.extend(self.module_exports[module_id.as_ref().unwrap()].clone());
                }
            }
        }

        self.module_exports.insert(module.module_id.clone().unwrap(), exports_env);

        Ok(())
    }

    fn resolve_module(&mut self, module: &mut Module) -> Result<(), Error> {
        self.environment = Environment::new();

        // Set up definitions for externs
        for ext in module.externs.iter_mut() {
            let definition_id = self.new_definition(ext.location.clone());
            ext.definition_id = Some(definition_id.clone());

            self.environment.insert_item(
                &module.module_path,
                ext.name.clone(),
                definition_id,
                &ext.location,
            )?;
        }

        // Set up definitions for items
        for item in module.items.iter_mut() {
            let definition_id = self.new_definition(item.location.clone());
            item.definition_id = Some(definition_id.clone());

            self.environment.insert_item(
                &module.module_path,
                item.name.clone(),
                definition_id,
                &item.location,
            )?;
        }

        // Resolve imports and exports, all referenced modules should exist at this point
        let mut imported_modules = HashMap::new();
        for import in module.imports.iter_mut() {
            // import.module_id is already resolved during sorting
            match imported_modules.get(&import.module_path) {
                Some(_) => {
                    // If module is already in imported_modules, it was imported more than once
                    return Err(Error::new(ErrorKind::AlreadyImportedModule(import.module_path.clone()), import.location.clone()));
                }
                None => {
                    imported_modules.insert(import.module_path.clone(), import.module_id.clone().unwrap());
                }
            }
        }
        for export in module.exports.iter_mut() {
            match export {
                Export::Item { item_name, location, ref mut definition_id } => {
                    *definition_id = Some(self.environment.lookup_unqualified_name(item_name, location)?);
                }
                Export::Module { module_path, location, module_id } => {
                    // If the exported module is not in imported_modules, then it exists but isn't imported
                    *module_id = Some(imported_modules.remove(module_path).ok_or(
                        Error::new(ErrorKind::ExportedUnimportedModule(module_path.clone()), location.clone())
                    )?);
                }
            }
        }

        // Resolve items
        self.process_module_imports(module);
        for item in module.items.iter_mut() {
            self.resolve_expr(&mut item.expr)?;
        }

        self.setup_module_exports(module)?;

        Ok(())
    }

}

// The definitions field for compiler_cache should be empty before name resolution
pub fn resolve_program(compiler_cache: &mut CompilerCache) -> Result<(), Error> {
    let mut resolver = NameResolver::new(compiler_cache);

    let mut queue = Vec::new();
    while let Some(mut module) = resolver.compiler_cache.modules.pop_front() {
        resolver.resolve_module(&mut module)?;
        queue.push(module);
    }

    while let Some(module) = queue.pop() {
        resolver.compiler_cache.modules.push_front(module);
    }

    Ok(())
}
