use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use crate::{
    ast::*,
    cache::{CompilerCache, Definition, DefinitionId, DefinitionKind, ModuleId},
    error::{Error, ErrorKind},
    location::Location,
    types::Type,
};

struct NameResolver<'a> {
    compiler_cache: &'a mut CompilerCache,
    module_exports: HashMap<ModuleId, Environment>,
    definition_count: usize,
    environment: Environment,
    module_path: Option<Vec<String>>,
    type_variants: HashMap<DefinitionId, HashSet<String>>,
}

#[derive(Clone, Debug)]
struct Environment {
    // BTreeMap is faster for iteration
    toplevels: BTreeMap<Vec<String>, DefinitionId>,
    // Stack of local scopes
    scopes: VecDeque<im::HashMap<String, DefinitionId>>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            toplevels: BTreeMap::new(),
            scopes: VecDeque::new(),
        }
    }

    // Move all toplevels from other environment into self
    fn extend(&mut self, other: Environment) {
        self.toplevels.extend(other.toplevels);
    }

    fn insert_toplevel(
        &mut self,
        module_path: &Vec<String>,
        name: String,
        definition_id: DefinitionId,
        location: &Location,
    ) -> Result<(), Error> {
        let mut qualified_name = module_path.clone();
        qualified_name.push(name);

        match self.toplevels.get(&qualified_name) {
            Some(_) => Err(Error::new(
                ErrorKind::Redefinition(qualified_name),
                location.clone(),
            )),
            None => {
                self.toplevels.insert(qualified_name, definition_id);
                Ok(())
            }
        }
    }

    // Allows shadowing
    fn push_scope(&mut self, name: String, definition_id: DefinitionId) {
        match self.scopes.back() {
            Some(previous) => self.scopes.push_back(previous.update(name, definition_id)),
            None => self
                .scopes
                .push_back(im::HashMap::unit(name, definition_id)),
        }
    }

    fn push_scope_many(&mut self, entries: Vec<(String, DefinitionId)>) {
        match self.scopes.back() {
            Some(previous) => self
                .scopes
                .push_back(im::HashMap::from(entries).union(previous.clone())),
            None => self.scopes.push_back(im::HashMap::from(entries)),
        }
    }

    fn pop_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn lookup_qualified_name(
        &self,
        name: &Vec<String>,
        location: &Location,
    ) -> Result<DefinitionId, Error> {
        match self.toplevels.get(name) {
            Some(definition_id) => Ok(definition_id.clone()),
            None => Err(Error::new(
                ErrorKind::Undefined(name.clone()),
                location.clone(),
            )),
        }
    }

    fn lookup_unqualified_name(
        &self,
        name: &String,
        location: &Location,
    ) -> Result<DefinitionId, Error> {
        // Check local definitions first
        for scope in &self.scopes {
            match scope.get(name) {
                Some(definition_id) => return Ok(definition_id.clone()),
                None => (),
            }
        }

        // Otherwise, iterate through all toplevels in scope. This is suboptimal, since
        // we only need to check toplevels that have the same unqualified portion of their
        // qualified name as the unqualified name we are searching for.
        let candidates = self
            .toplevels
            .iter()
            .filter(|(qualified_name, _)| qualified_name.last().unwrap() == name)
            .collect::<Vec<_>>();

        let num_candidates = candidates.len();
        if num_candidates > 1 {
            // If there are multiple possible definitions, throw an error
            Err(Error::new(
                ErrorKind::MultipleDefinitions(name.clone()),
                location.clone(),
            ))
        } else if num_candidates == 0 {
            // If there are no possible definitions, throw an error
            Err(Error::new(
                ErrorKind::Undefined(vec![name.clone()]),
                location.clone(),
            ))
        } else {
            Ok(candidates.first().unwrap().1.clone())
        }
    }

    fn lookup_name(&self, name: &Name, location: &Location) -> Result<DefinitionId, Error> {
        match name {
            Name::Unqualified(ident) => self.lookup_unqualified_name(ident, location),
            Name::Qualified(qual) => self.lookup_qualified_name(qual, location),
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
            module_path: None,
            type_variants: HashMap::new(),
        }
    }

    fn new_definition(
        &mut self,
        name: String,
        kind: DefinitionKind,
        location: Location,
        local: bool,
    ) -> DefinitionId {
        let mut mangled_name = self.module_path.clone().unwrap();
        mangled_name.push(name);

        // Mangle local definition names, we don't want to mangle toplevel names since
        // libraries can export toplevel definitions, and definition_ids aren't the same
        // between compiler runs.
        if local {
            mangled_name.push(self.definition_count.to_string());
        }

        let definition_id = DefinitionId(self.definition_count);
        self.definition_count += 1;

        self.compiler_cache.definitions.push(Definition {
            kind,
            location,
            mangled_name,
            local,
        });

        definition_id
    }

    fn assert_definition_kind(
        &self,
        definition_id: DefinitionId,
        kind: DefinitionKind,
    ) -> Result<DefinitionId, Error> {
        if self.compiler_cache[&definition_id].kind != kind {
            panic!("bad definition kind")
        } else {
            Ok(definition_id)
        }
    }

    fn resolve_type(&mut self, ty: &mut Type) -> Result<(), Error> {
        match ty {
            Type::Var(_) => Ok(()),
            Type::UniVar(_) => Ok(()),
            Type::Prim(_) => Ok(()),

            Type::Const {
                name,
                location,
                definition_id,
            } => {
                *definition_id = Some(self.assert_definition_kind(
                    self.environment.lookup_name(name, location)?,
                    DefinitionKind::Type,
                )?);
                Ok(())
            }

            Type::App(base, arg) => {
                self.resolve_type(base)?;
                self.resolve_type(arg)
            }

            Type::Fun(param, ret) => {
                self.resolve_type(param)?;
                self.resolve_type(ret)
            }
        }
    }

    fn resolve_pattern(
        &mut self,
        pattern: &mut Pattern,
        new_defs: &mut Vec<(String, DefinitionId)>,
    ) -> Result<(), Error> {
        match &mut pattern.kind {
            PatternKind::Variant {
                type_name,
                variant_name,
                type_definition_id,
                field_patterns,
            } => {
                let definition_id = self.assert_definition_kind(
                    self.environment.lookup_name(type_name, &pattern.location)?,
                    DefinitionKind::Type,
                )?;

                if !self.type_variants[&definition_id].contains(variant_name) {
                    panic!("nonexistent type variant")
                }

                *type_definition_id = Some(definition_id);

                for field_pattern in field_patterns {
                    self.resolve_pattern(field_pattern, new_defs)?;
                }

                Ok(())
            }

            PatternKind::Var {
                name,
                definition_id,
            } => {
                let new_definition_id = self.new_definition(
                    name.clone(),
                    DefinitionKind::Let,
                    pattern.location.clone(),
                    true,
                );

                *definition_id = Some(new_definition_id.clone());
                new_defs.push((name.clone(), new_definition_id));

                Ok(())
            }

            PatternKind::Lit(_) => Ok(()),
            PatternKind::Wild => Ok(()),
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), Error> {
        match &mut expr.kind {
            ExprKind::Lit(_) => Ok(()),

            ExprKind::Var {
                name,
                definition_id,
            } => {
                *definition_id = Some(self.assert_definition_kind(
                    self.environment.lookup_name(name, &expr.location)?,
                    DefinitionKind::Let,
                )?);
                Ok(())
            }

            ExprKind::Variant {
                type_name,
                variant_name,
                type_definition_id,
            } => {
                let definition_id = self.assert_definition_kind(
                    self.environment.lookup_name(type_name, &expr.location)?,
                    DefinitionKind::Type,
                )?;

                if !self.type_variants[&definition_id].contains(variant_name) {
                    panic!("nonexistent type variant")
                }

                *type_definition_id = Some(definition_id);
                Ok(())
            }

            ExprKind::App { fun, arg } => {
                self.resolve_expr(fun)?;
                self.resolve_expr(arg)
            }

            ExprKind::Extern {
                fun_name: _,
                args,
                prim_type: _,
            } => {
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }

            ExprKind::Lam {
                param_name,
                body,
                param_definition_id,
            } => {
                let new_definition_id = self.new_definition(
                    param_name.clone(),
                    DefinitionKind::Let,
                    expr.location.clone(),
                    true,
                );
                *param_definition_id = Some(new_definition_id.clone());

                self.environment
                    .push_scope(param_name.clone(), new_definition_id);
                self.resolve_expr(body)?;
                self.environment.pop_scope();

                Ok(())
            }

            ExprKind::Let {
                name,
                aexpr,
                body,
                definition_id,
            } => {
                self.resolve_expr(aexpr)?;

                let new_definition_id = self.new_definition(
                    name.clone(),
                    DefinitionKind::Let,
                    expr.location.clone(),
                    true,
                );
                *definition_id = Some(new_definition_id.clone());

                self.environment.push_scope(name.clone(), new_definition_id);
                self.resolve_expr(body)?;
                self.environment.pop_scope();

                Ok(())
            }

            ExprKind::If { cond, texpr, fexpr } => {
                self.resolve_expr(cond)?;
                self.resolve_expr(texpr)?;
                self.resolve_expr(fexpr)
            }

            ExprKind::Match { mexpr, branches } => {
                self.resolve_expr(mexpr)?;

                for branch in branches {
                    let mut new_defs = Vec::new();
                    self.resolve_pattern(&mut branch.0, &mut new_defs)?;
                    self.environment.push_scope_many(new_defs);
                    self.resolve_expr(&mut branch.1)?;
                    self.environment.pop_scope();
                }

                Ok(())
            }
        }
    }

    // Adds all exported definitions from imported modules into self.current_scope
    fn process_module_imports(&mut self, module: &Module) {
        for import in &module.imports {
            self.environment
                .extend(self.module_exports[import.module_id.as_ref().unwrap()].clone())
        }
    }

    // Insert a scope into module_exports which contains all toplevel definitions
    // exported by the module. Assumes all definition ids have been resolved in the
    // module.
    fn setup_module_exports(&mut self, module: &Module) -> Result<(), Error> {
        let mut exports_env = Environment::new();

        for export in &module.exports {
            match export {
                Export::Toplevel {
                    name,
                    location,
                    definition_id,
                } => {
                    exports_env.insert_toplevel(
                        &module.module_path,
                        name.clone(),
                        definition_id.clone().unwrap(),
                        location,
                    )?;
                }
                Export::Module {
                    module_path: _,
                    location: _,
                    module_id,
                } => {
                    exports_env.extend(self.module_exports[module_id.as_ref().unwrap()].clone());
                }
            }
        }

        self.module_exports
            .insert(module.module_id.clone().unwrap(), exports_env);

        Ok(())
    }

    fn resolve_module(&mut self, module: &mut Module) -> Result<(), Error> {
        self.module_path = Some(module.module_path.clone());
        self.environment = Environment::new();

        // Set up definitions for toplevels
        for toplevel in module.toplevels.iter_mut() {
            let definition_kind = match toplevel.kind {
                ToplevelKind::Let { .. } => DefinitionKind::Let,
                ToplevelKind::Type { .. } => DefinitionKind::Type,
            };

            let definition_id = self.new_definition(
                toplevel.name.clone(),
                definition_kind,
                toplevel.location.clone(),
                false,
            );
            toplevel.definition_id = Some(definition_id.clone());

            // Set up type variants if it's a type definition
            if let ToplevelKind::Type {
                type_vars: _,
                variants,
            } = &toplevel.kind
            {
                let mut variant_set = HashSet::new();
                for variant in variants {
                    if variant_set.contains(&variant.name) {
                        panic!("duplicate type variant");
                    } else {
                        variant_set.insert(variant.name.clone());
                    }
                }
                self.type_variants
                    .insert(definition_id.clone(), variant_set);
            }

            self.environment.insert_toplevel(
                &module.module_path,
                toplevel.name.clone(),
                definition_id,
                &toplevel.location,
            )?;
        }

        // Resolve imports and exports, all referenced modules should exist at this point
        let mut imported_modules = HashMap::new();
        for import in module.imports.iter_mut() {
            // import.module_id is already resolved during sorting
            match imported_modules.get(&import.module_path) {
                Some(_) => {
                    // If module is already in imported_modules, it was imported more than once
                    return Err(Error::new(
                        ErrorKind::AlreadyImportedModule(import.module_path.clone()),
                        import.location.clone(),
                    ));
                }
                None => {
                    imported_modules.insert(
                        import.module_path.clone(),
                        import.module_id.clone().unwrap(),
                    );
                }
            }
        }

        let mut exported_modules = HashSet::new();
        let mut exported_definitions = HashSet::new();
        for export in module.exports.iter_mut() {
            match export {
                Export::Toplevel {
                    name,
                    location,
                    definition_id,
                } => {
                    let resolved_id = self.environment.lookup_unqualified_name(name, location)?;
                    *definition_id = Some(resolved_id.clone());

                    if !exported_definitions.insert(resolved_id) {
                        return Err(Error::new(
                            ErrorKind::AlreadyExportedDefinition(name.clone()),
                            location.clone(),
                        ));
                    }
                }
                Export::Module {
                    module_path,
                    location,
                    module_id,
                } => {
                    // If the exported module is not in imported_modules, then it may exist but isn't imported
                    let resolved_id = imported_modules.get(module_path).ok_or(Error::new(
                        ErrorKind::ExportedUnimportedModule(module_path.clone()),
                        location.clone(),
                    ))?;
                    *module_id = Some(resolved_id.clone());

                    if !exported_modules.insert(resolved_id) {
                        return Err(Error::new(
                            ErrorKind::AlreadyExportedModule(module_path.clone()),
                            location.clone(),
                        ));
                    }
                }
            }
        }

        // Resolve toplevels
        self.process_module_imports(module);
        for toplevel in module.toplevels.iter_mut() {
            match &mut toplevel.kind {
                ToplevelKind::Let {
                    type_scheme,
                    expr,
                    is_main: _,
                } => {
                    self.resolve_type(&mut type_scheme.1)?;
                    self.resolve_expr(expr)?;
                }

                ToplevelKind::Type {
                    type_vars: _,
                    variants,
                } => {
                    for variant in variants {
                        for mut field_type in &mut variant.field_types {
                            self.resolve_type(&mut field_type)?;
                        }
                    }
                }
            }
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
