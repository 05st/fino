use std::{collections::HashMap, iter::once};

use crate::{
    ast::*,
    cache::CompilerCache,
    error::{Error, ErrorKind},
};

struct NameResolver<'a> {
    compiler_cache: &'a mut CompilerCache,
    // Only top-level functions definitions are stored in qualified_name_map
    qualified_name_map: HashMap<Vec<String>, DefId>,
    def_id_count: usize,
    current_module_name: Option<Vec<String>>,
    current_module_imports: Option<Vec<Import>>,
}

impl<'a> NameResolver<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> NameResolver<'a> {
        NameResolver {
            compiler_cache,
            qualified_name_map: HashMap::new(),
            def_id_count: 0,
            current_module_name: None,
            current_module_imports: None,
        }
    }

    fn insert_qualified_name(&mut self, name: Vec<String>, id: DefId, node_id: &NodeId) -> Result<(), Error> {
        if self.qualified_name_map.contains_key(&name) {
            return Err(self.compiler_cache.make_error(ErrorKind::Redefinition(name), node_id));
        }
        self.qualified_name_map.insert(name, id);
        Ok(())
    }

    fn lookup_name(&self, name: &Name, scope: im::HashMap<String, DefId>, node_id: &NodeId) -> Result<DefId, Error> {
        match name {
            Name::Qualified(qual) => {
                // Qualified name refers to top-level definition, just check qualified_name_map
                match self.qualified_name_map.get(qual) {
                    Some(def_id) => Ok(def_id.clone()),
                    None => Err(self.compiler_cache.make_error(ErrorKind::UnknownVariable(name.clone()), node_id)),
                }
            }
            Name::Unqualified(ident) => {
                // Check local scope first
                match scope.get(ident) {
                    Some(def_id) => return Ok(def_id.clone()),
                    None => (),
                };

                // Otherwise check top-level definitions of current module and imported modules
                let module_names = self
                    .current_module_imports
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|import| &import.module_name)
                    .chain(once(self.current_module_name.as_ref().unwrap()));

                for module_name in module_names {
                    let mut qual = module_name.clone();
                    qual.push(ident.clone());

                    match self.qualified_name_map.get(&qual) {
                        Some(def_id) => return Ok(def_id.clone()),
                        None => (),
                    }
                }

                Err(self.compiler_cache.make_error(ErrorKind::UnknownVariable(name.clone()), node_id))
            }
        }
    }

    // DefId(0) is currently the default value produced by the parser, so after name
    // resolution every DefId(n) should have n >= 1.
    fn new_def_id(&mut self) -> DefId {
        self.def_id_count += 1;
        DefId(self.def_id_count)
    }

    fn resolve_expr(&mut self, expr: &mut Expr, scope: im::HashMap<String, DefId>) -> Result<(), Error> {
        match &mut expr.kind {
            ExprKind::Lit { literal: _ } => Ok(()),
            ExprKind::Var { ref mut def_id, name } => {
                *def_id = self.lookup_name(&name, scope, &expr.node_id)?;
                Ok(())
            }
            ExprKind::App { ref mut fun, ref mut arg } => {
                self.resolve_expr(fun, scope.clone())?;
                self.resolve_expr(arg, scope)
            }
            ExprKind::Lam { ref mut param_def_id, param, body } => {
                *param_def_id = self.new_def_id();
                let new_scope = scope.update(param.clone(), param_def_id.clone());
                self.resolve_expr(body, new_scope)
            }
            ExprKind::Let { ref mut def_id, name, expr, body } => {
                // Note we don't put def_id into the scope yet, so recursive let-expressions
                // aren't allowed (for now).
                self.resolve_expr(expr, scope.clone())?;

                *def_id = self.new_def_id();
                let new_scope = scope.update(name.clone(), def_id.clone());

                self.resolve_expr(body, new_scope)
            }
            ExprKind::If { ref mut cond, ref mut texpr, ref mut fexpr } => {
                self.resolve_expr(cond, scope.clone())?;
                self.resolve_expr(texpr, scope.clone())?;
                self.resolve_expr(fexpr, scope)
            }
        }
    }

    fn resolve_module(&mut self, module: &'a mut Module) -> Result<(), Error> {
        self.current_module_name = Some(module.module_name.clone());
        self.current_module_imports = Some(module.imports.clone());

        // Insert top-level definitions to qualifed_name_map
        for item in &mut module.items {
            let mut qualified_name = module.module_name.clone();
            qualified_name.push(item.name.clone());

            let def_id = self.new_def_id();
            self.insert_qualified_name(qualified_name, def_id.clone(), &item.node_id)?;
            item.def_id = def_id;
        }
        
        // Resolve items, note each resolve_item() call starts with an empty scope.
        for item in module.items.iter_mut() {
            self.resolve_expr(&mut item.expr, im::HashMap::new())?;
        }

        Ok(())
    }
}

pub fn resolve_program(compiler_cache: &mut CompilerCache, program: &mut Vec<Module>) -> Result<(), Error> {
    let mut resolver = NameResolver::new(compiler_cache);
    for module in program {
        resolver.resolve_module(module)?;
    }
    Ok(())
}
