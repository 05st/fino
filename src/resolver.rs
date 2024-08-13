use std::collections::HashMap;

use crate::{
    ast::*,
    error::{Error, ErrorKind},
};

struct NameResolver {
    // Item definitions are only stored in item_map, not in the local scope
    item_map: HashMap<Vec<String>, DefId>,
    def_id_count: usize,
}

impl NameResolver {
    fn new() -> NameResolver {
        NameResolver {
            item_map: HashMap::new(),
            def_id_count: 0,
        }
    }

    fn insert_item(&mut self, name: Vec<String>, id: DefId, location: &Location) -> Result<(), Error> {
        if self.item_map.contains_key(&name) {
            return Err(Error::new(ErrorKind::Redefinition(name), location.clone()));
        }
        self.item_map.insert(name, id);
        Ok(())
    }

    fn lookup_name(&self, name: &Name, scope: im::HashMap<String, DefId>, location: &Location) -> Result<DefId, Error> {
        match name {
            Name::Qualified(qual) => {
                // Qualified names can only refer to top-level definitions
                match self.item_map.get(qual) {
                    Some(def_id) => Ok(def_id.clone()),
                    None => Err(Error::new(ErrorKind::UnknownVariable(name.clone()), location.clone())),
                }
            }
            Name::Unqualified(ident) => {
                // Check local scope first
                match scope.get(ident) {
                    Some(def_id) => return Ok(def_id.clone()),
                    None => (),
                };

                // TODO
                // Check imported modules

                Err(Error::new(ErrorKind::UnknownVariable(name.clone()), location.clone()))
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
            ExprKind::Lit(_) => Ok(()),
            ExprKind::Var { ref mut def_id, name } => {
                *def_id = self.lookup_name(&name, scope, &expr.location)?;
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

    fn resolve_module(&mut self, module: &mut Module) -> Result<(), Error> {
        let get_qualified_name = |name| {
            let mut qualified_name = module.module_name.clone();
            qualified_name.push(name);
            qualified_name
        };

        // Insert item definitions to qualified_name_map
        for ext in &mut module.externs {
            ext.def_id = self.new_def_id();
            self.insert_item(
                get_qualified_name(ext.name.clone()),
                ext.def_id.clone(),
                &ext.location,
            )?;
        }
        for item in &mut module.items {
            item.def_id = self.new_def_id();
            self.insert_item(
                get_qualified_name(item.name.clone()),
                item.def_id.clone(),
                &item.location,
            )?;
        }

        // Resolve items, note each resolve_item() call starts with an empty local scope
        for item in module.items.iter_mut() {
            self.resolve_expr(&mut item.expr, im::HashMap::new())?;
        }

        Ok(())
    }
}

pub fn resolve_program(program: &mut Vec<Module>) -> Result<(), Error> {
    let mut resolver = NameResolver::new();
    for module in program {
        resolver.resolve_module(module)?;
    }
    Ok(())
}
