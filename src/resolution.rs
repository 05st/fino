use std::collections::HashMap;

use crate::{
    ast::{DefId, Expr, Item, Module, Name, NodeId}, cache::CompilerCache, error::{Error, ErrorKind}
};

pub struct NameResolution<'a> {
    compiler_cache: &'a mut CompilerCache,
    qualified_name_map: HashMap<Vec<String>, DefId>,
    def_id_count: usize,
}

impl<'a> NameResolution<'a> {
    pub fn new(compiler_cache: &'a mut CompilerCache) -> NameResolution<'a> {
        NameResolution {
            compiler_cache,
            qualified_name_map: HashMap::new(),
            def_id_count: 0,
        }
    }

    fn insert_qualified_name(&mut self, name: Vec<String>, id: DefId) -> Result<(), Error> {
        match self.qualified_name_map.insert( name, id) {
            None => Ok(()),
            Some(_) => panic!("duplicate definition"), // TODO
        }
    }

    fn lookup_qualified_name(&self, name: &Vec<String>) -> Result<DefId, Error> {
        todo!()
    }

    fn lookup_name(&self, name: &String, scope: im::HashMap<String, DefId>, node_id: &NodeId) -> Result<DefId, Error> {
        match scope.get(name) {
            Some(def_id) => Ok(def_id.clone()),
            None => Err(self.compiler_cache.make_error(ErrorKind::UnknownVariable(Name::Unqualified(name.clone())), node_id)),
        }
    }

    // DefId(0) is currently the default value produced by the parser, so after name
    // resolution every DefId(n) should have n >= 1.
    fn new_def_id(&mut self) -> DefId {
        self.def_id_count += 1;
        DefId(self.def_id_count)
    }

    fn resolve_expr(&mut self, expr: &mut Expr, scope: im::HashMap<String, DefId>) -> Result<(), Error> {
        match expr {
            Expr::Lit { node_id, literal } => Ok(()),
            Expr::Var { node_id, ref mut def_id, name } => {
                *def_id = match name {
                    Name::Qualified(qual) => self.lookup_qualified_name(qual)?,
                    Name::Unqualified(ident) => self.lookup_name(ident, scope, node_id)?,
                };

                Ok(())
            }
            Expr::App { node_id, ref mut fun, ref mut arg } => {
                self.resolve_expr(fun, scope.clone())?;
                self.resolve_expr(arg, scope)
            }
            Expr::Lam { node_id, param_def_id, param, body } => {
                let new_scope = scope.update(param.clone(), self.new_def_id());
                self.resolve_expr(body, new_scope)
            }
            Expr::If { node_id, ref mut cond, ref mut texpr, ref mut fexpr } => {
                self.resolve_expr(cond, scope.clone())?;
                self.resolve_expr(texpr, scope.clone())?;
                self.resolve_expr(fexpr, scope)
            }
        }
    }

    fn resolve_item(&mut self, item: &mut Item, scope: im::HashMap<String, DefId>) -> Result<(), Error> {
        self.resolve_expr(&mut item.expr, scope)
    }

    fn resolve_module(&mut self, module: &mut Module) -> Result<(), Error> {
        // Insert top-level definitions to qualifed_name_map
        for item in &module.items {
            let mut qualified_name = module.module_name.clone();
            qualified_name.push(item.name.clone());

            println!("{:?}", qualified_name);

            let def_id = self.new_def_id();
            self.insert_qualified_name(qualified_name, def_id)?;
        }

        // Resolve items, note each resolve_item() call starts witha an empty scope
        for mut item in module.items.iter_mut() {
            self.resolve_item(&mut item, im::HashMap::new())?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, modules: &mut Vec<Module>) -> Result<(), Error> {
        for module in modules {
            self.resolve_module(module)?;
        }
        Ok(())
    }
}
