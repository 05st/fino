use std::collections::HashSet;

use crate::{ast, cache::{CompilerCache, DefinitionId}, mir};

struct Transformer<'a> {
    compiler_cache: &'a mut CompilerCache,
    globals: Vec<mir::Toplevel>,
    functions: HashSet<DefinitionId>,
    misc_name_count: usize,
}

impl<'a> Transformer<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> Transformer {
        Transformer {
            compiler_cache,
            globals: Vec::new(),
            functions: HashSet::new(),
            misc_name_count: 0,
        }
    }

    fn get_mangled_name(&self, definition_id: &DefinitionId) -> String {
        self.compiler_cache[definition_id].mangled_name.join("_")
    }

    fn new_misc_name(&mut self, prefix: &str) -> String {
        let new_name = format!("_{}_{}", prefix, self.misc_name_count);
        self.misc_name_count += 1;
        new_name
    }

    // Collects all the free variables in an expression
    fn collect_free_vars(&self, expr: &ast::Expr, scope: im::HashSet<DefinitionId>, free_vars: &mut Vec<String>) {
        match &expr.kind {
            ast::ExprKind::Lit(_) => (),

            ast::ExprKind::Var { name: _, definition_id } => {
                let definition_id = definition_id.as_ref().unwrap();
                // We don't want to treat references to top-level items as free variables
                if self.compiler_cache[definition_id].local && !scope.contains(definition_id) {
                    free_vars.push(self.get_mangled_name(definition_id));
                }
            }

            ast::ExprKind::App { fun, arg } => {
                self.collect_free_vars(fun, scope.clone(), free_vars);
                self.collect_free_vars(arg, scope, free_vars);
            }

            ast::ExprKind::Extern { fun_name: _, args, prim_type: _ } => {
                for arg in args {
                    self.collect_free_vars(arg, scope.clone(), free_vars);
                }
            }

            ast::ExprKind::Lam { param_name: _, body, param_definition_id } => {
                let updated_scope = scope.update(param_definition_id.clone().unwrap());
                self.collect_free_vars(body, updated_scope, free_vars);
            }

            ast::ExprKind::Let { name: _, aexpr, body, definition_id } => {
                let updated_scope = scope.update(definition_id.clone().unwrap());
                // Notice we don't use updated_scope for aexpr
                self.collect_free_vars(aexpr, scope, free_vars);
                self.collect_free_vars(body, updated_scope, free_vars);
            }

            ast::ExprKind::If { cond, texpr, fexpr } => {
                self.collect_free_vars(cond, scope.clone(), free_vars);
                self.collect_free_vars(texpr, scope.clone(), free_vars);
                self.collect_free_vars(fexpr, scope, free_vars);
            }
        }
    }

    fn transform_expr(&mut self, expr: &ast::Expr) -> mir::Expr {
        match &expr.kind {
            ast::ExprKind::Lit(literal) => mir::Expr::Lit(literal.clone()),

            ast::ExprKind::Var { name: _, definition_id } => {
                let definition_id = definition_id.as_ref().unwrap();
                let mangled_name = self.get_mangled_name(definition_id);

                if self.functions.contains(definition_id) {
                    mir::Expr::Closure {
                        fun_name: mangled_name,
                        env: Vec::new(),
                    }
                } else {
                    mir::Expr::Var(mangled_name)
                }
            }

            ast::ExprKind::Extern { fun_name, args, prim_type: _ } => {
                mir::Expr::Extern {
                    fun_name: fun_name.clone(),
                    args: args.into_iter().map(|arg| self.transform_expr(arg)).collect(),
                }
            }

            ast::ExprKind::App { fun, arg } => {
                mir::Expr::App {
                    // fun should evaluate to a closure
                    closure: Box::new(self.transform_expr(fun)),
                    arg: Box::new(self.transform_expr(arg)),
                }
            },

            ast::ExprKind::Lam { param_name: _, body, param_definition_id } => {
                let mut free_vars = Vec::new();
                self.collect_free_vars(expr, im::HashSet::new(), &mut free_vars);

                let fun_name = self.new_misc_name("lambda");

                let transformed_body = self.transform_expr(body);

                self.globals.push(mir::Toplevel::Function {
                    name: fun_name.clone(),
                    env: free_vars.clone(),
                    param: self.get_mangled_name(param_definition_id.as_ref().unwrap()),
                    body: transformed_body,
                    is_main: false,
                });

                mir::Expr::Closure {
                    fun_name,
                    env: free_vars,
                }
            }

            ast::ExprKind::Let { name: _, aexpr, body, definition_id } => {
                mir::Expr::Let {
                    name: self.get_mangled_name(definition_id.as_ref().unwrap()),
                    aexpr: Box::new(self.transform_expr(aexpr)),
                    body: Box::new(self.transform_expr(body)),
                }
            }

            ast::ExprKind::If { cond, texpr, fexpr } => {
                mir::Expr::If {
                    cond: Box::new(self.transform_expr(cond)),
                    texpr: Box::new(self.transform_expr(texpr)),
                    fexpr: Box::new(self.transform_expr(fexpr)),
                }
            }
        }
    }

    fn transform_module(&mut self, module: &ast::Module) -> () {
        // If the top expression of an item is a lambda, create a MIR function instead
        // Note all top expressions in items should have zero free variables.

        // Declare all functions first
        for item in &module.items {
            if let ast::ExprKind::Lam { .. } = &item.expr.kind {
                self.functions.insert(item.definition_id.clone().unwrap());
            }
        }

        // Transform items
        for item in &module.items {
            let name = self.get_mangled_name(item.definition_id.as_ref().unwrap());

            let global = match &item.expr.kind {
                ast::ExprKind::Lam { param_name: _, body, param_definition_id } => {
                    let transformed_body = self.transform_expr(body);

                    mir::Toplevel::Function {
                        name,
                        env: Vec::new(),
                        param: self.get_mangled_name(param_definition_id.as_ref().unwrap()),
                        body: transformed_body,
                        is_main: item.is_main,
                    }
                },

                _ => {
                    let transformed_body = self.transform_expr(&item.expr);
                    mir::Toplevel::Variable {
                        name,
                        body: transformed_body,
                    }
                }
            };

            self.globals.push(global);
        }
    }
}

pub fn transform_program(compiler_cache: &mut CompilerCache) -> Vec<mir::Toplevel> {
    let mut transformer = Transformer::new(compiler_cache);

    let mut queue = Vec::new();
    while let Some(module) = transformer.compiler_cache.modules.pop_front() {
        transformer.transform_module(&module);
        queue.push(module);
    }

    while let Some(module) = queue.pop() {
        transformer.compiler_cache.modules.push_front(module);
    }

    transformer.globals
}
