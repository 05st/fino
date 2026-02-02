use std::{
    collections::{HashMap, HashSet},
    iter::repeat_with,
};

use crate::{
    ast,
    cache::{CompilerCache, DefinitionId},
    mir,
};

struct Transformer<'a> {
    compiler_cache: &'a mut CompilerCache,
    globals: Vec<mir::Toplevel>,
    functions: HashSet<DefinitionId>,
    type_variant_tags: HashMap<DefinitionId, HashMap<String, i64>>,
    misc_name_count: usize,
}

impl<'a> Transformer<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> Transformer {
        Transformer {
            compiler_cache,
            globals: Vec::new(),
            functions: HashSet::new(),
            type_variant_tags: HashMap::new(),
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
    fn collect_free_vars(
        &self,
        expr: &ast::Expr,
        scope: im::HashSet<DefinitionId>,
        free_vars: &mut Vec<String>,
    ) {
        match &expr.kind {
            ast::ExprKind::Lit(_) => (),

            ast::ExprKind::Var {
                name: _,
                definition_id,
            } => {
                let definition_id = definition_id.as_ref().unwrap();
                // We don't want to treat references to let-definitions as free variables
                if self.compiler_cache[definition_id].local && !scope.contains(definition_id) {
                    free_vars.push(self.get_mangled_name(definition_id));
                }
            }

            // Type variant shouldn't refer to any free variables
            ast::ExprKind::Variant { .. } => (),

            ast::ExprKind::App { fun, arg } => {
                self.collect_free_vars(fun, scope.clone(), free_vars);
                self.collect_free_vars(arg, scope, free_vars);
            }

            ast::ExprKind::Extern {
                fun_name: _,
                args,
                prim_type: _,
            } => {
                for arg in args {
                    self.collect_free_vars(arg, scope.clone(), free_vars);
                }
            }

            ast::ExprKind::Lam {
                param_name: _,
                body,
                param_definition_id,
            } => {
                let updated_scope = scope.update(param_definition_id.clone().unwrap());
                self.collect_free_vars(body, updated_scope, free_vars);
            }

            ast::ExprKind::Let {
                name: _,
                aexpr,
                body,
                definition_id,
            } => {
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

            ast::ExprKind::Match { mexpr, branches } => {
                self.collect_free_vars(mexpr, scope.clone(), free_vars);

                for (pattern, branch_expr) in branches {
                    let mut branch_scope = scope.clone();
                    self.collect_pattern_scope(pattern, &mut branch_scope);
                    self.collect_free_vars(branch_expr, branch_scope, free_vars);
                }
            }
        }
    }

    fn collect_pattern_scope(
        &self,
        pattern: &ast::Pattern,
        scope: &mut im::HashSet<DefinitionId>,
    ) {
        match &pattern.kind {
            ast::PatternKind::Variant {
                type_name: _,
                variant_name: _,
                type_definition_id: _,
                field_patterns,
            } => {
                for field_pattern in field_patterns {
                    self.collect_pattern_scope(field_pattern, scope);
                }
            }

            ast::PatternKind::Var {
                name: _,
                definition_id,
            } => {
                if let Some(def_id) = definition_id {
                    *scope = scope.update(def_id.clone());
                }
            }

            ast::PatternKind::Lit(_) => (),
            ast::PatternKind::Wild => (),
        }
    }

    fn make_match_fail_expr(&self) -> mir::Expr {
        mir::Expr::Intrinsic {
            kind: mir::Intrinsic::MatchFail,
            args: Vec::new(),
            ints: Vec::new(),
        }
    }

    fn make_eq_expr(&self, lhs: mir::Expr, literal: &crate::literal::Literal) -> mir::Expr {
        let kind = match literal {
            crate::literal::Literal::Int(_) => mir::Intrinsic::EqInt,
            crate::literal::Literal::Float(_) => mir::Intrinsic::EqFloat,
            crate::literal::Literal::Char(_) => mir::Intrinsic::EqChar,
            crate::literal::Literal::Bool(_) => mir::Intrinsic::EqBool,
            crate::literal::Literal::String(_) => mir::Intrinsic::EqString,
            crate::literal::Literal::Unit => mir::Intrinsic::PtrEq,
        };

        mir::Expr::Intrinsic {
            kind,
            args: vec![lhs, mir::Expr::Lit(literal.clone())],
            ints: Vec::new(),
        }
    }

    fn make_tag_expr(&self, scrutinee: mir::Expr, num_fields: i64) -> mir::Expr {
        mir::Expr::Intrinsic {
            kind: mir::Intrinsic::VariantTag,
            args: vec![scrutinee],
            ints: vec![num_fields],
        }
    }

    fn make_field_expr(&self, scrutinee: mir::Expr, index: i64, num_fields: i64) -> mir::Expr {
        mir::Expr::Intrinsic {
            kind: mir::Intrinsic::VariantField,
            args: vec![scrutinee],
            ints: vec![index, num_fields],
        }
    }

    fn build_pattern_comp_expr(
        &mut self,
        scrutinee: mir::Expr,
        pattern: &ast::Pattern,
        success: mir::Expr,
        fail: mir::Expr,
    ) -> mir::Expr {
        match &pattern.kind {
            ast::PatternKind::Wild => success,

            ast::PatternKind::Var {
                name: _,
                definition_id,
            } => mir::Expr::Let {
                name: self.get_mangled_name(definition_id.as_ref().unwrap()),
                aexpr: Box::new(scrutinee),
                body: Box::new(success),
            },

            ast::PatternKind::Lit(literal) => mir::Expr::If {
                cond: Box::new(self.make_eq_expr(scrutinee, literal)),
                texpr: Box::new(success),
                fexpr: Box::new(fail),
            },

            ast::PatternKind::Variant {
                type_name: _,
                variant_name,
                type_definition_id,
                field_patterns,
            } => {
                let type_definition_id = type_definition_id.as_ref().unwrap();
                let tag = self.type_variant_tags[type_definition_id][variant_name];

                let num_fields = field_patterns.len() as i64;
                let tag_expr = self.make_tag_expr(scrutinee.clone(), num_fields);
                let tag_cond =
                    self.make_eq_expr(tag_expr, &crate::literal::Literal::Int(tag));

                let mut inner = success;
                for (i, field_pattern) in field_patterns.iter().enumerate().rev() {
                    let field_name = self.new_misc_name("field");
                    let field_expr =
                        self.make_field_expr(scrutinee.clone(), i as i64, num_fields);
                    let field_body = self.build_pattern_comp_expr(
                        mir::Expr::Var(field_name.clone()),
                        field_pattern,
                        inner,
                        fail.clone(),
                    );
                    inner = mir::Expr::Let {
                        name: field_name,
                        aexpr: Box::new(field_expr),
                        body: Box::new(field_body),
                    };
                }

                mir::Expr::If {
                    cond: Box::new(tag_cond),
                    texpr: Box::new(inner),
                    fexpr: Box::new(fail),
                }
            }
        }
    }

    fn transform_expr(&mut self, expr: &ast::Expr) -> mir::Expr {
        match &expr.kind {
            ast::ExprKind::Lit(literal) => mir::Expr::Lit(literal.clone()),

            ast::ExprKind::Var {
                name: _,
                definition_id,
            } => {
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

            ast::ExprKind::Variant {
                type_name: _,
                variant_name,
                type_definition_id,
            } => {
                let type_definition_id = type_definition_id.as_ref().unwrap();
                let variant_mangled_name =
                    self.get_mangled_name(type_definition_id) + "_" + variant_name;

                mir::Expr::Closure {
                    fun_name: variant_mangled_name,
                    env: Vec::new(),
                }
            }

            ast::ExprKind::Extern {
                fun_name,
                args,
                prim_type: _,
            } => mir::Expr::Extern {
                fun_name: fun_name.clone(),
                args: args
                    .into_iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect(),
            },

            ast::ExprKind::App { fun, arg } => {
                mir::Expr::App {
                    // fun should evaluate to a closure
                    closure: Box::new(self.transform_expr(fun)),
                    arg: Box::new(self.transform_expr(arg)),
                }
            }

            ast::ExprKind::Lam {
                param_name: _,
                body,
                param_definition_id,
            } => {
                let mut free_vars = Vec::new();
                self.collect_free_vars(expr, im::HashSet::new(), &mut free_vars);

                let fun_name = self.new_misc_name("lambda");

                let transformed_body = self.transform_expr(body);

                self.globals.push(mir::Toplevel::Function {
                    name: fun_name.clone(),
                    env: free_vars.clone(),
                    param: self.get_mangled_name(param_definition_id.as_ref().unwrap()),
                    body: transformed_body,
                });

                mir::Expr::Closure {
                    fun_name,
                    env: free_vars,
                }
            }

            ast::ExprKind::Let {
                name: _,
                aexpr,
                body,
                definition_id,
            } => mir::Expr::Let {
                name: self.get_mangled_name(definition_id.as_ref().unwrap()),
                aexpr: Box::new(self.transform_expr(aexpr)),
                body: Box::new(self.transform_expr(body)),
            },

            ast::ExprKind::If { cond, texpr, fexpr } => mir::Expr::If {
                cond: Box::new(self.transform_expr(cond)),
                texpr: Box::new(self.transform_expr(texpr)),
                fexpr: Box::new(self.transform_expr(fexpr)),
            },

            ast::ExprKind::Match { mexpr, branches } => {
                let scrutinee_name = self.new_misc_name("match");
                let scrutinee_expr = self.transform_expr(mexpr);
                let scrutinee_var = mir::Expr::Var(scrutinee_name.clone());

                let mut lowered = self.make_match_fail_expr();
                for (pattern, branch_expr) in branches.iter().rev() {
                    let branch_mir = self.transform_expr(branch_expr);
                    lowered = self.build_pattern_comp_expr(
                        scrutinee_var.clone(),
                        pattern,
                        branch_mir,
                        lowered,
                    );
                }

                mir::Expr::Let {
                    name: scrutinee_name,
                    aexpr: Box::new(scrutinee_expr),
                    body: Box::new(lowered),
                }
            }
        }
    }

    fn transform_module(&mut self, module: &ast::Module) -> () {
        // Declare all toplevels first
        for toplevel in &module.toplevels {
            match &toplevel.kind {
                ast::ToplevelKind::Let {
                    type_scheme: _,
                    expr,
                    is_main: _,
                } => {
                    // If the top expression of a let-definition is a lambda, create a MIR function instead
                    // Note all top expressions in let-definitions should have zero free variables.
                    if let ast::ExprKind::Lam { .. } = &expr.kind {
                        self.functions
                            .insert(toplevel.definition_id.clone().unwrap());
                    }
                }

                ast::ToplevelKind::Type { variants, .. } => {
                    let type_definition_id = toplevel.definition_id.as_ref().unwrap();
                    let mut variant_tags = HashMap::new();

                    for (i, variant) in variants.iter().enumerate() {
                        variant_tags.insert(variant.name.clone(), i as i64);
                    }

                    self.type_variant_tags
                        .insert(type_definition_id.clone(), variant_tags);
                }
            }
        }

        // Transform toplevels
        for toplevel in &module.toplevels {
            let name = self.get_mangled_name(toplevel.definition_id.as_ref().unwrap());

            match &toplevel.kind {
                ast::ToplevelKind::Let {
                    type_scheme: _,
                    expr,
                    is_main,
                } => {
                    let global = match &expr.kind {
                        ast::ExprKind::Lam {
                            param_name: _,
                            body,
                            param_definition_id,
                        } => {
                            let transformed_body = self.transform_expr(body);

                            mir::Toplevel::Function {
                                name,
                                env: Vec::new(),
                                param: self.get_mangled_name(param_definition_id.as_ref().unwrap()),
                                body: transformed_body,
                            }
                        }

                        _ => {
                            let transformed_body = self.transform_expr(expr);
                            mir::Toplevel::Variable {
                                name,
                                body: transformed_body,
                                is_main: *is_main,
                            }
                        }
                    };

                    self.globals.push(global);
                }

                ast::ToplevelKind::Type {
                    type_vars: _,
                    variants,
                } => {
                    let type_definition_id = toplevel.definition_id.as_ref().unwrap();

                    for (i, variant) in variants.iter().enumerate() {
                        let variant_mangled_name =
                            self.get_mangled_name(type_definition_id) + "_" + &variant.name;

                        let global = if variant.field_types.is_empty() {
                            mir::Toplevel::Variable {
                                name: variant_mangled_name,
                                body: mir::Expr::TypeInst {
                                    tag: i as i64,
                                    params: Vec::new(),
                                },
                                is_main: false,
                            }
                        } else {
                            let num_fields = variant.field_types.len();
                            let param_names = repeat_with(|| self.new_misc_name("param"))
                                .take(num_fields)
                                .collect::<Vec<_>>();

                            let body = param_names
                                .iter()
                                .rev()
                                .take(num_fields - 1)
                                .enumerate()
                                .fold(
                                    mir::Expr::TypeInst {
                                        tag: i as i64,
                                        params: param_names.clone(),
                                    },
                                    |child, (pi, param_name)| {
                                        let fun_name = self.new_misc_name("variant");
                                        let env = param_names
                                            .iter()
                                            .take(num_fields - pi - 1)
                                            .cloned()
                                            .collect::<Vec<_>>();
                                        self.globals.push(mir::Toplevel::Function {
                                            name: fun_name.clone(),
                                            env: env.clone(),
                                            param: param_name.clone(),
                                            body: child,
                                        });
                                        mir::Expr::Closure { fun_name, env }
                                    },
                                );

                            mir::Toplevel::Function {
                                name: variant_mangled_name,
                                env: Vec::new(),
                                param: param_names[0].clone(),
                                body,
                            }
                        };

                        self.globals.push(global);
                    }
                }
            }
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
