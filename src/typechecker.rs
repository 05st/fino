use std::collections::HashMap;

use ena::unify::InPlaceUnificationTable;

use crate::ast::*;
use crate::cache::CompilerCache;
use crate::error::{Error, ErrorKind};
use crate::types::*;

#[derive(Clone, Debug)]
enum Constraint {
    TypeEqual(Type, Type, NodeId),
}

struct TypeChecker<'a> {
    compiler_cache: &'a mut CompilerCache,
    item_scheme_map: HashMap<DefId, TypeScheme>,
    unification_table: InPlaceUnificationTable<TypeUniVar>,
}

impl<'a> TypeChecker<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> TypeChecker {
        TypeChecker {
            compiler_cache,
            unification_table: InPlaceUnificationTable::new(),
            item_scheme_map: HashMap::new(),
        }
    }

    fn fresh_uni_var(&mut self) -> TypeUniVar {
        self.unification_table.new_key(None)
    }

    fn cache_expr_type(&mut self, node_id: &NodeId, expr_type: &Type) {
        self.compiler_cache
            .expr_type_map
            .insert(node_id.clone(), expr_type.clone());
    }

    // Infer (synthesize) expression type. Only one of infer_expr or check_expr
    // should ever be called on an expression
    fn infer_expr(&mut self, env: im::HashMap<DefId, Type>, expr: &Expr) -> (Vec<Constraint>, Type) {
        let (constraints, expr_type) = match &expr.kind {
            ExprKind::Lit { literal } => {
                (
                    Vec::new(),
                    match literal {
                        Lit::Int(_) => Type::i32(),
                        Lit::Float(_) => Type::f32(),
                        Lit::String(_) => Type::str(),
                        Lit::Char(_) => Type::char(),
                        Lit::Bool(_) => Type::bool(),
                        Lit::Unit => Type::unit(),
                    }
                )
            }

            ExprKind::Var { def_id, name: _ } => {
                // Variable is guaranteed to be defined after name resolution, either in the
                // local scope or as a top-level definition.
                let var_type = match env.get(&def_id) {
                    Some(ty) => ty.clone(),
                    None => {
                        // Instantiate type scheme
                        let scheme = &self.item_scheme_map[&def_id].clone();
                        let subst = scheme.0
                            .iter()
                            .map(|tvar| (Type::Var(tvar.clone()), Type::UniVar(self.fresh_uni_var())))
                            .collect::<HashMap<_, _>>();
                        scheme.1.substitute(&subst)
                    },
                };

                (Vec::new(), var_type)
            }

            ExprKind::Lam { param_def_id, param: _, body } => {
                let param_uvar = self.fresh_uni_var();
                let new_env = env.update(param_def_id.clone(), Type::UniVar(param_uvar));

                let (body_constraints, body_type) = self.infer_expr(new_env, &body);
                (
                    body_constraints,
                    Type::Fun(Box::new(Type::UniVar(param_uvar)), Box::new(body_type)),
                )
            }

            ExprKind::App { fun, arg } => {
                let (arg_constraints, arg_type) = self.infer_expr(env.clone(), &arg);

                let ret_type = Type::UniVar(self.fresh_uni_var());
                let fun_type = Type::Fun(Box::new(arg_type), Box::new(ret_type.clone()));

                let fun_constraints = self.check_expr(env, &fun, fun_type);
                (
                    arg_constraints.into_iter().chain(fun_constraints).collect(),
                    ret_type,
                )
            }

            ExprKind::If { cond, texpr, fexpr } => {
                let cond_constraints = self.check_expr(env.clone(), &cond, Type::bool());

                let (texpr_constraints, branch_type) = self.infer_expr(env.clone(), &texpr);
                let fexpr_constraints = self.check_expr(env, &fexpr, branch_type.clone());

                (
                    cond_constraints
                        .into_iter()
                        .chain(texpr_constraints)
                        .chain(fexpr_constraints)
                        .collect(),
                    branch_type,
                )
            }
        };

        self.cache_expr_type(&expr.node_id, &expr_type);

        (constraints, expr_type)
    }

    // Check expression type against expected type. If it's not possible to deduce
    // type from info available, calls infer_expr and stores type equality
    // constraint.
    // TODO:
    // We can probably pass expected_type as a reference here
    fn check_expr(&mut self, env: im::HashMap<DefId, Type>, expr: &Expr, expected_type: Type) -> Vec<Constraint> {
        self.cache_expr_type(&expr.node_id, &expected_type);

        match (&expr.kind, expected_type) {
            (ExprKind::Lit { literal: Lit::Int(_) }, t) if t == Type::i32() => Vec::new(),
            (ExprKind::Lit { literal: Lit::Float(_) }, t) if t == Type::f32() => Vec::new(),
            (ExprKind::Lit { literal: Lit::String(_) }, t) if t == Type::str() => Vec::new(),
            (ExprKind::Lit { literal: Lit::Char(_) }, t) if t == Type::char() => Vec::new(),
            (ExprKind::Lit { literal: Lit::Bool(_) }, t) if t == Type::bool() => Vec::new(),
            (ExprKind::Lit { literal: Lit::Unit }, t) if t == Type::unit() => Vec::new(),

            (ExprKind::Lam { param_def_id, param: _, body }, Type::Fun(param_type, ret_type)) => {
                let new_env = env.update(param_def_id.clone(), *param_type);
                self.check_expr(new_env, &body, *ret_type)
            }

            (ExprKind::If { cond, texpr, fexpr }, branch_type) => {
                let cond_constraints = self.check_expr(env.clone(), &cond, Type::bool());
                let texpr_constraints = self.check_expr(env.clone(), &texpr, branch_type.clone());
                let fexpr_constraints = self.check_expr(env, &fexpr, branch_type);

                cond_constraints
                    .into_iter()
                    .chain(texpr_constraints)
                    .chain(fexpr_constraints)
                    .collect()
            }

            (_, expected_type) => {
                let (mut constraints, inferred_type) = self.infer_expr(env, expr);
                constraints.push(Constraint::TypeEqual(
                    expected_type,
                    inferred_type,
                    expr.node_id.clone(),
                ));

                constraints
            }
        }
    }

    // Get main representative of type in unification table
    fn normalize_type(&mut self, unnorm: Type) -> Type {
        match unnorm {
            Type::Var(_) | Type::Const(_) => unnorm,

            Type::UniVar(uvar) => match self.unification_table.probe_value(uvar) {
                Some(bound_type) => self.normalize_type(bound_type),
                None => unnorm,
            }

            Type::Fun(unnorm_param_type, unnorm_ret_type) => {
                let param_type = self.normalize_type(*unnorm_param_type);
                let ret_type = self.normalize_type(*unnorm_ret_type);
                Type::Fun(Box::new(param_type), Box::new(ret_type))
            }
        }
    }

    fn unify(&mut self, unnorm_left: Type, unnorm_right: Type, node_id: &NodeId) -> Result<(), Error> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Var(var_a), Type::Var(var_b)) => {
                (var_a == var_b).then_some(()).ok_or_else(|| todo!())
            }
            
            (Type::Const(name_a), Type::Const(name_b)) if name_a == name_b => Ok(()),

            (Type::Fun(param_a, ret_a), Type::Fun(param_b, ret_b)) => {
                self.unify(*param_a, *param_b, node_id)?;
                self.unify(*ret_a, *ret_b, node_id)
            }

            (Type::UniVar(uvar_a), Type::UniVar(uvar_b)) => self
                .unification_table
                .unify_var_var(uvar_a, uvar_b)
                .map_err(|(l, r)| self.compiler_cache.make_error(ErrorKind::TypeMismatch(l, r), node_id)),

            (Type::UniVar(uvar), ty) | (ty, Type::UniVar(uvar)) => {
                ty
                    .occurs_check(uvar)
                    .map_err(|t| self.compiler_cache.make_error(ErrorKind::InfiniteType(t), node_id))?;

                self.unification_table
                    .unify_var_value(uvar, Some(ty))
                    .map_err(|(l, r)| self.compiler_cache.make_error(ErrorKind::TypeMismatch(l, r), node_id))
            }

            (left, right) => Err(self.compiler_cache.make_error(ErrorKind::TypeMismatch(left, right), node_id)),
        }
    }

    fn solve_constraints(&mut self, constraints: Vec<Constraint>) -> Result<(), Error> {
        for constraint in constraints {
            match constraint {
                Constraint::TypeEqual(left, right, node_id) => self.unify(left, right, &node_id)?,
            }
        }
        Ok(())
    }
}

pub fn typecheck_program(compiler_cache: &mut CompilerCache, program: &Vec<Module>) -> Result<(), Error> {
    let mut typechecker = TypeChecker::new(compiler_cache);

    for module in program {
        // Insert all top-level definitions into item_scheme_map
        for item in &module.items {
            typechecker.item_scheme_map.insert(item.def_id.clone(), item.scheme.clone());
        }

        // Typecheck top-level definitions
        for item in &module.items {
            // MAYBE BUG:
            // Not exactly sure what reset_unifications does. Maybe we should just set
            // unification_table to InPlaceUnificationTable::new() instead?
            typechecker.unification_table.reset_unifications(|_| None);

            let constraints = typechecker.check_expr(im::HashMap::new(), &item.expr, item.scheme.1.clone());
            typechecker.solve_constraints(constraints)?;

            // Normalize expr_type_map after constraints have been solved
            // TODO:
            // Find a better way to do this
            let expr_type_map = typechecker.compiler_cache.expr_type_map.clone();
            typechecker.compiler_cache.expr_type_map = expr_type_map
                .into_iter()
                .map(|(k, v)| (k, typechecker.normalize_type(v)))
                .collect::<HashMap<_, _>>();
        }
    }

    Ok(())
}
