use std::collections::HashMap;

use ena::unify::InPlaceUnificationTable;

use crate::ast::*;
use crate::types::*;

type TypeEnv = im::HashMap<DefId, Type>;

#[derive(Clone, Debug)]
enum Constraint {
    TypeEqual(Type, Type),
}

struct TypeInference {
    unification_table: InPlaceUnificationTable<TypeUniVar>,
    expr_type_map: HashMap<NodeId, Type>,
}

#[derive(Debug)]
enum TypeError {
    Mismatch(Type, Type),
    Infinite(Type),
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            expr_type_map: HashMap::new(),
        }
    }

    fn fresh_uni_var(&mut self) -> TypeUniVar {
        self.unification_table.new_key(None)
    }

    fn infer_expr(&mut self, env: TypeEnv, expr: Expr) -> (Vec<Constraint>, Type) {
        match expr {
            Expr::Lit { node_id, literal } => {
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

            Expr::Var { node_id, def_id, name } => {
                // Variable is guaranteed to be defined after name resolution
                let var_type = &env[&def_id];

                (
                    Vec::new(),
                    var_type.clone(),
                )
            }

            Expr::Lam { node_id, param_def_id, param, body } => {
                let param_uvar = self.fresh_uni_var();
                let new_env = env.update(param_def_id, Type::UniVar(param_uvar));

                let (body_constraints, body_type) = self.infer_expr(new_env, *body);
                (
                    body_constraints,
                    Type::Fun(Box::new(Type::UniVar(param_uvar)), Box::new(body_type)),
                )
            }

            Expr::App { node_id, fun, arg } => {
                let (arg_constraints, arg_type) = self.infer_expr(env.clone(), *arg);

                let ret_type = Type::UniVar(self.fresh_uni_var());
                let fun_type = Type::Fun(Box::new(arg_type), Box::new(ret_type.clone()));

                let fun_constraints = self.check_expr(env, *fun, fun_type);
                (
                    arg_constraints
                        .into_iter()
                        .chain(fun_constraints)
                        .collect(),
                    ret_type,
                )
            }

            Expr::If { node_id, cond, texpr, fexpr } => {
                let cond_constraints = self.check_expr(env.clone(), *cond, Type::bool());

                let (texpr_constraints, branch_type) = self.infer_expr(env.clone(), *texpr);
                let fexpr_constraints = self.check_expr(env, *fexpr, branch_type.clone());

                (
                    cond_constraints
                        .into_iter()
                        .chain(texpr_constraints)
                        .chain(fexpr_constraints)
                        .collect(),
                    branch_type,
                )
            }
        }
    }

    fn check_expr(&mut self, env: TypeEnv, expr: Expr, expected_type: Type) -> Vec<Constraint> {
        match (expr, expected_type) {
            (Expr::Lit { node_id, literal: Lit::Int(_) }, t) if t == Type::i32() => Vec::new(),
            (Expr::Lit { node_id, literal: Lit::Float(_) }, t) if t == Type::f32() => Vec::new(),
            (Expr::Lit { node_id, literal: Lit::String(_) }, t) if t == Type::str() => Vec::new(),
            (Expr::Lit { node_id, literal: Lit::Char(_) }, t) if t == Type::char() => Vec::new(),
            (Expr::Lit { node_id, literal: Lit::Bool(_) }, t) if t == Type::bool() => Vec::new(),
            (Expr::Lit { node_id, literal: Lit::Unit }, t) if t == Type::unit() => Vec::new(),

            (Expr::Lam { node_id, param_def_id, param, body }, Type::Fun(param_type, ret_type)) => {
                let new_env = env.update(param_def_id, *param_type);
                self.check_expr(new_env, *body, *ret_type)
            }

            (Expr::If { node_id, cond, texpr, fexpr }, branch_type) => {
                let cond_constraints = self.check_expr(env.clone(), *cond, Type::bool());
                let texpr_constraints = self.check_expr(env.clone(), *texpr, branch_type.clone());
                let fexpr_constraints = self.check_expr(env, *fexpr, branch_type);

                cond_constraints
                    .into_iter()
                    .chain(texpr_constraints)
                    .chain(fexpr_constraints)
                    .collect()
            }

            (ast, expected_type) => {
                let (mut constraints, inferred_type) = self.infer_expr(env, ast);
                constraints.push(Constraint::TypeEqual(expected_type, inferred_type));

                constraints
            }
        }
    }

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

    fn unify(&mut self, unnorm_left: Type, unnorm_right: Type) -> Result<(), TypeError> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Var(var_a), Type::Var(var_b)) => (var_a == var_b)
                .then_some(())
                .ok_or_else(|| TypeError::Mismatch(Type::Var(var_a), Type::Var(var_b))),
            
            (Type::Const(name_a), Type::Const(name_b)) if name_a == name_b => Ok(()),

            (Type::Fun(param_a, ret_a), Type::Fun(param_b, ret_b)) => {
                self.unify(*param_a, *param_b)?;
                self.unify(*ret_a, *ret_b)
            }

            (Type::UniVar(uvar_a), Type::UniVar(uvar_b)) => self
                .unification_table
                .unify_var_var(uvar_a, uvar_b)
                .map_err(|(l, r)| TypeError::Mismatch(l, r)),

            (Type::UniVar(uvar), ty) | (ty, Type::UniVar(uvar)) => {
                ty.occurs_check(uvar).map_err(|t| TypeError::Infinite(t))?;

                self.unification_table
                    .unify_var_value(uvar, Some(ty))
                    .map_err(|(l, r)| TypeError::Mismatch(l, r))
            }

            (left, right) => Err(TypeError::Mismatch(left, right)),
        }
    }

    pub fn solve_constraints(&mut self, constraints: Vec<Constraint>) -> Result<(), TypeError> {
        for constraint in constraints {
            match constraint {
                Constraint::TypeEqual(left, right) => self.unify(left, right)?,
            }
        }
        Ok(())
    }
}
