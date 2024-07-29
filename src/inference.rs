use ena::unify::InPlaceUnificationTable;

use crate::ast::*;
use crate::types::*;

#[derive(Clone, Debug)]
pub enum Constraint {
    TypeEqual(Type, Type),
}

pub struct TypeInference {
    pub unification_table: InPlaceUnificationTable<TypeUniVar>,
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch(Type, Type),
    Infinite(Type),
}

impl TypeInference {
    pub fn fresh_uni_var(&mut self) -> TypeUniVar {
        self.unification_table.new_key(None)
    }

    pub fn synth(&mut self, env: im::HashMap<Var, Type>, ast: Ast<Var>) -> (Ast<TypedVar>, Vec<Constraint>, Type) {
        match ast {
            Ast::Var(var) => {
                // Variable is guaranteed to be defined after name resolution
                let var_type = &env[&var];
                (
                    Ast::Var(TypedVar(var, var_type.clone())),
                    Vec::new(),
                    var_type.clone(),
                )
            },
            Ast::Int(i) => (
                Ast::Int(i),
                Vec::new(),
                Type::Int,
            ),
            Ast::Lam(param, body) => {
                let param_uvar = self.fresh_uni_var();
                let new_env = env.update(param, Type::UniVar(param_uvar));

                let (body_ast, body_constraints, body_type) = self.synth(new_env, *body);
                (
                    Ast::Lam(TypedVar(param, Type::UniVar(param_uvar)), Box::new(body_ast)),
                    body_constraints,
                    Type::Fun(Box::new(Type::UniVar(param_uvar)), Box::new(body_type)),
                )
            },
            Ast::App(fun, arg) => {
                let (arg_ast, arg_constraints, arg_type) = self.synth(env.clone(), *arg);
                
                let ret_type = Type::UniVar(self.fresh_uni_var());
                let fun_type = Type::Fun(Box::new(arg_type), Box::new(ret_type.clone()));

                let (fun_ast, fun_constraints) = self.check(env, *fun, fun_type);
                (
                    Ast::App(Box::new(fun_ast), Box::new(arg_ast)),
                    arg_constraints
                        .into_iter()
                        .chain(fun_constraints)
                        .collect(),
                    ret_type,
                )
            },
        }
    }

    fn check(&mut self, env: im::HashMap<Var, Type>, ast: Ast<Var>, exp_type: Type) -> (Ast<TypedVar>, Vec<Constraint>) {
        match (ast, exp_type) {
            (Ast::Int(i), Type::Int) => (
                Ast::Int(i),
                Vec::new(),
            ),

            (Ast::Lam(param, body), Type::Fun(param_type, ret_type)) => {
                let new_env = env.update(param, *param_type);
                self.check(new_env, *body, *ret_type)
            },
            
            (ast, exp_type) => {
                let (typed_ast, mut constraints, inferred_type) = self.synth(env, ast);
                constraints.push(Constraint::TypeEqual(exp_type, inferred_type));

                (
                    typed_ast,
                    constraints,
                )
            }
        }
    }

    fn normalize_type(&mut self, unnorm: Type) -> Type {
        match unnorm {
            Type::Var(_) | Type::Int | Type::Unit => unnorm,
            Type::UniVar(uvar) => {
                match self.unification_table.probe_value(uvar) {
                    Some(bound_type) => self.normalize_type(bound_type),
                    None => unnorm,
                }
            },
            Type::Fun(unnorm_param_type, unnorm_ret_type) => {
                let param_type = self.normalize_type(*unnorm_param_type);
                let ret_type = self.normalize_type(*unnorm_ret_type);
                Type::Fun(Box::new(param_type), Box::new(ret_type))
            },
        }
    }

    /* Note: It is important to note the distinction made between type variables and unification
     *       variables. A type variable is rigid, they are universally quantified, and they should
     *       not be present anywhere during unification. They are only introduced through type
     *       annotations as of now. When generalized types (type schemes) are instantiated, any
     *       occurence of a type variable will be replaced by fresh unification variables. A
     *       unification variable is flexible, they are placeholders for a rigid, concrete type.
     *       This could be a type constant, function type, or a type variable as well.
     */
    fn unify(&mut self, unnorm_left: Type, unnorm_right: Type) -> Result<(), TypeError> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Var(var_a), Type::Var(var_b)) => {
                (var_a == var_b)
                    .then_some(())
                    .ok_or_else(|| TypeError::Mismatch(Type::Var(var_a), Type::Var(var_b)))
            },
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Fun(a_param, a_ret), Type::Fun(b_param, b_ret)) => {
                self.unify(*a_param, *b_param)?;
                self.unify(*a_ret, *b_ret)
            },
            (Type::UniVar(uvar_a), Type::UniVar(uvar_b)) => {
                self.unification_table
                    .unify_var_var(uvar_a, uvar_b)
                    .map_err(|(l, r)| TypeError::Mismatch(l, r))
            },
            (Type::UniVar(uvar), ty) | (ty, Type::UniVar(uvar)) => {
                ty.occurs_check(uvar)
                    .map_err(|t| TypeError::Infinite(t))?;

                self.unification_table
                    .unify_var_value(uvar, Some(ty))
                    .map_err(|(l, r)| TypeError::Mismatch(l, r))
            },
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
