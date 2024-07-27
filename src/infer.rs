use ena::unify::InPlaceUnificationTable;

use crate::ast::*;
use crate::types::*;

enum Constraint {
    TypeEqual(Type, Type),
}

struct TypeInference {
    unification_table: InPlaceUnificationTable<TypeVar>,
}

impl TypeInference {
    fn fresh_type_var(&mut self) -> TypeVar {
        todo!()
    }

    fn synth(&mut self, env: im::HashMap<Var, Type>, ast: Ast<Var>) -> (Ast<TypedVar>, Vec<Constraint>, Type) {
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
                let param_type_var: TypeVar = self.fresh_type_var();
                let new_env = env.update(param, Type::Var(param_type_var));

                let (body_ast, body_constraints, body_type) = self.synth(new_env, *body);

                (
                    Ast::Lam(TypedVar(param, Type::Var(param_type_var)), Box::new(body_ast)),
                    body_constraints,
                    Type::Fun(Box::new(Type::Var(param_type_var)), Box::new(body_type)),
                )
            },

            Ast::App(fun, arg) => {
                let (arg_ast, arg_constraints, arg_type) = self.synth(env.clone(), *arg);
                
                let ret_type = Type::Var(self.fresh_type_var());
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
            Type::Var(var) => match self.unification_table.probe_value(var) {
                Some(var_type) => self.normalize_type(var_type),
                None => Type::Var(var),
            },
            Type::Int => Type::Int,
            Type::Unit => Type::Unit,
            Type::Fun(unnorm_param_type, unnorm_ret_type) => {
                let param_type = self.normalize_type(*unnorm_param_type);
                let ret_type = self.normalize_type(*unnorm_ret_type);
                Type::Fun(Box::new(param_type), Box::new(ret_type))
            },
        }
    }

    fn unify(&mut self, unnorm_left: Type, unnorm_right: Type) -> Result<(), String> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Fun(a_param, a_ret), Type::Fun(b_param, b_ret)) => {
                self.unify(*a_param, *b_param)?;
                self.unify(*a_ret, *b_ret)
            },
            (Type::Var(a), Type::Var(b)) => {
                self.unification_table
                    .unify_var_var(a, b)
                    .map_err(|(l, r)| String::from("Type mismatch"))
            },
            (Type::Var(v), t) | (t, Type::Var(v)) => {
                // TODO: Occurs check
                self.unification_table
                    .unify_var_value(v, Some(t))
                    .map_err(|(l, r)| String::from("Type mismatch"))
            },
            (left, right) => Err(String::from("Type mismatch")),
        }
    }

    fn solve_constraints(&mut self, constraints: Vec<Constraint>) -> Result<(), String> {
        for constraint in constraints {
            match constraint {
                Constraint::TypeEqual(left, right) => self.unify(left, right)?,
            }
        }

        Ok(())
    }
}
