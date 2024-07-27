use crate::ast::*;
use crate::types::*;

enum Constraint {
    TypeEqual(Type, Type),
}

struct TypeInference {

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

            Ast::Fun(param, body) => {
                let param_type_var: TypeVar = self.fresh_type_var();
                let new_env = env.update(param, Type::Var(param_type_var));

                let (body_ast, body_constraints, body_type) = self.synth(new_env, *body);

                (
                    Ast::Fun(TypedVar(param, Type::Var(param_type_var)), Box::new(body_ast)),
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

    fn check(&mut self, env: im::HashMap<Var, Type>, ast: Ast<Var>, check_type: Type) -> (Ast<TypedVar>, Vec<Constraint>) {
        todo!()
    }
}
