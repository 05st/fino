use std::collections::HashMap;

use crate::ast::*;
use crate::types::*;

enum Constraint {
    TypeEqual(Type, Type),
}

struct TypeChecker {

}

impl TypeChecker {
    fn fresh_type_var(&mut self) -> TypeVar {
        todo!()
    }
    
    fn infer(&mut self, env: &mut HashMap<Var, Type>, ast: Ast<Var>) -> (Vec<Constraint>, Type) {
        match ast {
            Ast::Var(v) => todo!(),
            Ast::Int(i) => todo!(),
            Ast::Fun(param, body) => todo!(),
            Ast::App(fun, arg) => todo!(),
        }
    }

    fn check(&mut self, env: &mut HashMap<Var, Type>, ast: Ast<Var>) -> Vec<Constraint> {
        todo!()
    }
}
