use ast::{Ast, Var};
use ena::unify::InPlaceUnificationTable;
use inference::{Constraint, TypeInference};
use types::{Type, TypeVar};

mod ast;
mod inference;
mod types;

fn main() {
    let mut inf = TypeInference {
        unification_table: InPlaceUnificationTable::new(),
    };
    /*let ast = Ast::App(
        Box::new(Ast::Lam(
            Var(0),
            Box::new(Ast::App(
                Box::new(Ast::Var(Var(0))),
                Box::new(Ast::Var(Var(0))))))),
        Box::new(Ast::Lam(
            Var(1),
            Box::new(Ast::Var(Var(1))))));
    let (ast, constrs, typ) = inf.synth(im::HashMap::new(), ast);*/
    let tv1 = inf.fresh_uni_var();
    let tv2 = inf.fresh_uni_var();
    let tv3 = inf.fresh_uni_var();
    let res = inf.solve_constraints(vec![
        Constraint::TypeEqual(
            Type::UniVar(tv2),
            Type::Fun(Box::new(Type::Int), Box::new(Type::UniVar(tv3))),
        ),
        Constraint::TypeEqual(Type::UniVar(tv1), Type::UniVar(tv2)),
        Constraint::TypeEqual(
            Type::UniVar(tv2),
            Type::Fun(Box::new(Type::Int), Box::new(Type::Int)),
        ),
    ]);
    println!("{:?}", res);
    println!("{:?}", inf.unification_table);
}
