use clap::Parser as _;
use parser::Parser;

mod ast;
mod error;
mod inference;
mod lexer;
mod parser;
mod types;

#[derive(clap::Parser, Debug)]
struct Args {
    // Input path
    #[arg(default_value = "./", help = "Input path")]
    src: String,

    // Output path
    #[arg(short, long, default_value = "a", help = "Output path")]
    out: String,

    // Input is a file rather than dir
    #[arg(short, long, help = "Input is a file")]
    file: bool,
}

fn main() {
    let args = Args::parse();
    let input = std::fs::read_to_string(args.src).unwrap();
    let mut parser = Parser::new();
    match parser.parse(&input) {
        Ok(res) => println!("{:?}", res),
        Err(err) => {
            todo!()
        }
    }

    /*
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
    */
}
