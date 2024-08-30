use crate::literal::Literal;

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Literal),
    Var(String),
    Closure {
        fun_name: String,
        env: Vec<String>,
    },
    App {
        closure: Box<Expr>,
        arg: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    },
    Let {
        name: String,
        aexpr: Box<Expr>,
        body: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Global {
    Function {
        name: String,
        env: Vec<String>,
        param: String,
        body: Expr,
        is_main: bool,
    },
    Variable {
        name: String,
        body: Expr,
    }
}
