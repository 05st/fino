use crate::literal::Literal;

#[derive(Debug)]
pub enum Expr {
    Lit(Literal),
    Var(String),
    Closure {
        fun_name: String,
        free_vars: Vec<String>,
    },
    App {
        fun: Box<Expr>,
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

#[derive(Debug)]
pub enum Global {
    Function {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
    Variable {
        name: String,
        body: Expr,
    }
}

#[derive(Debug)]
pub enum Type {
    Int,
}
