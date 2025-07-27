use crate::literal::Literal;

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Literal),
    Var(String),
    TypeInst {
        tag: i64,
        params: Vec<String>,
    },
    Closure {
        fun_name: String,
        env: Vec<String>,
    },
    App {
        closure: Box<Expr>,
        arg: Box<Expr>,
    },
    Extern {
        fun_name: String,
        args: Vec<Expr>,
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
pub enum Toplevel {
    Function {
        name: String,
        env: Vec<String>,
        param: String,
        body: Expr,
    },
    Variable {
        name: String,
        body: Expr,
        is_main: bool,
    },
}
