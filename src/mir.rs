use crate::literal::Literal;

#[derive(Debug)]
pub enum Expr {
    Lit(Literal),
    Var(String),
    App {
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

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub body: Expr,
}

#[derive(Debug)]
pub enum Type {
    Bool,
    Char,
    String,
    Int32,
    Float32,
    Fun(Vec<Type>, Box<Type>)
}
