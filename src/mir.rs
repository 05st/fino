use crate::{cache::DefinitionId, literal::Literal};

pub enum Expr {
    Lit(Literal),
    Var(DefinitionId),
    App {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    },
    Let {
        aexpr: Box<Expr>,
        body: Box<Expr>,
        definition_id: DefinitionId,
    },
}

pub struct Function {
    pub definition_id: DefinitionId,
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub body: Expr,
}

pub enum Type {
    Bool,
    Char,
    String,
    Int32,
    Float32,
    Fun(Vec<Type>, Box<Type>)
}
