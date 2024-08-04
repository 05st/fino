use logos::Span;

use crate::types::Type;

pub struct NodeSource {
    pub span: Span,
    pub file_path: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefId(pub usize);

#[derive(Debug)]
pub enum Lit {
    Int(u64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unit,
}

#[derive(Debug)]
pub enum Expr {
    Lit {
        id: NodeId,
        literal: Lit,
    },
    Var {
        id: NodeId,
        def_id: DefId,
        name: String,
    },
    App {
        id: NodeId,
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Lam {
        id: NodeId,
        param: String,
        param_def_id: DefId,
        body: Box<Expr>,
    },
    If {
        id: NodeId,
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    }
}

// An item is a top-level let-definition. Functions are desugared into curried
// lambda expressions by the parser.
#[derive(Debug)]
pub struct Item {
    pub name: String,
    pub type_ann: Type,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}
