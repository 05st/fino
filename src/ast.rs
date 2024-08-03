use logos::Span;

use crate::types::Type;

pub struct NodeSource {
    span: Span,
    file_path: String,
}

impl NodeSource {
    pub fn new(span: Span, file_path: String) -> Self {
        Self {
            span,
            file_path,
        }
    }
}

#[derive(Debug)]
pub struct NodeId(usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefId(usize);

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
}

// An item is a top-level let-definition. Functions are desugared into curried
// lambda expressions by the parser.
#[derive(Debug)]
pub struct Item {
    name: String,
    type_ann: Type,
    expr: Expr,
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}
