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

// A qualified name contains the entire path to the name. For example,
// 'abc::xyz::func' is a qualified name. An unqualified name would just be
// 'func'.
#[derive(Debug)]
pub enum Name {
    Unqualified(String),
    Qualified(Vec<String>),
}

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
        node_id: NodeId,
        literal: Lit,
    },
    Var {
        node_id: NodeId,
        def_id: DefId,
        name: Name,
    },
    App {
        node_id: NodeId,
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Lam {
        node_id: NodeId,
        param_def_id: DefId,
        param: String,
        body: Box<Expr>,
    },
    If {
        node_id: NodeId,
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    },
}

// An item is a top-level let-definition. Functions are desugared into curried
// lambda expressions by the parser.
#[derive(Debug)]
pub struct Item {
    pub name: String,
    pub type_ann: Type,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub node_id: NodeId,
    // Fully qualified name of module
    pub module_name: Vec<String>,
}

#[derive(Debug)]
pub enum Export {
    Item {
        node_id: NodeId,
        def_id: DefId,
        name: String,
    },
    Module {
        node_id: NodeId,
        // Fully qualified name of module
        module_name: Vec<String>,
    },
}

#[derive(Debug)]
pub struct Module {
    pub module_name: Vec<String>,
    pub items: Vec<Item>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}
