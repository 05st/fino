use crate::types::Type;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefId(pub usize);

// A qualified name contains the entire path to the name. For example,
// 'abc::xyz::func' is a qualified name. An unqualified name would just be
// 'func'.
#[derive(Clone, Debug)]
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
        // A variable can be a qualified or unqualified name. A variable written as a
        // qualified names can only refer to a top-level definition.
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
// lambda expressions by the parser. The fully qualified name of the item is
// given by module_name ++ name;
#[derive(Debug)]
pub struct Item {
    pub node_id: NodeId,
    pub name: String,
    pub def_id: DefId,
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
