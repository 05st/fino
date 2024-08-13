use std::{ops::Range, path::PathBuf};

use crate::types::*;

pub type Span = Range<usize>;

#[derive(Clone, Debug)]
pub struct Location {
    pub path: PathBuf,
    pub span: Range<usize>,
}

impl Location {
    pub fn new(path: PathBuf, span: Span) -> Location {
        Location { path, span }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefId(pub usize);

impl Default for DefId {
    fn default() -> DefId {
        DefId(0)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExprId(pub usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModuleId(pub usize);

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
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unit,
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Var {
        def_id: DefId,
        // A variable can be a qualified or unqualified name. A variable written as a
        // qualified names can only refer to a top-level definition.
        name: Name,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Lam {
        param_def_id: DefId,
        param: String,
        body: Box<Expr>,
    },
    Let {
        def_id: DefId,
        name: String,
        expr: Box<Expr>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Expr {
    pub expr_id: ExprId,
    pub location: Location,
    pub kind: ExprKind,
}

// An item is a top-level let-definition. Fn definitions are desugared into curried
// lambda expressions by the parser. The fully qualified name of the item is
// given by module name ++ name;
#[derive(Debug)]
pub struct Item {
    pub location: Location,
    pub def_id: DefId,
    pub name: String,
    pub scheme: TypeScheme,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub location: Location,
    // Fully qualified name of module
    pub module_name: Vec<String>,
}

#[derive(Debug)]
pub enum Export {
    Item {
        location: Location,
        def_id: DefId,
        item_name: String,
    },
    Module {
        location: Location,
        // Fully qualified name of module
        module_name: Vec<String>,
    },
}

// Externs are treated as an item by the name resolver / type checker. It is up
// to the programmer to ensure the function exists.
#[derive(Debug)]
pub struct Extern {
    pub location: Location,
    pub def_id: DefId,
    pub name: String,
    pub scheme: TypeScheme,
}

#[derive(Debug)]
pub struct Module {
    pub module_id: ModuleId,
    pub module_name: Vec<String>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub externs: Vec<Extern>,
    pub items: Vec<Item>,
}
