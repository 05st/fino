use crate::{cache::{DefinitionId, ModuleId}, literal::Literal, location::Location, types::*};

// A qualified name contains the entire path to the name. For example,
// 'abc::xyz::func' is a qualified name. An unqualified name would just be
// 'func'.
#[derive(Clone, Debug)]
pub enum Name {
    Unqualified(String),
    Qualified(Vec<String>),
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Literal),
    Var {
        // A variable can be a qualified or unqualified name. A variable written as a
        // qualified names can only refer to a top-level definition.
        name: Name,
        definition_id: Option<DefinitionId>,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Lam {
        param_name: String,
        body: Box<Expr>,
        param_definition_id: Option<DefinitionId>,
    },
    Let {
        name: String,
        aexpr: Box<Expr>,
        body: Box<Expr>,
        definition_id: Option<DefinitionId>,
    },
    If {
        cond: Box<Expr>,
        texpr: Box<Expr>,
        fexpr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub location: Location,
}

// An item is a top-level let-definition. Fn definitions are desugared into curried
// lambda expressions by the parser. The fully qualified name of the item is
// given by module name ++ name;
#[derive(Debug)]
pub struct Item {
    pub name: String,
    pub type_scheme: TypeScheme,
    pub expr: Expr,
    pub location: Location,
    pub definition_id: Option<DefinitionId>,
    pub is_main: bool,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub module_path: Vec<String>,
    pub location: Location,
    pub module_id: Option<ModuleId>,
}

#[derive(Debug)]
pub enum Export {
    Item {
        item_name: String,
        location: Location,
        definition_id: Option<DefinitionId>,

    },
    Module {
        module_path: Vec<String>,
        location: Location,
        module_id: Option<ModuleId>,
    },
}

// Externs are treated as an item by the name resolver / type checker. It is up
// to the programmer to ensure the function exists.
#[derive(Debug)]
pub struct Extern {
    pub name: String,
    pub type_scheme: TypeScheme,
    pub location: Location,
    pub definition_id: Option<DefinitionId>,
}

#[derive(Debug)]
pub struct Module {
    pub module_path: Vec<String>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub externs: Vec<Extern>,
    pub items: Vec<Item>,
    pub module_id: Option<ModuleId>,
}
