use std::collections::BTreeSet;

use crate::{
    cache::{DefinitionId, ModuleId},
    literal::Literal,
    location::Location,
    types::*,
};

// A qualified name contains the entire path to the name. For example,
// 'abc::xyz::func' is a qualified name. An unqualified name would just be
// 'func'.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Name {
    Unqualified(String),
    Qualified(Vec<String>),
}

impl Name {
    pub fn get_unqualified_part(&self) -> String {
        match self {
            Name::Unqualified(part) => part.clone(),
            Name::Qualified(parts) => parts.last().unwrap().clone(),
        }
    }
}

#[derive(Debug)]
pub enum PatternKind {
    Variant {
        type_name: Name,
        variant_name: String,
        type_definition_id: Option<DefinitionId>,
        field_patterns: Vec<Pattern>,
    },
    Var {
        name: String,
        definition_id: Option<DefinitionId>,
    },
    Lit(Literal),
    Wild,
}

#[derive(Debug)]
pub struct Pattern {
    pub kind: PatternKind,
    pub location: Location,
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Literal),
    // A variable / type constant name can be a qualified or unqualified name. A variable written as a
    // qualified names can only refer to a toplevel definition.
    Var {
        name: Name,
        definition_id: Option<DefinitionId>,
    },
    Variant {
        type_name: Name,
        variant_name: String,
        type_definition_id: Option<DefinitionId>,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Extern {
        fun_name: String,
        args: Vec<Expr>,
        prim_type: TypePrim,
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
    Match {
        mexpr: Box<Expr>,
        branches: Vec<(Pattern, Expr)>,
    },
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub location: Location,
}

#[derive(Debug)]
pub struct TypeVariant {
    pub name: String,
    pub field_types: Vec<Type>,
}

#[derive(Debug)]
pub enum ToplevelKind {
    // Fn definitions are desugared into curried lambda expressions by the parser.
    Let {
        type_scheme: TypeScheme,
        expr: Expr,
        is_main: bool,
    },
    Type {
        type_vars: BTreeSet<TypeVar>,
        variants: Vec<TypeVariant>,
    },
}

#[derive(Debug)]
pub struct Toplevel {
    pub kind: ToplevelKind,
    pub name: String,
    pub location: Location,
    pub definition_id: Option<DefinitionId>,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub module_path: Vec<String>,
    pub location: Location,
    pub module_id: Option<ModuleId>,
}

#[derive(Debug)]
pub enum Export {
    Toplevel {
        name: String,
        location: Location,
        definition_id: Option<DefinitionId>,
    },
    Module {
        module_path: Vec<String>,
        location: Location,
        module_id: Option<ModuleId>,
    },
}

#[derive(Debug)]
pub struct Module {
    pub module_path: Vec<String>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub toplevels: Vec<Toplevel>,
    pub module_id: Option<ModuleId>,
}
