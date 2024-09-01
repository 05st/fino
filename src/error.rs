use std::fmt::Display;
use std::fs::read_to_string;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::lexer::Token;
use crate::location::Location;
use crate::types::Type;

#[derive(Debug)]
pub enum ErrorKind {
    // Parser errors
    UnknownToken,
    ExpectedOneOf(Token, Vec<String>),
    InvalidIndentation(usize, usize),
    UndeclaredOperator(String),
    InvalidPrefixOperator(String),
    MultipleMainDefinitions,
    InvalidMainDefinitionType,

    // Module sort errors
    CircularDependency,
    UnknownModule(Vec<String>),

    // Name resolution errors
    UnknownVariable(Vec<String>),
    Redefinition(Vec<String>), // Only possible with toplevel definitions
    MultipleDefinitions(String), // Only possible when looking up unqualified name
    ExportedUnimportedModule(Vec<String>),
    AlreadyImportedModule(Vec<String>),
    // AlreadyExportedDefinition(String),
    // AlreadyExportedModule(Vec<String>),

    // Type inference errors
    TypeMismatch(Type, Type),
    InfiniteType(Type),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    location: Location,
}

impl Error {
    pub fn new(kind: ErrorKind, location: Location) -> Error {
        Error { kind, location }
    }

    pub fn report(&self) {
        // File was already read for input to parser, we should probably cache that and
        // use it here instead of reading it again.
        let source = read_to_string(self.location.path.as_path()).unwrap();
        let filename = self.location.path.to_string_lossy().into_owned();

        Report::build(ReportKind::Error, &filename, self.location.span.start)
            .with_message(self.kind.to_string())
            .with_label(Label::new((&filename, self.location.span.clone())).with_color(Color::Red))
            .finish()
            .eprint((&filename, Source::from(source)))
            .unwrap();
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorKind::*;

        match self {
            UnknownToken => write!(f, "Encountered unknown token here"),
            ExpectedOneOf(got, allowed) => {
                if allowed.len() == 1 {
                    write!(f, "Expected {} here, got {}", allowed[0], got)
                } else {
                    write!(f, "Expected one of {} here, got {}", allowed.join(", "), got)
                }
            }
            InvalidIndentation(got, exp) => write!(f, "Invalid indentation of size {} here, expected at least {}", got, exp),
            UndeclaredOperator(oper) => write!(f, "Undeclared operator {} encountered here", oper),
            InvalidPrefixOperator(oper) => write!(f, "Invalid prefix operator {}", oper),
            MultipleMainDefinitions => write!(f, "Found multiple main definitions"),
            InvalidMainDefinitionType => write!(f, "Main definition must have a type of unit"),

            CircularDependency => write!(f, "Modules have a circular dependency"),
            UnknownModule(module_path) => write!(f, "Could not find module {}", module_path.join(".")),

            UnknownVariable(name) => write!(f, "Unknown variable {}", name.join(".")),
            Redefinition(qual) => write!(f, "{} is already defined", qual.join(".")),
            MultipleDefinitions(name) => write!(f, "Multiple definitions for {} found", name),
            ExportedUnimportedModule(module_path) => write!(f, "{} was never imported", module_path.join(".")),
            AlreadyImportedModule(module_path) => write!(f, "{} was already imported", module_path.join(".")),
            // AlreadyExportedDefinition(name) => write!(f, "{} was already exported", name),
            // AlreadyExportedModule(module_path) => write!(f, "{} was already exported", module_path.join(".")),

            TypeMismatch(type_a, type_b) => write!(f, "Type mismatch between {} and {} here", type_a, type_b),
            InfiniteType(t) => write!(f, "Attempt to construct infinite type {} here", t),
        }
    }
}
