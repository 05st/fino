use std::fmt::Display;
use std::fs::read_to_string;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::ast::Name;
use crate::cache::Location;
use crate::lexer::Token;
use crate::types::Type;

#[derive(Debug)]
pub enum ErrorKind {
    // Parser errors
    UnknownToken,
    ExpectedOneOf(Token, Vec<String>),
    UnexpectedIndent(usize),
    UndeclaredOperator(String),
    InvalidPrefixOperator(String),

    // Module sort errors
    CircularDependency,
    UnknownModule,

    // Name resolution errors
    UnknownVariable(Name),
    Redefinition(Vec<String>),

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
        let source = read_to_string(self.location.filepath.as_path()).unwrap();
        let filename = self.location.filepath.to_string_lossy().into_owned();

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
            UnexpectedIndent(size) => write!(f, "Unexpected indent of size {} here", size),
            UndeclaredOperator(oper) => write!(f, "Undeclared operator {} encountered here", oper),
            InvalidPrefixOperator(oper) => write!(f, "Invalid prefix operator {}", oper),

            CircularDependency => write!(f, "Modules have a circular dependency"),
            UnknownModule => write!(f, "Could not find module"),

            UnknownVariable(name) => match name {
                Name::Qualified(qual) => write!(f, "Unknown variable {}", qual.join(".")),
                Name::Unqualified(ident) => write!(f, "Unknown variable {}", ident),
            },
            Redefinition(qual) => write!(f, "{} is already defined", qual.join(".")),

            TypeMismatch(type_a, type_b) => write!(f, "Type mismatch between {} and {} here", type_a, type_b),
            InfiniteType(t) => write!(f, "Attempt to construct infinite type {} here", t),
        }
    }
}
