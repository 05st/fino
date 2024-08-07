use std::fmt::Display;

use crate::types::Type;
use crate::cache::Location;

pub enum ErrorKind {
    // Parser errors
    ReachedEnd,
    UnknownToken,
    ExpectedOneOf(Vec<String>),
    ExpectedIndent(usize, usize),

    // Module sort errors
    CircularDependency,
    UndefinedModule,

    // Type inference errors
    TypeMismatch(Type, Type),
    InfiniteType,
}

pub struct Error {
    error: ErrorKind,
    location: Location,
}

impl Error {
    pub fn new(error: ErrorKind, location: Location) -> Error {
        Error {
            error,
            location,
        }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorKind::*;

        match self {
            ReachedEnd => write!(f, "Reached end of input"),
            UnknownToken => write!(f, "Encountered unknown token here"),
            ExpectedOneOf(items) => {
                if items.len() == 1 {
                    write!(f, "Expected {} here", items[0])
                } else {
                    write!(f, "Expected one of {} here", items.join(", "))
                }
            },
            ExpectedIndent(expected, got) => write!(f, "Expected {}-wide indent here, got {}-wide", expected, got),

            CircularDependency => write!(f, "Modules have a circular dependency"),
            UndefinedModule => write!(f, "Could not find module"),

            TypeMismatch(type_a, type_b) => write!(f, "Type mismatch between {} and {} here", type_a, type_b),
            InfiniteType => write!(f, "Attempt to construct infinite type here"),
        }
    }
}
