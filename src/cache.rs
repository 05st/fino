use std::{collections::HashMap, path::PathBuf};

use logos::Span;

use crate::{ast::NodeId, error::{Error, ErrorKind}};

#[derive(Debug, Clone)]
pub struct Location {
    pub filepath: PathBuf,
    pub span: Span,
}

impl Location {
    pub fn new(filepath: PathBuf, span: Span) -> Self {
        Location { filepath, span }
    }
}

pub struct CompilerCache {
    pub location_map: HashMap<NodeId, Location>,
}

impl CompilerCache {
    pub fn make_error(&self, error: ErrorKind, node_id: &NodeId) -> Error {
        Error::new(error, self.location_map[node_id].clone())
    }
}
