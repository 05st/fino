use std::{collections::HashMap, path::PathBuf};

use logos::Span;

use crate::ast::NodeId;

#[derive(Debug, Clone)]
pub struct Location {
    filepath: PathBuf,
    span: Span,
}

impl Location {
    pub fn new(filepath: PathBuf, span: Span) -> Self {
        Location { filepath, span }
    }
}

pub struct CompilerCache {
    pub location_map: HashMap<NodeId, Location>,
}
