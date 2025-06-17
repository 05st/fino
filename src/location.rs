use std::{ops::Range, path::PathBuf};

pub type Span = Range<usize>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Location {
    pub path: PathBuf,
    pub span: Span,
}

impl Location {
    pub fn new(path: PathBuf, span: Span) -> Location {
        Location { path, span }
    }
}
