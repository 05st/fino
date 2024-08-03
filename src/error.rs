use crate::ast::NodeSource;

pub struct CompilerError<E> {
    error: E,
    source: Option<NodeSource>,
}

impl<E> CompilerError<E> {
    pub fn new(error: E, source: Option<NodeSource>) -> Self {
        Self {
            error,
            source,
        }
    }

    pub fn from(error: E) -> Self {
        Self::new(error, None)
    }

    pub fn with_source(error: E, source: NodeSource) -> Self {
        Self::new(error, Some(source))
    }

    pub fn report(&self) {
        
    }
}
