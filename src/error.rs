use ariadne::{Report, ReportKind, Source};
use logos::Span;

use crate::ast::NodeSource;

pub struct CompilerError<E> {
    error: E,
    span: Option<Span>,
    file_path: String,
}

impl<E> CompilerError<E> {
    pub fn new(error: E, span: Option<Span>, file_path: String) -> Self {
        Self {
            error,
            span,
            file_path,
        }
    }

    pub fn primitive(error: E, file_path: String) -> Self {
        Self::new(error, None, file_path)
    }

    pub fn from_source(error: E, source: NodeSource) -> Self {
        Self::new(error, Some(source.span), source.file_path)
    }

    pub fn report(&self) {
        Report::<Span>::build(ReportKind::Error, (), 0)
            .with_message("reached end")
            .finish()
            .print(Source::from(self.file_path.clone())).unwrap();
    }
}
