use crate::{
    ast::{Item, Module},
    error::CompilerError,
};

enum ResolveError {

}

type ResolveResult<T> = Result<T, CompilerError<ResolveError>>;

struct NameResolution {

}

impl NameResolution {
    pub fn new() -> Self {
        todo!()
    }

    fn resolve_item(&mut self, item: &mut Item) -> ResolveResult<()> {
        todo!()
    }

    fn resolve_module(&mut self, module: &mut Module) -> ResolveResult<()> {
        todo!()
    }

    fn resolve() {
        todo!()
    }
}
