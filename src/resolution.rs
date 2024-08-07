use crate::{
    ast::{Item, Module, Name}, cache::CompilerCache, error::Error
};

struct NameResolution<'a> {
    compiler_cache: &'a mut CompilerCache,
}

impl<'a> NameResolution<'a> {
    pub fn new(compiler_cache: &'a mut CompilerCache) -> NameResolution<'a> {
        NameResolution { compiler_cache }
    }

    fn resolve_item(&mut self, item: &mut Item) -> Result<(), Error> {
        todo!()
    }

    fn resolve_module(&mut self, module: &mut Module) -> Result<(), Error> {
        todo!()
    }

    fn resolve() {
        todo!()
    }
}
