use crate::{ast, cache::CompilerCache, mir};

struct Transformer<'a> {
    compiler_cache: &'a mut CompilerCache,
    functions: Vec<mir::Function>,
}

impl<'a> Transformer<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> Transformer {
        Transformer {
            compiler_cache,
            functions: Vec::new(),
        }
    }

    fn transform_expr(&mut self, expr: &ast::Expr) -> mir::Expr {
        match &expr.kind {
            ast::ExprKind::Lit(literal) => mir::Expr::Lit(literal.clone()),
            ast::ExprKind::Var { name: _, definition_id } => mir::Expr::Var(definition_id.clone().unwrap()),
            ast::ExprKind::App { fun, arg } => todo!(),
            ast::ExprKind::Lam { param_name, body, param_definition_id } => todo!(),
            ast::ExprKind::Let { name, aexpr, body, definition_id } => todo!(),
            ast::ExprKind::If { cond, texpr, fexpr } => todo!(),
        }
    }
}

pub fn transform_program(compiler_cache: &mut CompilerCache) -> Vec<mir::Function> {
    let transformer = Transformer::new(compiler_cache);
    transformer.functions
}
