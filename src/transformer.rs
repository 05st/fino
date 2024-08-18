use crate::{ast, cache::{CompilerCache, DefinitionId}, mir};

struct Transformer<'a> {
    compiler_cache: &'a mut CompilerCache,
    functions: Vec<mir::Function>,
    misc_name_count: usize,
}

impl<'a> Transformer<'a> {
    fn new(compiler_cache: &mut CompilerCache) -> Transformer {
        Transformer {
            compiler_cache,
            functions: Vec::new(),
            misc_name_count: 0,
        }
    }

    fn get_definition_name(&self, definition_id: &DefinitionId) -> String {
        self.compiler_cache[definition_id].qualified_name.join("_")
    }

    // Flattens nested App exprs into a list of all the args, and returns the first non-app fun
    // it encounters, at which point it stops.
    fn flatten_app_exprs<'e>(expr: &'e ast::Expr, args: &mut Vec<&'e ast::Expr>) -> &'e ast::Expr {
        match &expr.kind {
            ast::ExprKind::App { fun, arg } => {
                args.push(arg);
                Transformer::flatten_app_exprs(fun, args)
            }
            _ => expr,
        }
    }

    fn transform_expr(&mut self, expr: &ast::Expr) -> mir::Expr {
        match &expr.kind {
            ast::ExprKind::Lit(literal) => mir::Expr::Lit(literal.clone()),

            ast::ExprKind::Var { name: _, definition_id } => {
                mir::Expr::Var(self.get_definition_name(definition_id.as_ref().unwrap()))
            }

            ast::ExprKind::App { .. } => {
                let mut args = Vec::new();
                let fun = Transformer::flatten_app_exprs(expr, &mut args);

                let fun_name = match &fun.kind {
                    ast::ExprKind::Var { name: _, definition_id } => {
                        self.get_definition_name(definition_id.as_ref().unwrap())
                    }
                    _ => {
                        // TODO:
                        // If the function expr is not a variable, we need to wrap it in a new closure
                        todo!()
                    }
                };

                mir::Expr::App {
                    fun_name,
                    args: args.into_iter().map(|f| self.transform_expr(f)).collect(),
                }
            }

            ast::ExprKind::Lam { param_name, body, param_definition_id } => todo!(),

            ast::ExprKind::Let { name: _, aexpr, body, definition_id } => {
                mir::Expr::Let {
                    name: self.get_definition_name(definition_id.as_ref().unwrap()),
                    aexpr: Box::new(self.transform_expr(aexpr)),
                    body: Box::new(self.transform_expr(body)),
                }
            }

            ast::ExprKind::If { cond, texpr, fexpr } => {
                mir::Expr::If {
                    cond: Box::new(self.transform_expr(cond)),
                    texpr: Box::new(self.transform_expr(texpr)),
                    fexpr: Box::new(self.transform_expr(fexpr)),
                }
            }
        }
    }

    fn transform_module(&mut self, module: &ast::Module) -> () {

    }
}

pub fn transform_program(compiler_cache: &mut CompilerCache) -> Vec<mir::Function> {
    let mut transformer = Transformer::new(compiler_cache);

    let mut queue = Vec::new();
    while let Some(module) = transformer.compiler_cache.modules.pop_front() {
        transformer.transform_module(&module);
        queue.push(module);
    }

    while let Some(module) = queue.pop() {
        transformer.compiler_cache.modules.push_front(module);
    }

    transformer.functions
}
