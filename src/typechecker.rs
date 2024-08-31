use std::collections::HashMap;

use ena::unify::InPlaceUnificationTable;

use crate::ast::*;
use crate::cache::{CompilerCache, DefinitionId};
use crate::error::{Error, ErrorKind};
use crate::literal::Literal;
use crate::location::Location;
use crate::types::*;

#[derive(Clone, Debug)]
enum Constraint {
    TypeEqual(Type, Type, Location),
}

struct TypeChecker<'a> {
    compiler_cache: &'a mut CompilerCache,
    unification_table: InPlaceUnificationTable<TypeUniVar>,
    type_schemes: HashMap<DefinitionId, TypeScheme>,
}

impl<'a> TypeChecker<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> TypeChecker {
        TypeChecker {
            compiler_cache,
            unification_table: InPlaceUnificationTable::new(),
            type_schemes: HashMap::new(),
        }
    }

    fn fresh_uni_var(&mut self) -> TypeUniVar {
        self.unification_table.new_key(None)
    }

    // Infer (synthesize) expression type. Only one of infer_expr or check_expr
    // should ever be called on an expression
    fn infer_expr(&mut self, env: im::HashMap<DefinitionId, Type>, expr: &Expr) -> (Vec<Constraint>, Type) {
        let (constraints, expr_type) = match &expr.kind {
            ExprKind::Lit(literal) => {
                (
                    Vec::new(),
                    match literal {
                        Literal::Int(_) => Type::int(),
                        Literal::Float(_) => Type::float(),
                        Literal::String(_) => Type::str(),
                        Literal::Char(_) => Type::char(),
                        Literal::Bool(_) => Type::bool(),
                        Literal::Unit => Type::unit(),
                    }
                )
            }

            ExprKind::Var { name: _, definition_id } => {
                // Variable is guaranteed to be defined after name resolution, either in the
                // local scope or as a top-level definition.
                let var_type = match env.get(&definition_id.as_ref().unwrap()) {
                    Some(ty) => ty.clone(),
                    None => {
                        // Instantiate type scheme
                        let scheme = &self.type_schemes[&definition_id.as_ref().unwrap()].clone();
                        let subst = scheme.0
                            .iter()
                            .map(|tvar| (Type::Var(tvar.clone()), Type::UniVar(self.fresh_uni_var())))
                            .collect::<HashMap<_, _>>();
                        scheme.1.substitute(&subst)
                    },
                };

                (Vec::new(), var_type)
            }

            ExprKind::Lam { param_name: _, body, param_definition_id } => {
                let param_uvar = self.fresh_uni_var();
                let new_env = env.update(param_definition_id.clone().unwrap(), Type::UniVar(param_uvar));

                let (body_constraints, body_type) = self.infer_expr(new_env, &body);
                (
                    body_constraints,
                    Type::Fun(Box::new(Type::UniVar(param_uvar)), Box::new(body_type)),
                )
            }

            ExprKind::App { fun, arg } => {
                let (arg_constraints, arg_type) = self.infer_expr(env.clone(), &arg);

                let ret_type = Type::UniVar(self.fresh_uni_var());
                let fun_type = Type::Fun(Box::new(arg_type), Box::new(ret_type.clone()));

                let fun_constraints = self.check_expr(env, &fun, fun_type);
                (
                    arg_constraints.into_iter().chain(fun_constraints).collect(),
                    ret_type,
                )
            }

            ExprKind::Extern { fun_name: _, args, prim_type } => {
                let mut args_constraints = Vec::new();
                for arg in args {
                    let (mut constraints, _) = self.infer_expr(env.clone(), arg);
                    args_constraints.append(&mut constraints);
                }

                (
                    args_constraints,
                    prim_type.clone()
                )
            }

            ExprKind::Let { name: _, aexpr, body, definition_id } => {
                let (expr_constraints, def_type) = self.infer_expr(env.clone(), &aexpr);
                let new_env = env.update(definition_id.clone().unwrap(), def_type);
                let (body_constraints, body_type) = self.infer_expr(new_env, &body);

                (
                    expr_constraints.into_iter().chain(body_constraints).collect(),
                    body_type,
                )
            }

            ExprKind::If { cond, texpr, fexpr } => {
                let cond_constraints = self.check_expr(env.clone(), &cond, Type::bool());

                let (texpr_constraints, branch_type) = self.infer_expr(env.clone(), &texpr);
                let fexpr_constraints = self.check_expr(env, &fexpr, branch_type.clone());

                (
                    cond_constraints
                        .into_iter()
                        .chain(texpr_constraints)
                        .chain(fexpr_constraints)
                        .collect(),
                    branch_type,
                )
            }
        };

        (constraints, expr_type)
    }

    // Check expression type against expected type. If it's not possible to deduce
    // type from info available, calls infer_expr and stores type equality
    // constraint.
    // TODO:
    // We can probably pass expected_type as a reference here
    fn check_expr(&mut self, env: im::HashMap<DefinitionId, Type>, expr: &Expr, expected_type: Type) -> Vec<Constraint> {
        match (&expr.kind, expected_type) {
            (ExprKind::Lit(Literal::Int(_)), t) if t == Type::int() => Vec::new(),
            (ExprKind::Lit(Literal::Float(_)), t) if t == Type::float() => Vec::new(),
            (ExprKind::Lit(Literal::String(_)), t) if t == Type::str() => Vec::new(),
            (ExprKind::Lit(Literal::Char(_)), t) if t == Type::char() => Vec::new(),
            (ExprKind::Lit(Literal::Bool(_)), t) if t == Type::bool() => Vec::new(),
            (ExprKind::Lit(Literal::Unit), t) if t == Type::unit() => Vec::new(),

            (ExprKind::Lam { param_name: _, body, param_definition_id }, Type::Fun(param_type, ret_type)) => {
                let new_env = env.update(param_definition_id.clone().unwrap(), *param_type);
                self.check_expr(new_env, &body, *ret_type)
            }

            (ExprKind::If { cond, texpr, fexpr }, branch_type) => {
                let cond_constraints = self.check_expr(env.clone(), &cond, Type::bool());
                let texpr_constraints = self.check_expr(env.clone(), &texpr, branch_type.clone());
                let fexpr_constraints = self.check_expr(env, &fexpr, branch_type);

                cond_constraints
                    .into_iter()
                    .chain(texpr_constraints)
                    .chain(fexpr_constraints)
                    .collect()
            }

            (_, expected_type) => {
                let (mut constraints, inferred_type) = self.infer_expr(env, expr);
                constraints.push(Constraint::TypeEqual(
                    expected_type,
                    inferred_type,
                    expr.location.clone(),
                ));

                constraints
            }
        }
    }

    // Get main representative of type in unification table
    fn normalize_type(&mut self, unnorm: Type) -> Type {
        match unnorm {
            Type::Var(_) | Type::Const(_) => unnorm,

            Type::UniVar(uvar) => match self.unification_table.probe_value(uvar) {
                Some(bound_type) => self.normalize_type(bound_type),
                None => unnorm,
            }

            Type::Fun(unnorm_param_type, unnorm_ret_type) => {
                let param_type = self.normalize_type(*unnorm_param_type);
                let ret_type = self.normalize_type(*unnorm_ret_type);
                Type::Fun(Box::new(param_type), Box::new(ret_type))
            }
        }
    }

    fn unify(&mut self, unnorm_left: Type, unnorm_right: Type, location: Location) -> Result<(), Error> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Var(var_a), Type::Var(var_b)) => {
                (var_a == var_b).then_some(()).ok_or_else(|| todo!())
            }
            
            (Type::Const(name_a), Type::Const(name_b)) if name_a == name_b => Ok(()),

            (Type::Fun(param_a, ret_a), Type::Fun(param_b, ret_b)) => {
                self.unify(*param_a, *param_b, location.clone())?;
                self.unify(*ret_a, *ret_b, location)
            }

            (Type::UniVar(uvar_a), Type::UniVar(uvar_b)) => self
                .unification_table
                .unify_var_var(uvar_a, uvar_b)
                .map_err(|(l, r)| Error::new(ErrorKind::TypeMismatch(l, r), location)),

            (Type::UniVar(uvar), ty) | (ty, Type::UniVar(uvar)) => {
                ty
                    .occurs_check(uvar)
                    .map_err(|t| Error::new(ErrorKind::InfiniteType(t), location.clone()))?;

                self.unification_table
                    .unify_var_value(uvar, Some(ty))
                    .map_err(|(l, r)| Error::new(ErrorKind::TypeMismatch(l, r), location))
            }

            (left, right) => Err(Error::new(ErrorKind::TypeMismatch(left, right), location)),
        }
    }

    fn solve_constraints(&mut self, constraints: Vec<Constraint>) -> Result<(), Error> {
        for constraint in constraints {
            match constraint {
                Constraint::TypeEqual(left, right, location) => self.unify(left, right, location)?,
            }
        }
        Ok(())
    }

    fn typecheck_module(&mut self, module: &Module) -> Result<(), Error> {
        // Insert all top-level definitions into item_scheme_map
        for item in &module.items {
            self.type_schemes.insert(item.definition_id.clone().unwrap(), item.type_scheme.clone());
        }

        // Typecheck top-level definitions
        for item in &module.items {
            // MAYBE BUG:
            // Not exactly sure what reset_unifications does. Maybe we should just set
            // unification_table to InPlaceUnificationTable::new() instead?
            self.unification_table.reset_unifications(|_| None);

            let constraints = self.check_expr(im::HashMap::new(), &item.expr, item.type_scheme.1.clone());
            self.solve_constraints(constraints)?;
        }

        Ok(())
    }
}

pub fn typecheck_program(compiler_cache: &mut CompilerCache) -> Result<(), Error> {
    let mut typechecker = TypeChecker::new(compiler_cache);

    let mut queue = Vec::new();
    while let Some(module) = typechecker.compiler_cache.modules.pop_front() {
        typechecker.typecheck_module(&module)?;
        queue.push(module);
    }

    while let Some(module) = queue.pop() {
        typechecker.compiler_cache.modules.push_front(module);
    }

    Ok(())
}
