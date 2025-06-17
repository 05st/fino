use std::collections::{BTreeSet, HashMap};

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
    type_constrs: HashMap<(DefinitionId, String), TypeScheme>,
}

impl<'a> TypeChecker<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> TypeChecker {
        TypeChecker {
            compiler_cache,
            unification_table: InPlaceUnificationTable::new(),
            type_schemes: HashMap::new(),
            type_constrs: HashMap::new(),
        }
    }

    fn fresh_uni_var(&mut self) -> TypeUniVar {
        self.unification_table.new_key(None)
    }

    // Infer (synthesize) expression type. Only one of infer_expr or check_expr
    // should ever be called on an expression
    fn infer_expr(
        &mut self,
        env: im::HashMap<DefinitionId, Type>,
        expr: &Expr,
    ) -> (Vec<Constraint>, Type) {
        let (constraints, expr_type) = match &expr.kind {
            ExprKind::Lit(literal) => (
                Vec::new(),
                match literal {
                    Literal::Int(_) => Type::int(),
                    Literal::Float(_) => Type::float(),
                    Literal::String(_) => Type::str(),
                    Literal::Char(_) => Type::char(),
                    Literal::Bool(_) => Type::bool(),
                    Literal::Unit => Type::unit(),
                },
            ),

            ExprKind::Var {
                name: _,
                definition_id,
            } => {
                // Variable is guaranteed to be defined after name resolution, either in the
                // local scope or as a toplevel definition.
                let var_type = match env.get(&definition_id.as_ref().unwrap()) {
                    Some(ty) => ty.clone(),
                    None => {
                        // Instantiate type scheme
                        let scheme = &self.type_schemes[&definition_id.as_ref().unwrap()].clone();
                        let subst = scheme
                            .0
                            .iter()
                            .map(|tvar| {
                                (Type::Var(tvar.clone()), Type::UniVar(self.fresh_uni_var()))
                            })
                            .collect::<HashMap<_, _>>();
                        scheme.1.substitute(&subst)
                    }
                };

                (Vec::new(), var_type)
            }

            ExprKind::TypeConstr {
                type_name: _,
                constr_name,
                type_definition_id,
            } => {
                // how can we avoid these clones?
                let constr_scheme = &self.type_constrs[&(type_definition_id.as_ref().unwrap().clone(), constr_name.clone())].clone();
                let subst = constr_scheme
                    .0
                    .iter()
                    .map(|tvar| {
                        (Type::Var(tvar.clone()), Type::UniVar(self.fresh_uni_var()))
                    })
                    .collect::<HashMap<_, _>>();
                (Vec::new(), constr_scheme.1.substitute(&subst))
            }

            ExprKind::Lam {
                param_name: _,
                body,
                param_definition_id,
            } => {
                let param_uvar = self.fresh_uni_var();
                let new_env = env.update(
                    param_definition_id.clone().unwrap(),
                    Type::UniVar(param_uvar),
                );

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

            ExprKind::Extern {
                fun_name: _,
                args,
                prim_type,
            } => {
                let mut args_constraints = Vec::new();
                for arg in args {
                    let (mut constraints, _) = self.infer_expr(env.clone(), arg);
                    args_constraints.append(&mut constraints);
                }

                (args_constraints, Type::Prim(prim_type.clone()))
            }

            ExprKind::Let {
                name: _,
                aexpr,
                body,
                definition_id,
            } => {
                let (expr_constraints, def_type) = self.infer_expr(env.clone(), &aexpr);
                let new_env = env.update(definition_id.clone().unwrap(), def_type);
                let (body_constraints, body_type) = self.infer_expr(new_env, &body);

                (
                    expr_constraints
                        .into_iter()
                        .chain(body_constraints)
                        .collect(),
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
    fn check_expr(
        &mut self,
        env: im::HashMap<DefinitionId, Type>,
        expr: &Expr,
        expected_type: Type,
    ) -> Vec<Constraint> {
        match (&expr.kind, expected_type) {
            (ExprKind::Lit(Literal::Int(_)), t) if t == Type::int() => Vec::new(),
            (ExprKind::Lit(Literal::Float(_)), t) if t == Type::float() => Vec::new(),
            (ExprKind::Lit(Literal::String(_)), t) if t == Type::str() => Vec::new(),
            (ExprKind::Lit(Literal::Char(_)), t) if t == Type::char() => Vec::new(),
            (ExprKind::Lit(Literal::Bool(_)), t) if t == Type::bool() => Vec::new(),
            (ExprKind::Lit(Literal::Unit), t) if t == Type::unit() => Vec::new(),

            (
                ExprKind::Lam {
                    param_name: _,
                    body,
                    param_definition_id,
                },
                Type::Fun(param_type, ret_type),
            ) => {
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
            Type::Var(_) | Type::Prim(_) | Type::Const { .. } => unnorm,

            Type::UniVar(uvar) => match self.unification_table.probe_value(uvar) {
                Some(bound_type) => self.normalize_type(bound_type),
                None => unnorm,
            },

            Type::App(unnorm_base, unnorm_arg) => {
                let base = self.normalize_type(*unnorm_base);
                let arg = self.normalize_type(*unnorm_arg);
                Type::App(Box::new(base), Box::new(arg))
            }

            Type::Fun(unnorm_param_type, unnorm_ret_type) => {
                let param_type = self.normalize_type(*unnorm_param_type);
                let ret_type = self.normalize_type(*unnorm_ret_type);
                Type::Fun(Box::new(param_type), Box::new(ret_type))
            }
        }
    }

    fn unify(
        &mut self,
        unnorm_left: Type,
        unnorm_right: Type,
        location: Location,
    ) -> Result<(), Error> {
        let left = self.normalize_type(unnorm_left);
        let right = self.normalize_type(unnorm_right);

        match (left, right) {
            (Type::Var(var_a), Type::Var(var_b)) if var_a == var_b => Ok(()),

            (Type::Prim(prim_a), Type::Prim(prim_b)) if prim_a == prim_b => Ok(()),

            // Compare definition ids for type constants, which should be Some(id) at this point
            (
                Type::Const {
                    name: _,
                    location: _,
                    definition_id: a,
                },
                Type::Const {
                    name: _,
                    location: _,
                    definition_id: b,
                },
            ) if *a.as_ref().unwrap() == *b.as_ref().unwrap() => Ok(()),

            (Type::App(base_a, arg_a), Type::App(base_b, arg_b)) => {
                self.unify(*base_a, *base_b, location.clone())?;
                self.unify(*arg_a, *arg_b, location)
            }

            (Type::Fun(param_a, ret_a), Type::Fun(param_b, ret_b)) => {
                self.unify(*param_a, *param_b, location.clone())?;
                self.unify(*ret_a, *ret_b, location)
            }

            (Type::UniVar(uvar_a), Type::UniVar(uvar_b)) => self
                .unification_table
                .unify_var_var(uvar_a, uvar_b)
                .map_err(|(l, r)| Error::new(ErrorKind::TypeMismatch(l, r), location)),

            (Type::UniVar(uvar), ty) | (ty, Type::UniVar(uvar)) => {
                ty.occurs_check(uvar)
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
                Constraint::TypeEqual(left, right, location) => {
                    self.unify(left, right, location)?
                }
            }
        }
        Ok(())
    }

    fn typecheck_module(&mut self, module: &Module) -> Result<(), Error> {
        // Declare toplevels
        for toplevel in &module.toplevels {
            match &toplevel.kind {
                // Insert all let-definitions into type_schemes
                ToplevelKind::Let {
                    type_scheme,
                    expr: _,
                    is_main: _,
                } => {
                    self.type_schemes
                        .insert(toplevel.definition_id.clone().unwrap(), type_scheme.clone());
                }

                // Setup type schemes for type_constrs
                ToplevelKind::Type {
                    type_vars,
                    constructors,
                } => {
                    for constr in constructors {
                        let constr_type = constr.params.iter().rev().fold(
                            Type::Const {
                                // Maybe we should use the qualified name instead?
                                name: Name::Unqualified(toplevel.name.clone()),
                                location: toplevel.location.clone(),
                                definition_id: toplevel.definition_id.clone(),
                            },
                            |child, t| Type::Fun(Box::new(t.clone()), Box::new(child)),
                        );

                        let mut extracted_type_vars = BTreeSet::new();
                        constr_type.extract_type_vars(&mut extracted_type_vars);
                        
                        // Ensure all type variables were closed over
                        for type_var in extracted_type_vars.iter() {
                            if !type_vars.contains(&type_var) {
                                // TODO: Use better location for type variable
                                return Err(Error::new(ErrorKind::UndefinedTypeVariable(type_var.clone()), toplevel.location.clone()));
                            }
                        }

                        self.type_constrs.insert(
                            (toplevel.definition_id.clone().unwrap(), constr.name.clone()),
                            TypeScheme(extracted_type_vars, constr_type)
                        );
                    }
                }
            }
        }

        // Typecheck toplevels
        for toplevel in &module.toplevels {
            match &toplevel.kind {
                ToplevelKind::Let {
                    type_scheme,
                    expr,
                    is_main: _,
                } => {
                    self.unification_table = InPlaceUnificationTable::new();

                    let constraints =
                        self.check_expr(im::HashMap::new(), expr, type_scheme.1.clone());
                    self.solve_constraints(constraints)?;
                }

                ToplevelKind::Type {
                    type_vars: _,
                    constructors: _,
                } => { } // Shouldn't have to do anything (?)
            }
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
