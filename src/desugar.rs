use crate::{
    ast::{Expr, ExprKind, Name, Pattern, PatternKind},
    literal::Literal,
    location::Span,
    parser::Parser,
};

#[derive(Debug)]
pub(crate) struct FnBranch {
    pub(crate) patterns: Vec<Pattern>,
    pub(crate) body: Expr,
}

#[derive(Clone)]
struct MatchRow {
    patterns: Vec<Pattern>,
    body: Expr,
    bindings: Vec<(String, String)>,
}

#[derive(Clone, PartialEq)]
struct CtorKey {
    type_name: Name,
    variant_name: String,
    field_count: usize,
}

impl<'a> Parser<'a> {
    fn literal_eq(&self, a: &Literal, b: &Literal) -> bool {
        match (a, b) {
            (Literal::Int(a), Literal::Int(b)) => a == b,
            (Literal::Float(a), Literal::Float(b)) => a == b,
            (Literal::String(a), Literal::String(b)) => a == b,
            (Literal::Char(a), Literal::Char(b)) => a == b,
            (Literal::Bool(a), Literal::Bool(b)) => a == b,
            (Literal::Unit, Literal::Unit) => true,
            _ => false,
        }
    }

    fn apply_bindings(&self, mut body: Expr, bindings: &[(String, String)], span: Span) -> Expr {
        for (name, value) in bindings.iter().rev() {
            body = Expr {
                kind: ExprKind::Let {
                    name: name.clone(),
                    aexpr: Box::new(Expr {
                        kind: ExprKind::Var {
                            name: Name::Unqualified(value.clone()),
                            definition_id: None,
                        },
                        location: self.make_location(span.clone()),
                    }),
                    body: Box::new(body),
                    definition_id: None,
                },
                location: self.make_location(span.clone()),
            };
        }
        body
    }

    pub(crate) fn build_match_tree(
        &mut self,
        param_names: &[String],
        branches: Vec<FnBranch>,
        span: Span,
    ) -> Expr {
        let rows = branches
            .into_iter()
            .map(|branch| MatchRow {
                patterns: branch.patterns,
                body: branch.body,
                bindings: Vec::new(),
            })
            .collect::<Vec<_>>();

        self.compile_match_rows(param_names.to_vec(), rows, span)
    }

    fn compile_match_rows(&mut self, vars: Vec<String>, rows: Vec<MatchRow>, span: Span) -> Expr {
        if rows.is_empty() {
            panic!("no branches to build match tree");
        }

        if vars.is_empty() {
            let row = rows.into_iter().next().unwrap();
            return self.apply_bindings(row.body, &row.bindings, span);
        }

        let current_var = vars[0].clone();
        let rest_vars = vars[1..].to_vec();

        let mut ctor_keys: Vec<CtorKey> = Vec::new();
        let mut lit_keys: Vec<Literal> = Vec::new();

        for row in &rows {
            let pattern = &row.patterns[0];
            match &pattern.kind {
                PatternKind::Variant {
                    type_name,
                    variant_name,
                    field_patterns,
                    type_definition_id: _,
                } => {
                    let key = CtorKey {
                        type_name: type_name.clone(),
                        variant_name: variant_name.clone(),
                        field_count: field_patterns.len(),
                    };
                    if !ctor_keys.iter().any(|k| k == &key) {
                        ctor_keys.push(key);
                    }
                }

                PatternKind::Lit(literal) => {
                    if !lit_keys.iter().any(|lit| self.literal_eq(lit, literal)) {
                        lit_keys.push(literal.clone());
                    }
                }

                PatternKind::Var { .. } | PatternKind::Wild => (),
            }
        }

        let mut match_branches = Vec::new();

        for key in &ctor_keys {
            let field_names = (0..key.field_count)
                .map(|_| self.new_match_name("field"))
                .collect::<Vec<_>>();

            let field_patterns = field_names
                .iter()
                .map(|name| Pattern {
                    kind: PatternKind::Var {
                        name: name.clone(),
                        definition_id: None,
                    },
                    location: self.make_location(span.clone()),
                })
                .collect::<Vec<_>>();

            let branch_pattern = Pattern {
                kind: PatternKind::Variant {
                    type_name: key.type_name.clone(),
                    variant_name: key.variant_name.clone(),
                    type_definition_id: None,
                    field_patterns,
                },
                location: self.make_location(span.clone()),
            };

            let mut branch_rows = Vec::new();
            for row in &rows {
                if let Some(specialized) =
                    self.specialize_ctor_row(row, key, &current_var, span.clone())
                {
                    branch_rows.push(specialized);
                }
            }

            let mut next_vars = field_names;
            next_vars.extend(rest_vars.iter().cloned());
            let branch_expr = self.compile_match_rows(next_vars, branch_rows, span.clone());

            match_branches.push((branch_pattern, branch_expr));
        }

        for literal in &lit_keys {
            let mut branch_rows = Vec::new();
            for row in &rows {
                if let Some(specialized) =
                    self.specialize_literal_row(row, literal, &current_var, span.clone())
                {
                    branch_rows.push(specialized);
                }
            }

            let branch_expr = self.compile_match_rows(rest_vars.clone(), branch_rows, span.clone());
            let branch_pattern = Pattern {
                kind: PatternKind::Lit(literal.clone()),
                location: self.make_location(span.clone()),
            };

            match_branches.push((branch_pattern, branch_expr));
        }

        let mut default_rows = Vec::new();
        for row in &rows {
            if let Some(specialized) = self.specialize_default_row(row, &current_var, span.clone())
            {
                default_rows.push(specialized);
            }
        }

        if !default_rows.is_empty() {
            let default_expr = self.compile_match_rows(rest_vars, default_rows, span.clone());
            match_branches.push((
                Pattern {
                    kind: PatternKind::Wild,
                    location: self.make_location(span.clone()),
                },
                default_expr,
            ));
        }

        Expr {
            kind: ExprKind::Match {
                mexpr: Box::new(Expr {
                    kind: ExprKind::Var {
                        name: Name::Unqualified(current_var),
                        definition_id: None,
                    },
                    location: self.make_location(span.clone()),
                }),
                branches: match_branches,
            },
            location: self.make_location(span),
        }
    }

    fn specialize_ctor_row(
        &self,
        row: &MatchRow,
        key: &CtorKey,
        current_var: &str,
        span: Span,
    ) -> Option<MatchRow> {
        let (first, rest) = row.patterns.split_first()?;
        match &first.kind {
            PatternKind::Variant {
                type_name,
                variant_name,
                field_patterns,
                type_definition_id: _,
            } if type_name == &key.type_name
                && variant_name == &key.variant_name
                && field_patterns.len() == key.field_count =>
            {
                let mut patterns = field_patterns.clone();
                patterns.extend_from_slice(rest);
                Some(MatchRow {
                    patterns,
                    body: row.body.clone(),
                    bindings: row.bindings.clone(),
                })
            }

            PatternKind::Wild => {
                let mut patterns = vec![
                    Pattern {
                        kind: PatternKind::Wild,
                        location: self.make_location(span.clone()),
                    };
                    key.field_count
                ];
                patterns.extend_from_slice(rest);
                Some(MatchRow {
                    patterns,
                    body: row.body.clone(),
                    bindings: row.bindings.clone(),
                })
            }

            PatternKind::Var {
                name,
                definition_id: _,
            } => {
                let mut bindings = row.bindings.clone();
                bindings.push((name.clone(), current_var.to_string()));

                let mut patterns = vec![
                    Pattern {
                        kind: PatternKind::Wild,
                        location: self.make_location(span.clone()),
                    };
                    key.field_count
                ];
                patterns.extend_from_slice(rest);
                Some(MatchRow {
                    patterns,
                    body: row.body.clone(),
                    bindings,
                })
            }

            _ => None,
        }
    }

    fn specialize_literal_row(
        &self,
        row: &MatchRow,
        literal: &Literal,
        current_var: &str,
        _span: Span,
    ) -> Option<MatchRow> {
        let (first, rest) = row.patterns.split_first()?;
        match &first.kind {
            PatternKind::Lit(lit) if self.literal_eq(lit, literal) => Some(MatchRow {
                patterns: rest.to_vec(),
                body: row.body.clone(),
                bindings: row.bindings.clone(),
            }),

            PatternKind::Wild => Some(MatchRow {
                patterns: rest.to_vec(),
                body: row.body.clone(),
                bindings: row.bindings.clone(),
            }),

            PatternKind::Var {
                name,
                definition_id: _,
            } => {
                let mut bindings = row.bindings.clone();
                bindings.push((name.clone(), current_var.to_string()));
                Some(MatchRow {
                    patterns: rest.to_vec(),
                    body: row.body.clone(),
                    bindings,
                })
            }

            _ => None,
        }
    }

    fn specialize_default_row(
        &self,
        row: &MatchRow,
        current_var: &str,
        _span: Span,
    ) -> Option<MatchRow> {
        let (first, rest) = row.patterns.split_first()?;
        match &first.kind {
            PatternKind::Wild => Some(MatchRow {
                patterns: rest.to_vec(),
                body: row.body.clone(),
                bindings: row.bindings.clone(),
            }),

            PatternKind::Var {
                name,
                definition_id: _,
            } => {
                let mut bindings = row.bindings.clone();
                bindings.push((name.clone(), current_var.to_string()));
                Some(MatchRow {
                    patterns: rest.to_vec(),
                    body: row.body.clone(),
                    bindings,
                })
            }

            _ => None,
        }
    }
}
