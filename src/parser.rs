use std::{collections::{BTreeSet, HashMap, VecDeque}, ops::Range, path::PathBuf};

use crate::{
    ast::*,
    error::{Error, ErrorKind},
    lexer::Token,
    types::{Type, TypeScheme, TypeVar},
};

pub enum Precedence {
    Prefix(u8),
    Infix(u8, u8),
    Postfix(u8),
}

pub struct Parser {
    tokens: VecDeque<(Token, Span)>,
    previous: Option<(Token, Span)>,
    filepath: PathBuf,

    operator_map: HashMap<String, Precedence>,

    expr_id_count: usize,
    module_id_count: usize,
}

// Utility macro to create ErrorKind::ExpectedOneOf errors from string literals
// without having to type String::from(...).
macro_rules! expected_one_of {
    ( $t:expr, $( $x:literal ),+ ) => {
        ErrorKind::ExpectedOneOf(
            $t,
            vec![
            $(
                String::from($x),
            )+
            ]
        )
    };
}

impl Parser {
    pub fn new(operator_map: HashMap<String, Precedence>) -> Parser {
        Parser {
            tokens: VecDeque::new(),
            previous: None,
            filepath: PathBuf::new(),
            operator_map,
            expr_id_count: 0,
            module_id_count: 0,
        }
    }

    // Puts previous token into token stream. Can only be called once before another
    // token needs to be consumed since we only store the previous token, not a
    // history of previous tokens. A bit of a hacky solution to parsing function
    // applications and qualified variable names.
    fn restore(&mut self) {
        // We assume restore() is only called when self.previous is Some(expr),
        // otherwise .unwrap() will panic
        self.tokens.push_front(self.previous.to_owned().unwrap());
        self.previous = None;
    }

    fn new_expr_id(&mut self) -> ExprId {
        let expr_id = ExprId(self.expr_id_count);
        self.expr_id_count += 1;
        expr_id
    }

    // Insert node id to location mapping into compiler cache
    fn make_location(&self, span: Span) -> Location {
        Location::new(self.filepath.clone(), span)
    }

    // TODO:
    // Maybe define some macros instead
    fn make_error(&self, error: ErrorKind, span: Span) -> Error {
        Error::new(error, Location::new(self.filepath.clone(), span))
    }

    fn error<T>(&self, error: ErrorKind, span: Span) -> Result<T, Error> {
        Err(self.make_error(error, span))
    }

    // Make span for eof token by using the end point of the span of the previous token
    fn make_eof_span(&self) -> Span {
        // If self.previous was None, then the input was empty, so we can return an
        // empty span at 0 bytes.
        self.previous
            .as_ref()
            .map(|p| Range { start: p.1.end, end: p.1.end })
            .unwrap_or(Range { start: 0, end: 0 })
    }

    // Advance token stream and return popped (token, span)
    fn next_with_span(&mut self) -> (Token, Span) {
        let result = self
            .tokens
            .pop_front()
            .unwrap_or((Token::Eof, self.make_eof_span()));

        self.previous = Some(result.clone());
        result
    }

    // Advance token stream and return popped token
    fn next(&mut self) -> Token {
        self.next_with_span().0
    }

    // Peek current token
    fn peek(&self) -> &Token {
        self.tokens
            .front()
            .map(|t| &t.0)
            .unwrap_or(&Token::Eof)
    }

    // Span of current token
    fn span(&self) -> Span {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .unwrap_or(self.make_eof_span())
    }

    // Consume specific token
    fn expect(&mut self, expected: Token) -> Result<Token, Error> {
        let (current, span) = self.next_with_span();

        if current == expected {
            Ok(current)
        } else {
            self.error(ErrorKind::ExpectedOneOf(current, vec![expected.to_string()]), span)
        }
    }

    // Consume any identifier (including parenthesized operators)
    fn expect_identifier(&mut self) -> Result<String, Error> {
        match self.next_with_span() {
            (Token::LowerIdentifier(ident) | Token::UpperIdentifier(ident), _) => Ok(ident),
            (Token::LeftParen, _) => {
                let (token, span) = self.next_with_span();
                if let Token::Operator(oper) = token {
                    self.expect(Token::RightParen)?;
                    Ok(oper)
                } else {
                    self.error(expected_one_of!(token, "operator"), span)
                }
            }
            (other, span) => self.error(expected_one_of!(other, "identifier"), span),
        }
    }

    // Skip zero or more newlines
    fn skip_newlines(&mut self) {
        while let Token::Newline = self.peek() {
            self.next();
        }
    }

    fn parse_separated_name(&mut self) -> Result<Vec<String>, Error> {
        let mut result = Vec::new();
        loop {
            result.push(self.expect_identifier()?);
            match self.peek() {
                Token::Dot => self.next(),
                _ => break,
            };
        }
        Ok(result)
    }

    fn parse_name(&mut self) -> Result<Name, Error> {
        let mut name = self.parse_separated_name()?;
        if name.len() == 1 {
            // parse_separated_name() should have encounted at least one identifier
            Ok(Name::Unqualified(name.pop().unwrap()))
        } else {
            Ok(Name::Qualified(name))
        }
    }

    // Parse type constant or type variable
    fn parse_base_type(&mut self) -> Result<Type, Error> {
        let (token, span) = self.next_with_span();

        match token {
            Token::KwUnit => Ok(Type::unit()),
            Token::KwBool => Ok(Type::bool()),
            Token::KwChar => Ok(Type::char()),
            Token::KwStr => Ok(Type::str()),
            Token::KwI32 => Ok(Type::i32()),
            Token::KwI64 => Ok(Type::i64()),
            Token::KwU32 => Ok(Type::u32()),
            Token::KwU64 => Ok(Type::u64()),
            Token::KwF32 => Ok(Type::f32()),
            Token::KwF64 => Ok(Type::f64()),

            Token::UpperIdentifier(type_name) => Ok(Type::Const(type_name)),
            Token::LowerIdentifier(type_var) => Ok(Type::Var(TypeVar(type_var))),

            other => self.error(expected_one_of!(other, "type", "type variable"), span),
        }
    }

    // Parse type
    fn parse_type(&mut self) -> Result<Type, Error> {
        let base_type = self.parse_base_type()?;

        match self.peek() {
            Token::SmallArrow => {
                self.next();
                Ok(Type::Fun(Box::new(base_type), Box::new(self.parse_type()?)))
            }
            _ => Ok(base_type),
        }
    }

    fn parse_type_scheme(&mut self) -> Result<TypeScheme, Error> {
        let ty = self.parse_type()?;
        let mut type_vars = BTreeSet::new();
        ty.extract_type_vars(&mut type_vars);
        Ok(TypeScheme(type_vars, ty))
    }

    fn parse_let_expr(&mut self) -> Result<Expr, Error> {
        let let_span = self.span();
        self.expect(Token::KwLet)?;

        let name = self.expect_identifier()?;
        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::KwIn)?;
        let body = self.parse_expr()?;

        Ok(Expr {
            location: self.make_location(let_span),
            expr_id: self.new_expr_id(),
            kind: ExprKind::Let {
                def_id: DefId::default(),
                name,
                expr: Box::new(expr),
                body: Box::new(body),
            },
        })
    }

    // Parse if expression
    fn parse_if_expr(&mut self) -> Result<Expr, Error> {
        let if_span = self.span();
        self.expect(Token::KwIf)?;
        let cond = self.parse_expr()?;

        self.expect(Token::KwThen)?;
        let texpr = self.parse_expr()?;

        self.expect(Token::KwElse)?;
        let fexpr = self.parse_expr()?;

        Ok(Expr {
            location: self.make_location(if_span),
            expr_id: self.new_expr_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                texpr: Box::new(texpr),
                fexpr: Box::new(fexpr),
            },
        })
    }

    // Parse expression atom (i.e. literal, variable, parenthesized expression)
    // The error type contains a bool which is true if zero tokens were matched
    // which is useful for parse_app() to know if it should go through and 
    // report the error (if more than zero were matched) or ignore it (if zero
    // were matched).
    fn parse_atom(&mut self) -> Result<Expr, (bool, Error)> {
        let (token, span) = self.next_with_span();
        match token {
            // Parse parenthesized expression
            Token::LeftParen => {
                let expr = self.parse_expr().map_err(|e| (false, e))?;
                self.expect(Token::RightParen).map_err(|e| (false, e))?;
                Ok(expr)
            }

            Token::UpperIdentifier(ident) | Token::LowerIdentifier(ident) => {
                // Parse qualified name if we see '::' token
                let name = if let Token::Dot = self.peek() {
                    // Restore identifier token for parse_name()
                    self.restore();
                    self.parse_name().map_err(|e| (false, e))?
                } else {
                    Name::Unqualified(ident)
                };

                Ok(Expr {
                    location: self.make_location(span),
                    expr_id: self.new_expr_id(),
                    kind: ExprKind::Var {
                        def_id: DefId::default(),
                        name: name,
                    },
                })
            }

            Token::LitBool(b) => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::Bool(b)),
            }),

            Token::LitInteger(i) => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::Int(i)),
            }),

            Token::LitFloat(f) => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::Float(f)),
            }),

            Token::LitString(s) => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::String(s)),
            }),

            Token::LitChar(c) => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::Char(c)),
            }),

            Token::LitUnit => Ok(Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lit(Lit::Unit),
            }),

            other => Err((true, self.make_error(expected_one_of!(other, "identifier", "literal", "'('"), span)))
        }
    }

    // Parse function application
    fn parse_app(&mut self) -> Result<Expr, Error> {
        let mut result = self.parse_atom().map_err(|atom_err| atom_err.1)?;
        
        loop {
            // Need to get span for possible arg expr before parse_atom() is called because
            // it consumes the token, so we can't get the span afterwards.
            let arg_span = self.span();
            match self.parse_atom() {
                Ok(arg_expr) => {
                    result = Expr {
                        location: self.make_location(arg_span),
                        expr_id: self.new_expr_id(),
                        kind: ExprKind::App {
                            fun: Box::new(result),
                            arg: Box::new(arg_expr),
                        }
                    };
                }
                Err((false, err)) => return Err(err),
                // When parsing atom fails without matching any token, restore token consumed
                // by parse_atom()
                Err((true, _)) => {
                    self.restore();
                    break;
                }
            }
        }

        Ok(result)
    }

    // Desugar unary operator application
    fn unary_oper(&mut self, oper: String, location: Location, expr: Expr) -> Expr {
        Expr {
            location: location.clone(),
            expr_id: self.new_expr_id(),
            kind: ExprKind::App {
                fun: Box::new(Expr {
                    location,
                    expr_id: self.new_expr_id(),
                    kind: ExprKind::Var {
                        def_id: DefId::default(),
                        name: Name::Unqualified(oper),
                    },
                }),
                arg: Box::new(expr),
            }
        }
    }

    // Desugar binary operator application
    fn binary_oper(&mut self, oper: String, location: Location, left: Expr, right: Expr) -> Expr {
        Expr {
            location: location.clone(),
            expr_id: self.new_expr_id(),
            kind: ExprKind::App {
                fun: Box::new(Expr {
                    location: location.clone(),
                    expr_id: self.new_expr_id(),
                    kind: ExprKind::App {
                        fun: Box::new(Expr {
                            location,
                            expr_id: self.new_expr_id(),
                            kind: ExprKind::Var {
                                def_id: DefId::default(),
                                name: Name::Unqualified(oper),
                            },
                        }),
                        arg: Box::new(left),
                    },
                }),
                arg: Box::new(right),
            },
        }
    }

    // Parse operators with a pratt parser
    fn parse_oper_expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        let mut lhs = match self.next_with_span() {
            (Token::Operator(oper), span) => {
                if let Some(Precedence::Prefix(right_prec)) = self.operator_map.get(&oper) {
                    let expr = self.parse_oper_expr(*right_prec)?;
                    self.unary_oper(oper, self.make_location(self.span()), expr)
                } else {
                    return self.error(ErrorKind::InvalidPrefixOperator(oper), span);
                }
            }
            _ => {
                self.restore();
                self.parse_app()?
            }
        };

        loop {
            let oper = match self.peek() {
                // Don't really need to clone here
                Token::Operator(oper) => oper.clone(),
                Token::SpacedDot => String::from("."),
                _ => break,
            };

            let prec = self.operator_map
                .get(&oper)
                .ok_or(self.make_error(ErrorKind::UndeclaredOperator(oper.clone()), self.span()))?;

            if let Precedence::Postfix(left_prec) = *prec {
                if left_prec < min_prec {
                    break;
                }
                let (_, span) = self.next_with_span();

                lhs = self.unary_oper(oper, self.make_location(span), lhs);
                continue;
            }

            if let Precedence::Infix(left_prec, right_prec) = *prec {
                if left_prec < min_prec {
                    break;
                }
                let (_, span) = self.next_with_span();

                let rhs = self.parse_oper_expr(right_prec)?;
                lhs = self.binary_oper(oper, self.make_location(span), lhs, rhs,);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    // Parse expression
    fn parse_expr(&mut self) -> Result<Expr, Error> {
        match self.peek() {
            Token::KwLet => self.parse_let_expr(),
            Token::KwIf => self.parse_if_expr(),
            _ => self.parse_oper_expr(0),
        }
    }

    // Parse and desugar top-level function definition
    fn parse_fn_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwFn)?;

        let span = self.span();
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let scheme = self.parse_type_scheme()?;

        self.expect(Token::Indent)?;

        // TODO:
        // Parse and desugar multiple pattern matching branches

        let mut params = Vec::new();
        loop {
            let (token, span) = self.next_with_span();
            match token {
                Token::UpperIdentifier(param) | Token::LowerIdentifier(param) => {
                    params.push((param, span))
                }
                Token::Equal => break,
                other => return self.error(expected_one_of!(other, "identifier", "'='"), span),
            }
        }

        let body = self.parse_expr()?;
        self.expect(Token::Dedent)?;

        // Reverse for proper fold direction
        params.reverse();

        // Desugar function into definition with curried lambdas
        let expr = params
            .into_iter()
            .fold(body, |child, (param_name, span)| Expr {
                location: self.make_location(span),
                expr_id: self.new_expr_id(),
                kind: ExprKind::Lam {
                    param_def_id: DefId::default(),
                    param: param_name,
                    body: Box::new(child),
                },
            });

        Ok(Item {
            location: self.make_location(span),
            name,
            def_id: DefId::default(),
            scheme,
            expr,
        })
    }

    // Parse top-level let-definition
    fn parse_let_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwLet)?;

        let span = self.span();
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let scheme = self.parse_type_scheme()?;

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::Newline)?;

        Ok(Item {
            location: self.make_location(span),
            name,
            def_id: DefId::default(),
            scheme,
            expr,
        })
    }

    fn parse_import(&mut self) -> Result<Import, Error> {
        let span = self.span();
        self.expect(Token::KwImport)?;

        let module_name = self.parse_separated_name()?;
        self.expect(Token::Newline)?;

        Ok(Import {
            location: self.make_location(span),
            module_name,
        })
    }

    fn parse_export(&mut self) -> Result<Export, Error> {
        let span = self.span();
        self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek() {
            self.next();

            Export::Module {
                location: self.make_location(span),
                module_name: self.parse_separated_name()?,
            }
        } else {
            Export::Item {
                location: self.make_location(span),
                def_id: DefId::default(),
                item_name: self.expect_identifier()?,
            }
        };

        self.expect(Token::Newline)?;

        Ok(export)
    }

    fn parse_extern(&mut self) -> Result<Extern, Error> {
        let span = self.span();
        self.expect(Token::KwExtern)?;
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let scheme = self.parse_type_scheme()?;

        self.expect(Token::Newline)?;

        Ok(Extern {
            location: self.make_location(span),
            def_id: DefId::default(),
            name,
            scheme,
        })
    }

    pub fn parse_module(&mut self, tokens: Vec<(Token, Span)>, module_name: Vec<String>, filepath: PathBuf) -> Result<Module, Error> {
        self.tokens = VecDeque::from(tokens);
        self.filepath = filepath;

        let mut externs = Vec::new();
        let mut imports = Vec::new();
        let mut exports = Vec::new();
        let mut items = Vec::new();

        loop {
            self.skip_newlines();

            if !self.tokens.is_empty() {
                match self.peek() {
                    Token::KwExtern => externs.push(self.parse_extern()?),
                    Token::KwImport => imports.push(self.parse_import()?),
                    Token::KwExport => exports.push(self.parse_export()?),
                    Token::KwFn => items.push(self.parse_fn_item()?),
                    Token::KwLet => items.push(self.parse_let_item()?),

                    _ => {
                        let (token, span) = self.next_with_span();
                        return self.error(
                            expected_one_of!(token, "'import'", "'export'", "top-level definition"),
                            span,
                        );
                    }
                }
            } else {
                break;
            }
        }

        let module_id = ModuleId(self.module_id_count);
        self.module_id_count += 1;

        Ok(Module {
            module_id,
            module_name,
            externs,
            imports,
            exports,
            items,
        })
    }
}
