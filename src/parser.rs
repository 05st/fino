use std::{
    collections::{BTreeSet, VecDeque},
    fs::read_to_string,
    ops::Range, path::{Path, PathBuf}
};

use walkdir::DirEntry;

use crate::{
    ast::*, cache::CompilerCache, error::{Error, ErrorKind}, lexer::{tokenize, Token}, literal::Literal, location::{Location, Span}, types::{Type, TypeScheme, TypeVar}
};

pub enum Precedence {
    Prefix(u8),
    Infix(u8, u8),
    Postfix(u8),
}

struct Parser<'a> {
    compiler_cache: &'a mut CompilerCache,
    tokens: VecDeque<(Token, Span)>,
    previous: Option<(Token, Span)>,
    filepath: PathBuf,
    parsed_main: bool,
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

impl<'a> Parser<'a> {
    fn new(compiler_cache: &'a mut CompilerCache) -> Parser {
        Parser {
            compiler_cache,
            tokens: VecDeque::new(),
            previous: None,
            filepath: PathBuf::new(),
            parsed_main: false,
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
            Token::KwInt => Ok(Type::int()),
            Token::KwFloat => Ok(Type::float()),

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
            kind: ExprKind::Let {
                name,
                aexpr: Box::new(expr),
                body: Box::new(body),
                definition_id: None,
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
            kind: ExprKind::If {
                cond: Box::new(cond),
                texpr: Box::new(texpr),
                fexpr: Box::new(fexpr),
            },
            location: self.make_location(if_span),
        })
    }

    fn parse_lam_expr(&mut self) -> Result<Expr, Error> {
        self.expect(Token::Backslash)?;
        
        let mut params = Vec::new();
        loop {
            let (token, span) = self.next_with_span();
            match token {
                Token::LowerIdentifier(param) | Token::UpperIdentifier(param) => {
                    params.push((param, span))
                }
                Token::SmallArrow => break,
                other => return self.error(expected_one_of!(other, "identifier", "'->'"), span),
            }
        }

        let body = self.parse_expr()?;

        // Desugar function into definition with curried lambdas
        let expr = params
            .into_iter()
            .rev()
            .fold(body, |child, (param_name, span)| Expr {
                kind: ExprKind::Lam {
                    param_name,
                    body: Box::new(child),
                    param_definition_id: None,
                },
                location: self.make_location(span),
            });

        Ok(expr)
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
                    kind: ExprKind::Var {
                        name,
                        definition_id: None,
                    },
                    location: self.make_location(span),
                })
            }

            Token::LitBool(b) => Ok(Expr {
                kind: ExprKind::Lit(Literal::Bool(b)),
                location: self.make_location(span),
            }),

            Token::LitInteger(i) => Ok(Expr {
                kind: ExprKind::Lit(Literal::Int(i)),
                location: self.make_location(span),
            }),

            Token::LitFloat(f) => Ok(Expr {
                kind: ExprKind::Lit(Literal::Float(f)),
                location: self.make_location(span),
            }),

            Token::LitString(s) => Ok(Expr {
                kind: ExprKind::Lit(Literal::String(s)),
                location: self.make_location(span),
            }),

            Token::LitChar(c) => Ok(Expr {
                kind: ExprKind::Lit(Literal::Char(c)),
                location: self.make_location(span),
            }),

            Token::LitUnit => Ok(Expr {
                kind: ExprKind::Lit(Literal::Unit),
                location: self.make_location(span),
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
                        kind: ExprKind::App {
                            fun: Box::new(result),
                            arg: Box::new(arg_expr),
                        },
                        location: self.make_location(arg_span),
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
    fn unary_oper(oper: String, location: Location, expr: Expr) -> Expr {
        Expr {
            kind: ExprKind::App {
                fun: Box::new(Expr {
                    kind: ExprKind::Var {
                        definition_id: None,
                        name: Name::Unqualified(oper),
                    },
                    location: location.clone(),
                }),
                arg: Box::new(expr),
            },
            location,
        }
    }

    // Desugar binary operator application
    fn binary_oper(oper: String, location: Location, left: Expr, right: Expr) -> Expr {
        Expr {
            kind: ExprKind::App {
                fun: Box::new(Expr {
                    kind: ExprKind::App {
                        fun: Box::new(Expr {
                            kind: ExprKind::Var {
                                name: Name::Unqualified(oper),
                                definition_id: None,
                            },
                            location: location.clone(),
                        }),
                        arg: Box::new(left),
                    },
                    location: location.clone(),
                }),
                arg: Box::new(right),
            },
            location,
        }
    }

    // Parse operators with a pratt parser
    fn parse_oper_expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        let mut lhs = match self.next_with_span() {
            (Token::Operator(oper), span) => {
                if let Some(Precedence::Prefix(right_prec)) = self.compiler_cache.operator_precedences.get(&oper) {
                    let expr = self.parse_oper_expr(*right_prec)?;
                    Parser::unary_oper(oper, self.make_location(self.span()), expr)
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

            let prec = self.compiler_cache
                .operator_precedences
                .get(&oper)
                .ok_or(self.make_error(ErrorKind::UndeclaredOperator(oper.clone()), self.span()))?;

            if let Precedence::Postfix(left_prec) = *prec {
                if left_prec < min_prec {
                    break;
                }
                let (_, span) = self.next_with_span();

                lhs = Parser::unary_oper(oper, self.make_location(span), lhs);
                continue;
            }

            if let Precedence::Infix(left_prec, right_prec) = *prec {
                if left_prec < min_prec {
                    break;
                }
                let (_, span) = self.next_with_span();

                let rhs = self.parse_oper_expr(right_prec)?;
                lhs = Parser::binary_oper(oper, self.make_location(span), lhs, rhs,);
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
            Token::Backslash => self.parse_lam_expr(),
            _ => self.parse_oper_expr(0),
        }
    }

    // Parse and desugar top-level function definition
    fn parse_fn_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwFn)?;

        let span = self.span();
        let name = self.expect_identifier()?;

        // Check if we parsed a main function
        let is_main = name == "main";
        if self.parsed_main {
            return self.error(ErrorKind::MultipleMainFunctions, span);
        }
        self.parsed_main = is_main;

        self.expect(Token::Colon)?;
        let type_scheme = self.parse_type_scheme()?;

        // A main function must have a unit -> unit type
        if is_main && type_scheme.1 != Type::Fun(Box::new(Type::unit()), Box::new(Type::unit())) {
            return self.error(ErrorKind::InvalidMainFunctionType, span);
        }

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

        // Desugar function into definition with curried lambdas
        let expr = params
            .into_iter()
            .rev()
            .fold(body, |child, (param_name, span)| Expr {
                kind: ExprKind::Lam {
                    param_name,
                    body: Box::new(child),
                    param_definition_id: None,
                },
                location: self.make_location(span),
            });

        Ok(Item {
            name,
            type_scheme,
            expr,
            location: self.make_location(span),
            definition_id: None,
            is_main,
        })
    }

    // Parse top-level let-definition
    fn parse_let_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwLet)?;

        let span = self.span();
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let type_scheme = self.parse_type_scheme()?;

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::Newline)?;

        Ok(Item {
            name,
            type_scheme,
            expr,
            location: self.make_location(span),
            definition_id: None,
            // Let-definitions shouldn't be an entry point
            is_main: false,
        })
    }

    fn parse_import(&mut self) -> Result<Import, Error> {
        let span = self.span();
        self.expect(Token::KwImport)?;

        let module_path = self.parse_separated_name()?;
        self.expect(Token::Newline)?;

        Ok(Import {
            module_path,
            location: self.make_location(span),
            module_id: None,
        })
    }

    fn parse_export(&mut self) -> Result<Export, Error> {
        let span = self.span();
        self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek() {
            self.next();

            Export::Module {
                module_path: self.parse_separated_name()?,
                location: self.make_location(span),
                module_id: None,
            }
        } else {
            Export::Item {
                item_name: self.expect_identifier()?,
                location: self.make_location(span),
                definition_id: None,
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
        let type_scheme = self.parse_type_scheme()?;

        self.expect(Token::Newline)?;

        Ok(Extern {
            name,
            type_scheme,
            location: self.make_location(span),
            definition_id: None,
        })
    }

    pub fn parse_module(&mut self, tokens: Vec<(Token, Span)>, module_path: Vec<String>, filepath: PathBuf) -> Result<(), Error> {
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

        self.compiler_cache.modules.push_back(Module {
            module_path,
            externs,
            imports,
            exports,
            items,
            module_id: None,
        });
        
        Ok(())
    }
}

fn get_module_path(root: &Path, file: &Path) -> Vec<String> {
    if file == root {
        vec![String::from(
            file.with_extension("")
                .file_name()
                .expect("Failed to get file name")
                .to_str()
                .expect("Failed to convert OsStr to str"),
        )]
    } else {
        file.with_extension("")
            .to_path_buf()
            .strip_prefix(root)
            .expect("Failed to strip root path prefix")
            .components()
            .map(|c| {
                String::from(
                    c.as_os_str()
                        .to_str()
                        .expect("Failed to convert OsStr to str"),
                )
            })
            .collect()
    }
}

pub fn parse_program(compiler_cache: &mut CompilerCache, files: Vec<DirEntry>, root: &Path) -> Result<(), Error> {
    let mut lexer_output = Vec::new();
    for file in files {
        let path = file.path();
        let source = read_to_string(path).expect(format!("Failed to read file {:?}", path).as_str());

        let tokens = tokenize(&source, path.to_path_buf(), &mut compiler_cache.operator_precedences)?;
        lexer_output.push((tokens, path.to_path_buf()));
    }

    let mut parser = Parser::new(compiler_cache);
    for (tokens, path) in lexer_output {
        parser.parse_module(
            tokens,
            get_module_path(root, path.as_path()),
            path,
        )?;
    }

    Ok(())
}
