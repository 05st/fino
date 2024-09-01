use std::{
    collections::{BTreeSet, VecDeque},
    fs::read_to_string,
    ops::Range, path::{Path, PathBuf}
};

use walkdir::DirEntry;

use crate::{
    ast::*,
    cache::CompilerCache,
    error::{Error, ErrorKind},
    lexer::{tokenize, Token},
    literal::Literal,
    location::{Location, Span},
    types::{Type, TypeScheme, TypeVar}
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
    line_start: usize,
    block_stack: Vec<usize>,
}

// Utility macro to create ErrorKind::ExpectedOneOf errors from string literals
// without having to type String::from(...).
macro_rules! expected_one_of {
    ( $t:expr, $( $x:literal ),+ ) => {
        ErrorKind::ExpectedOneOf($t, vec![$(String::from($x),)+])
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
            line_start: 0,
            block_stack: Vec::new(),
        }
    }

    // Insert node id to location mapping into compiler cache
    fn make_location(&self, span: Span) -> Location {
        Location::new(self.filepath.clone(), span)
    }

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

    // Pop current token
    fn pop(&mut self) -> (Token, Span) {
        let result = self.tokens
            .pop_front()
            .unwrap_or((Token::Eof, self.make_eof_span()));
        self.previous = Some(result.clone());
        result
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
    // Skip zero or more newlines
    fn skip_newlines(&mut self) {
        while let Token::Newline = self.peek() {
            let (_, span) = self.pop();
            self.line_start = span.end;
        }
    }

    // Advance token stream and return popped (token, span)
    fn next_with_span(&mut self) -> Result<(Token, Span), Error> {
        let result = self.pop();

        // Ensure column position meets current block threshold
        let column = result.1.start - self.line_start;
        let thresh = *self.block_stack.first().unwrap_or(&0);
        if column < thresh {
            return self.error(ErrorKind::InvalidIndentation(column, thresh), result.1);
        }

        self.skip_newlines();

        Ok(result)
    }

    // Advance token stream and return popped token
    fn next(&mut self) -> Result<Token, Error> {
        Ok(self.next_with_span()?.0)
    }

    // Begins a new indented block, with indentation level of the current token,
    // which must be greater than of the previous block
    fn begin_block(&mut self) -> Result<(), Error> {
        let span = self.span();

        let column = span.start - self.line_start;
        let prev_col = *self.block_stack.first().unwrap_or(&0);
        if column <= prev_col {
            return self.error(ErrorKind::InvalidIndentation(column, prev_col), span);
        }

        self.block_stack.push(column);

        Ok(())
    }

    // Pops the current block
    fn end_block(&mut self) {
        self.block_stack.pop();
    }

    // Consume specific token
    fn expect(&mut self, expected: Token) -> Result<Span, Error> {
        let (current, span) = self.next_with_span()?;

        if current == expected {
            Ok(span)
        } else {
            self.error(ErrorKind::ExpectedOneOf(current, vec![expected.to_string()]), span)
        }
    }

    // Consume any identifier (including parenthesized operators)
    fn expect_identifier(&mut self) -> Result<(String, Span), Error> {
        match self.next_with_span()? {
            (Token::LowerIdentifier(ident) | Token::UpperIdentifier(ident), span) => Ok((ident, span)),
            (Token::LeftParen, _) => {
                let (token, span) = self.next_with_span()?;
                if let Token::Operator(oper) = token {
                    self.expect(Token::RightParen)?;
                    Ok((oper, span))
                } else {
                    self.error(expected_one_of!(token, "operator"), span)
                }
            }
            (other, span) => self.error(expected_one_of!(other, "identifier"), span),
        }
    }

    fn parse_separated_name(&mut self) -> Result<Vec<String>, Error> {
        let mut result = Vec::new();
        loop {
            result.push(self.expect_identifier()?.0);
            match self.peek() {
                Token::Dot => self.next()?,
                _ => break,
            };
        }
        Ok(result)
    }

    // Parse type constant or type variable
    fn parse_base_type(&mut self) -> Result<Type, Error> {
        let (token, span) = self.next_with_span()?;
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
                self.next()?;
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

    fn parse_extern_expr(&mut self) -> Result<Expr, Error> {
        let extern_span = self.expect(Token::KwExtern)?;

        let fun_name = match self.next_with_span()? {
            (Token::ExternIdentifier(ident), _) => ident,
            (other, span) => return self.error(expected_one_of!(other, "extern identifier"), span),
        };

        // Parse comma separated args
        self.expect(Token::LeftParen)?;
        let mut args = Vec::new();
        loop {
            if let Token::RightParen = self.peek() {
                break;
            }
            let arg_expr = self.parse_expr()?;
            args.push(arg_expr);
            if let Token::RightParen = self.peek() {
                break;
            } else {
                self.expect(Token::Comma)?;
            }
        }
        self.expect(Token::RightParen)?;

        self.expect(Token::Colon)?;
        let prim_type = self.parse_base_type()?;

        // TODO: Assert prim_type is actually a primitive type

        Ok(Expr {
            kind: ExprKind::Extern {
                fun_name,
                args,
                prim_type,
            },
            location: self.make_location(extern_span),
        })
    }

    fn parse_let_expr(&mut self) -> Result<Expr, Error> {
        self.expect(Token::KwLet)?;
        let (name, span) = self.expect_identifier()?;

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::KwIn)?;
        let body = self.parse_expr()?;

        Ok(Expr {
            location: self.make_location(span),
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
        let if_span = self.expect(Token::KwIf)?;
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
            let (token, span) = self.next_with_span()?;
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

    // Based on the token, determines if it could be the start of an an atom
    // Used by parse_app()
    fn could_be_atom(token: &Token) -> bool {
        match token {
              Token::LeftParen
            | Token::UpperIdentifier(_)
            | Token::LowerIdentifier(_)
            | Token::LitBool(_)
            | Token::LitInteger(_)
            | Token::LitFloat(_)
            | Token::LitString(_)
            | Token::LitChar(_)
            | Token::LitUnit => true,
            _ => false
        }
    }

    // Parse expression atom (i.e. literal, variable, parenthesized expression)
    // The error type contains a bool which is true if zero tokens were matched
    // which is useful for parse_app() to know if it should go through and 
    // report the error (if more than zero were matched) or ignore it (if zero
    // were matched).
    fn parse_atom(&mut self) -> Result<Expr, Error> {
        let (token, span) = self.next_with_span()?;
        match token {
            // Parse parenthesized expression
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }

            Token::UpperIdentifier(ident) | Token::LowerIdentifier(ident) => {
                let mut parts = vec![ident.clone()];

                loop {
                    if let Token::Dot = self.peek() {
                        self.next()?;
                        parts.push(self.expect_identifier()?.0);
                    } else {
                        break;
                    }
                }

                let name = if parts.len() == 1 {
                    Name::Unqualified(parts.pop().unwrap())
                } else {
                    Name::Qualified(parts)
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
                kind: ExprKind::Lit(Literal::String(s.clone())),
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

            other => Err(self.make_error(expected_one_of!(other.clone(), "identifier", "literal", "'('"), span)),
        }
    }

    // Parse function application
    fn parse_app(&mut self) -> Result<Expr, Error> {
        let mut result = self.parse_atom()?;
        
        loop {
            if Parser::could_be_atom(self.peek()) {
                // Need to get span for possible arg expr before parse_atom() is called because
                // it consumes the token, so we can't get the span afterwards.
                let arg_span = self.span();
                let arg = self.parse_atom()?;
                result = Expr {
                    kind: ExprKind::App {
                        fun: Box::new(result),
                        arg: Box::new(arg),
                    },
                    location: self.make_location(arg_span),
                };
            } else {
                break;
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
        let token = self.peek().clone();

        let mut lhs = match token {
            Token::Operator(oper) => {
                // Advance if token is an operator
                let (_, span) = self.next_with_span()?;

                if let Some(Precedence::Prefix(right_prec)) = self.compiler_cache.operator_precedences.get(&oper) {
                    let expr = self.parse_oper_expr(*right_prec)?;
                    Parser::unary_oper(oper.clone(), self.make_location(self.span()), expr)
                } else {
                    return self.error(ErrorKind::InvalidPrefixOperator(oper.clone()), span);
                }
            }
            _ => self.parse_app()?,
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
                let (_, span) = self.next_with_span()?;

                lhs = Parser::unary_oper(oper, self.make_location(span), lhs);
                continue;
            }

            if let Precedence::Infix(left_prec, right_prec) = *prec {
                if left_prec < min_prec {
                    break;
                }
                let (_, span) = self.next_with_span()?;

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
            Token::KwExtern => self.parse_extern_expr(),
            Token::KwLet => self.parse_let_expr(),
            Token::KwIf => self.parse_if_expr(),
            Token::Backslash => self.parse_lam_expr(),
            _ => self.parse_oper_expr(0),
        }
    }

    // Parse and desugar top-level function definition
    fn parse_fn_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwFn)?;
        let (name, span) = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let type_scheme = self.parse_type_scheme()?;

        self.begin_block()?;

        // TODO:
        // Parse and desugar multiple pattern matching branches

        let mut params = Vec::new();
        loop {
            let (token, span) = self.next_with_span()?;
            match token {
                Token::UpperIdentifier(param) | Token::LowerIdentifier(param) => {
                    params.push((param, span))
                }
                Token::Equal => break,
                other => return self.error(expected_one_of!(other, "identifier", "'='"), span),
            }
        }

        let body = self.parse_expr()?;

        self.end_block();

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
            // Fn definitions shouldn't be entry points
            is_main: false,
        })
    }

    // Parse top-level let-definition
    fn parse_let_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwLet)?;
        let (name, span) = self.expect_identifier()?;

        // Check if we parsed the main definition
        let is_main = name == "main";
        if is_main && self.parsed_main {
            return self.error(ErrorKind::MultipleMainDefinitions, span);
        }
        self.parsed_main = is_main;

        self.expect(Token::Colon)?;
        let type_scheme = self.parse_type_scheme()?;

        // Main definition must have a unit type
        if is_main && type_scheme.1 != Type::unit() {
            return self.error(ErrorKind::InvalidMainDefinitionType, span);
        }

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        Ok(Item {
            name,
            type_scheme,
            expr,
            location: self.make_location(span),
            definition_id: None,
            is_main,
        })
    }

    fn parse_import(&mut self) -> Result<Import, Error> {
        let span = self.expect(Token::KwImport)?;

        let module_path = self.parse_separated_name()?;

        Ok(Import {
            module_path,
            location: self.make_location(span),
            module_id: None,
        })
    }

    fn parse_export(&mut self) -> Result<Export, Error> {
        let span = self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek() {
            self.next()?;

            Export::Module {
                module_path: self.parse_separated_name()?,
                location: self.make_location(span),
                module_id: None,
            }
        } else {
            Export::Item {
                item_name: self.expect_identifier()?.0,
                location: self.make_location(span),
                definition_id: None,
            }
        };

        Ok(export)
    }

    pub fn parse_module(&mut self, tokens: Vec<(Token, Span)>, module_path: Vec<String>, filepath: PathBuf) -> Result<(), Error> {
        self.tokens = VecDeque::from(tokens);
        self.line_start = 0;
        self.filepath = filepath;

        let mut imports = Vec::new();
        let mut exports = Vec::new();
        let mut items = Vec::new();

        loop {
            self.skip_newlines();

            if !self.tokens.is_empty() {
                match self.peek() {
                    Token::KwImport => imports.push(self.parse_import()?),
                    Token::KwExport => exports.push(self.parse_export()?),
                    Token::KwFn => items.push(self.parse_fn_item()?),
                    Token::KwLet => items.push(self.parse_let_item()?),

                    _ => {
                        let (token, span) = self.next_with_span()?;
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
