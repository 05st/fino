use std::{borrow::BorrowMut, collections::{BTreeSet, VecDeque}, ops::Range, path::PathBuf};

use crate::{
    ast::*,
    cache::{CompilerCache, Location},
    error::{Error, ErrorKind},
    lexer::{LexerError, Token},
    types::*,
};
use logos::{Logos, Span};

pub struct Parser<'a> {
    compiler_cache: &'a mut CompilerCache,
    node_id_count: usize,
    tokens: VecDeque<(Token, Span)>,
    filepath: PathBuf,
    previous: Option<(Token, Span)>,
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
    pub fn new(compiler_cache: &'a mut CompilerCache) -> Parser<'a> {
        Parser {
            compiler_cache,
            node_id_count: 0,
            tokens: VecDeque::new(),
            filepath: PathBuf::new(),
            previous: None,
        }
    }

    // Insert node id to location mapping into compiler cache
    fn cache_location(&mut self, span: Span) -> NodeId {
        let node_id = NodeId(self.node_id_count);
        self.node_id_count += 1;

        let location = Location::new(self.filepath.clone(), span);
        self.compiler_cache
            .location_map
            .insert(node_id.clone(), location);

        node_id
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
            .borrow_mut()
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
    fn peek(&mut self) -> &Token {
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

    // Consume any identifier
    fn expect_identifier(&mut self) -> Result<String, Error> {
        match self.next_with_span() {
            (Token::LowerIdentifier(ident) | Token::UpperIdentifier(ident), _) => Ok(ident),
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

    fn parse_let_expr(&mut self) -> Result<Expr, Error> {
        let let_span = self.span();
        self.expect(Token::KwLet)?;

        let name = self.expect_identifier()?;
        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::KwIn)?;
        let body = self.parse_expr()?;

        Ok(Expr {
            node_id: self.cache_location(let_span),
            kind: ExprKind::Let {
                def_id: DefId(0),
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
            node_id: self.cache_location(if_span),
            kind: ExprKind::If {
                cond: Box::new(cond),
                texpr: Box::new(texpr),
                fexpr: Box::new(fexpr),
            },
        })
    }

    // Parse expression atom (i.e. literal, variable, parenthesized expression)
    fn parse_atom(&mut self) -> Result<Expr, Error> {
        let (token, span) = self.next_with_span();
        match token {
            // Parse parenthesized expression
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            }

            Token::UpperIdentifier(ident) | Token::LowerIdentifier(ident) => {
                // Parse qualified name if we see '::' token
                let name = if let Token::Dot = self.peek() {
                    // Restore identifier token for parse_name()
                    self.restore();
                    self.parse_name()?
                } else {
                    Name::Unqualified(ident)
                };

                Ok(Expr {
                    node_id: self.cache_location(span),
                    kind: ExprKind::Var {
                        def_id: DefId(0),
                        name: name,
                    },
                })
            }

            Token::LitBool(b) => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::Bool(b),
                },
            }),

            Token::LitDecimal(n) => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::Int(n),
                },
            }),

            Token::LitFloat(x) => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::Float(x),
                },
            }),

            Token::LitString(s) => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::String(s),
                },
            }),

            Token::LitChar(c) => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::Char(c),
                },
            }),

            Token::LitUnit => Ok(Expr {
                node_id: self.cache_location(span),
                kind: ExprKind::Lit {
                    literal: Lit::Unit,
                },
            }),

            other => self.error(expected_one_of!(other, "identifier", "literal", "'('"), span),
        }
    }

    // Parse function application
    fn parse_app(&mut self) -> Result<Expr, Error> {
        let mut result = self.parse_atom()?;

        // This is such a hack. We only discard the error from parse_atom() if it fails
        // to match the first token of the atom. Otherwise, if the first token matches
        // as an atom, we should go through with parsing the atom and report any
        // failure. Find better way to do this.
        let mut next_tmp = self.peek().clone();

        // Need to get span for possible arg expr before parse_atom() is called because
        // it consumes the token, so we can't get the span afterwards.
        let mut arg_span = self.span();
        let mut arg_result = self.parse_atom();

        while arg_result.is_ok() {
            result = Expr {
                node_id: self.cache_location(arg_span),
                kind: ExprKind::App {
                    fun: Box::new(result),
                    arg: Box::new(arg_result.unwrap()),
                },
            };

            arg_span = self.span();
            next_tmp = self.peek().clone();
            arg_result = self.parse_atom();
        }

        // When parsing atom fails, restore token consumed by parse_atom()
        self.restore();
        // If restored token doesn't match next token after attempting parse_atom(),
        // then it matched the first token of an atom, so we should go through with it
        // and report the failure.
        if *self.peek() != next_tmp {
            Err(arg_result.unwrap_err())
        } else {
            Ok(result)
        }
    }

    // Parse operators with a pratt parser
    fn parse_oper_expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        self.parse_app()
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

        let type_ann = self.parse_type()?;
        let mut type_vars = BTreeSet::new();
        type_ann.extract_type_vars(&mut type_vars);

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
                node_id: self.cache_location(span),
                kind: ExprKind::Lam {
                    param_def_id: DefId(0),
                    param: param_name,
                    body: Box::new(child),
                },
            });

        Ok(Item {
            node_id: self.cache_location(span),
            name,
            def_id: DefId(0),
            scheme: TypeScheme(type_vars, type_ann),
            expr,
        })
    }

    // Parse top-level let-definition
    fn parse_let_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwLet)?;

        let span = self.span();
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;

        let type_ann = self.parse_type()?;
        let mut type_vars = BTreeSet::new();
        type_ann.extract_type_vars(&mut type_vars);

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;

        self.expect(Token::Newline)?;

        Ok(Item {
            node_id: self.cache_location(span),
            name,
            def_id: DefId(0),
            scheme: TypeScheme(type_vars, type_ann),
            expr,
        })
    }

    fn parse_import(&mut self) -> Result<Import, Error> {
        let span = self.span();
        self.expect(Token::KwImport)?;

        let module_name = self.parse_separated_name()?;
        self.expect(Token::Newline)?;

        Ok(Import {
            node_id: self.cache_location(span),
            module_name,
        })
    }

    fn parse_export(&mut self) -> Result<Export, Error> {
        let span = self.span();
        self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek() {
            self.next();

            Export::Module {
                node_id: self.cache_location(span),
                module_name: self.parse_separated_name()?,
            }
        } else {
            Export::Item {
                node_id: self.cache_location(span),
                def_id: DefId(0),
                name: self.expect_identifier()?,
            }
        };

        self.expect(Token::Newline)?;

        Ok(export)
    }

    pub fn parse_module(&mut self, input: &String, module_name: Vec<String>, filepath: PathBuf) -> Result<Module, Error> {
        self.filepath = filepath;

        // Tokenize input
        let lexer = Token::lexer(input).spanned();

        self.tokens.clear();
        for lex in lexer {
            match lex {
                (Ok(token), span) => self.tokens.push_back((token, span)),
                (Err(lexer_error), span) => {
                    return {
                        // Translate lexer errors
                        match lexer_error {
                            LexerError::UnexpectedIndent(size) => {
                                self.error(ErrorKind::UnexpectedIndent(size), span)
                            }
                            LexerError::Default => self.error(ErrorKind::UnknownToken, span),
                        }
                    }
                }
            }
        }

        for token in self.tokens.clone() {
            println!("{:#?}", token);
        }

        // Parse input
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

        Ok(Module {
            module_name,
            items,
            imports,
            exports,
        })
    }
}
