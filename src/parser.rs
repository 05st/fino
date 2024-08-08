use std::{collections::VecDeque, ops::Range, path::PathBuf};

use crate::{
    ast::*,
    cache::{CompilerCache, Location},
    error::{Error, ErrorKind},
    lexer::{LexerError, Token},
    types::{Type, TypeVar},
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
    ( $( $x:literal ),+ ) => {
        ErrorKind::ExpectedOneOf(vec![
            $(
                String::from($x),
            )+
        ])
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
        self.compiler_cache.location_map.insert(node_id.clone(), location);

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

    // The next few functions can throw a ReachedEnd error which is supposed to have
    // no location info associated with it (other than a filepath), we pass a
    // Range(0, 0), but it is important to be careful of what is shown to the user.

    // Advance token stream and return popped (token, span)
    fn next_with_span(&mut self) -> Result<(Token, Span), Error> {
        let result = self
            .tokens
            .pop_front()
            .ok_or(self.make_error(ErrorKind::ReachedEnd, Range { start: 0, end: 0 }))?;

        self.previous = Some(result.clone());
        Ok(result)
    }

    // Advance token stream and return popped token
    fn next(&mut self) -> Result<Token, Error> {
        self.next_with_span().map(|res| res.0)
    }

    // Peek current token
    fn peek(&mut self) -> Result<&Token, Error> {
        self.tokens
            .front()
            .map(|t| &t.0)
            .ok_or(self.make_error(ErrorKind::ReachedEnd, Range { start: 0, end: 0 }))
    }

    // Span of current token
    fn span(&mut self) -> Result<Span, Error> {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .ok_or(self.make_error(ErrorKind::ReachedEnd, Range { start: 0, end: 0 }))
    }

    // Consume specific token variant
    fn expect(&mut self, expected: Token) -> Result<Token, Error> {
        let (current, span) = self.next_with_span()?;

        if std::mem::discriminant(&current) == std::mem::discriminant(&expected) {
            Ok(current)
        } else {
            self.error(ErrorKind::ExpectedOneOf(vec![expected.to_string()]), span)
        }
    }

    // Consume any identifier
    fn expect_identifier(&mut self) -> Result<String, Error> {
        match self.next_with_span()? {
            (Token::LowerIdentifier(ident) | Token::UpperIdentifier(ident), _) => Ok(ident),
            (_, span) => self.error(expected_one_of!("identifier"), span),
        }
    }

    // Consume capitalized identifier
    fn expect_upper_identifier(&mut self) -> Result<String, Error> {
        match self.next_with_span()? {
            (Token::UpperIdentifier(ident), _) => Ok(ident),
            (_, span) => self.error(expected_one_of!("capitalized identifier"), span),
        }
    }

    // Skip newlines while ignoring ReachedEnd error from self.peek()
    fn skip_newlines(&mut self) {
        while let Ok(Token::Newline) = self.peek() {
            self.next();
        }
    }

    fn parse_separated_name(&mut self) -> Result<Vec<String>, Error> {
        let mut result = Vec::new();
        loop {
            result.push(self.expect_identifier()?);
            match self.peek()? {
                Token::ColonColon => self.next()?,
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
        let (token, span) = self.next_with_span()?;

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

            _ => self.error(expected_one_of!("type", "type variable"), span),
        }
    }

    // Parse type
    fn parse_type(&mut self) -> Result<Type, Error> {
        let base_type = self.parse_base_type()?;

        match self.peek()? {
            Token::SmallArrow => {
                self.next()?;
                Ok(Type::Fun(Box::new(base_type), Box::new(self.parse_type()?)))
            }
            _ => Ok(base_type),
        }
    }

    // Parse if expression
    fn parse_if_expr(&mut self) -> Result<Expr, Error> {
        let if_span = self.span()?;
        self.expect(Token::KwIf)?;
        let cond = self.parse_expr()?;

        self.expect(Token::KwThen)?;
        let texpr = self.parse_expr()?;

        self.expect(Token::KwElse)?;
        let fexpr = self.parse_expr()?;

        Ok(Expr::If {
            node_id: self.cache_location(if_span),
            cond: Box::new(cond),
            texpr: Box::new(texpr),
            fexpr: Box::new(fexpr),
        })
    }

    // Parse expression atom (i.e. literal, variable, parenthesized expression)
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
                // Parse qualified name if we see '::' token
                let name = if let Token::ColonColon = self.peek()? {
                    // Restore identifier token for parse_name()
                    self.restore();
                    self.parse_name()?
                } else {
                    Name::Unqualified(ident)
                };

                Ok(Expr::Var {
                    node_id: self.cache_location(span),
                    def_id: DefId(0),
                    name: name,
                })
            }

            Token::LitBool(b) => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::Bool(b),
            }),

            Token::LitDecimal(n) => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::Int(n),
            }),

            Token::LitFloat(x) => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::Float(x),
            }),

            Token::LitString(s) => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::String(s),
            }),

            Token::LitChar(c) => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::Char(c),
            }),

            Token::ClosedParens => Ok(Expr::Lit {
                node_id: self.cache_location(span),
                literal: Lit::Unit,
            }),

            _ => self.error(expected_one_of!("identifier", "literal", "'('"), span),
        }
    }
    
    // Parse function application
    fn parse_fn_app(&mut self) -> Result<Expr, Error> {
        let mut result = self.parse_atom()?;

        // Need to get span for possible arg expr before parse_atom() is
        // called because it consumes the token, so we can't get the span
        // afterwards.
        let mut arg_span = self.span()?;
        while let Ok(arg_expr) = self.parse_atom() {
            result = Expr::App {
                node_id: self.cache_location(arg_span),
                fun: Box::new(result),
                arg: Box::new(arg_expr),
            };
            arg_span = self.span()?;
        }
        // When parsing atom fails, restore token consumed by parse_atom()
        self.restore();

        Ok(result)
    }

    // Parse expression
    fn parse_expr(&mut self) -> Result<Expr, Error> {
        match self.peek()? {
            Token::KwIf => self.parse_if_expr(),
            _ => self.parse_fn_app(),
        }
    }

    // Parse and desugar top-level function definition
    fn parse_fn_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwFn)?;

        let span = self.span()?;
        let fn_name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let type_ann = self.parse_type()?;

        self.expect(Token::Indent)?;

        // TODO:
        // Parse and desugar multiple pattern matching branches

        let mut params = Vec::new();
        loop {
            let (token, span) = self.next_with_span()?;
            match token {
                Token::UpperIdentifier(param) | Token::LowerIdentifier(param) => params.push((param, span)),
                Token::Equal => break,
                _ => return self.error(expected_one_of!("identifier", "'='"), span),
            }
        }

        let body = self.parse_expr()?;
        self.expect(Token::Dedent)?;
        
        // Reverse for proper fold direction
        params.reverse();

        // Desugar function into definition with curried lambdas
        let lambda = params
            .into_iter()
            .fold(body, |child, (param_name, span)| Expr::Lam {
                node_id: self.cache_location(span),
                param_def_id: DefId(0),
                param: param_name,
                body: Box::new(child),
        });

        Ok(Item {
            node_id: self.cache_location(span),
            name: fn_name,
            def_id: DefId(0),
            type_ann,
            expr: lambda,
        })
    }

    // Parse top-level let-definition
    fn parse_let_item(&mut self) -> Result<Item, Error> {
        self.expect(Token::KwLet)?;

        let span = self.span()?;
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let type_ann = self.parse_type()?;

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;
        
        self.expect(Token::Newline)?;

        Ok(Item {
            node_id: self.cache_location(span),
            name,
            def_id: DefId(0),
            type_ann,
            expr,
        })
    }

    fn parse_import(&mut self) -> Result<Import, Error> {
        let span = self.span()?;
        self.expect(Token::KwImport)?;

        let module_name = self.parse_separated_name()?;
        self.expect(Token::Newline)?;

        Ok(Import {
            node_id: self.cache_location(span),
            module_name,
        })
    }

    fn parse_export(&mut self) -> Result<Export, Error> {
        let span = self.span()?;
        self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek()? {
            self.next()?;
            
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

    pub fn parse(&mut self, input: &String, module_name: Vec<String>, filepath: PathBuf) -> Result<Module, Error> {
        self.filepath = filepath;

        // Tokenize input
        let lexer = Token::lexer(input).spanned();

        self.tokens.clear();
        for lex in lexer {
            match lex {
                (Ok(token), span) => self.tokens.push_back((token, span)),
                (Err(lexer_error), span) => return {
                    // Translate lexer errors
                    match lexer_error {
                        LexerError::UnexpectedIndent(size) => self.error(ErrorKind::UnexpectedIndent(size), span),
                        LexerError::Default => self.error(ErrorKind::UnknownToken, span),
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
                match self.peek()? {
                    Token::KwImport => imports.push(self.parse_import()?),
                    Token::KwExport => exports.push(self.parse_export()?),
                    Token::KwFn => items.push(self.parse_fn_item()?),
                    Token::KwLet => items.push(self.parse_let_item()?),

                    _ => {
                        let span = self.span()?;
                        return self.error(
                            expected_one_of!("'import'", "'export'", "top-level definition"),
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
