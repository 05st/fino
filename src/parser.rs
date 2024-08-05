use std::collections::{HashMap, VecDeque};

use crate::{
    ast::*,
    error::CompilerError,
    lexer::{LexerError, Token},
    types::{Type, TypeVar}
};
use logos::{Logos, Span};

#[derive(Debug)]
pub enum ParseError {
    ReachedEnd,
    UnexpectedToken,
    TokenMismatch(String),
    IncorrectIndent(usize, usize),
}

type ParseResult<T> = Result<T, CompilerError<ParseError>>;

pub struct Parser {
    source_map: HashMap<NodeId, NodeSource>,
    node_id_count: usize,
    tokens: VecDeque<(Token, Span)>,
    file_path: String,
    previous: Option<(Token, Span)>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            source_map: HashMap::new(),
            node_id_count: 0,
            tokens: VecDeque::new(),
            file_path: String::new(),
            previous: None,
        }
    }

    // Create node id to source position mapping and insert in source_map
    fn register_source(&mut self, span: Span) -> NodeId {
        let node_id = NodeId(self.node_id_count);

        self.source_map.insert(node_id.clone(), NodeSource {
            span,
            file_path: self.file_path.clone(),
        });

        self.node_id_count += 1;

        node_id
    }

    // Maybe use a macro instead?
    // self.error(...)?; or return self.error(); looks weird
    fn error<T>(&self, error: ParseError, span: Span) -> ParseResult<T> {
        Err(CompilerError::new(error, Some(span), self.file_path.clone()))
    }

    // Puts previous token into token stream. Can only be called once before another
    // token needs to be consumed since we only store the previous token, not a
    // history of previous tokens. A bit of a hacky solution to parsing function
    // applications and qualified variable names.
    fn backtrack(&mut self) {
        // We assume backtrack() is only called when self.previous is Some(expr),
        // otherwise .unwrap() will panic
        self.tokens.push_front(self.previous.to_owned().unwrap());
        self.previous = None;
    }

    // Advance token stream and return popped (token, span)
    fn next_with_span(&mut self) -> ParseResult<(Token, Span)> {
        let result = self.tokens
            .pop_front()
            .ok_or(CompilerError::new(ParseError::ReachedEnd, None, self.file_path.clone()))?;

        self.previous = Some(result.clone());

        Ok(result)
    }

    // Advance token stream and return popped token
    fn next(&mut self) -> ParseResult<Token> {
        self.next_with_span().map(|res| res.0)
    }

    // Peek current token
    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens
            .front()
            .map(|t| &t.0)
            .ok_or(CompilerError::new(ParseError::ReachedEnd, None, self.file_path.clone()))
    }

    // Span of current token
    fn span(&mut self) -> ParseResult<Span> {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .ok_or(CompilerError::new(ParseError::ReachedEnd, None, self.file_path.clone()))
    }

    // Consume specific token variant
    fn expect(&mut self, expected: Token) -> ParseResult<Token> {
        let (current, span) = self.next_with_span()?;

        if std::mem::discriminant(&current) == std::mem::discriminant(&expected) {
            Ok(current)
        } else {
            self.error(ParseError::TokenMismatch(format!("{:?}", expected)), span)
        }
    }

    // TODO:
    // Probably add an optional (or mandatory) message string parameter to expect()
    // instead of using this function.
    fn expect_identifier(&mut self) -> ParseResult<String> {
        match self.next_with_span()? {
            (Token::Identifier(ident), _) => Ok(ident),
            (_, span) => self.error(ParseError::TokenMismatch(String::from("identifier")), span),
        }
    }

    // Skip newlines while ignoring ReachedEnd error from self.peek()
    fn skip_newlines(&mut self) {
        while let Ok(Token::Newline) = self.peek() {
            self.next();
        }
    }

    fn parse_name(&mut self) -> ParseResult<Name> {
        let mut result = Vec::new();
        loop {
            result.push(self.expect_identifier()?);

            if let Token::ColonColon = self.peek()? {
                self.next()?;
            } else {
                break;
            }
        }

        if result.len() == 1 {
            // We should have parsed at least one identifier
            Ok(Name::Unqualified(result.first().unwrap().to_owned()))
        } else {
            Ok(Name::Qualified(result))
        }
    }

    fn parse_module_name(&mut self) -> ParseResult<Vec<String>> {
        // Use parse_name() to help parse module name
        // TODO:
        // Maybe it should be the other way around?
        Ok(match self.parse_name()? {
            Name::Unqualified(ident) => vec![ident],
            Name::Qualified(qual) => qual
        })
    }

    // Parse type constant or type variable
    fn parse_base_type(&mut self) -> ParseResult<Type> {
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

            Token::Identifier(ident) => {
                if ident.chars().next().unwrap().is_uppercase() {
                    Ok(Type::Const(ident))
                } else {
                    Ok(Type::Var(TypeVar(ident)))
                }
            }

            _ => self.error(ParseError::TokenMismatch(String::from("type constant or type variable")), span),
        }
    }

    // Parse type
    fn parse_type(&mut self) -> ParseResult<Type> {
        let base_type = self.parse_base_type()?;

        match self.peek()? {
            Token::SmallArrow => {
                self.next()?;
                Ok(Type::Fun(Box::new(base_type), Box::new(self.parse_type()?)))
            }
            _ => Ok(base_type),
        }
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let if_span = self.span()?;
        self.expect(Token::KwIf)?;
        let cond = self.parse_expr()?;

        self.expect(Token::KwThen)?;
        let texpr = self.parse_expr()?;

        self.expect(Token::KwElse)?;
        let fexpr = self.parse_expr()?;

        Ok(Expr::If {
            node_id: self.register_source(if_span),
            cond: Box::new(cond),
            texpr: Box::new(texpr),
            fexpr: Box::new(fexpr),
        })
    }

    fn parse_atom(&mut self) -> ParseResult<Expr> {
        let (token, span) = self.next_with_span()?;
        match token {
            // Parse parenthesized expression
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            },

            Token::Identifier(ident) => {
                // Parse qualified name if we see '::' token
                let name = if let Token::ColonColon = self.peek()? {
                    // Restore identifier token for parse_name()
                    self.backtrack();

                    self.parse_name()?
                } else {
                    Name::Unqualified(ident)
                };

                Ok(Expr::Var {
                    node_id: self.register_source(span),
                    def_id: DefId(0),
                    name: name
                })
            },

            Token::LitBool(b) => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::Bool(b),
            }),

            Token::LitDecimal(n) => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::Int(n),
            }),

            Token::LitFloat(x) => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::Float(x),
            }),

            Token::LitString(s) => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::String(s),
            }),

            Token::LitChar(c) => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::Char(c),
            }),

            Token::ClosedParens => Ok(Expr::Lit {
                node_id: self.register_source(span),
                literal: Lit::Unit,
            }),

            _ => self.error(ParseError::TokenMismatch(String::from("identifier, literal, or (")), span),
        }
    }

    fn parse_fn_app(&mut self) -> ParseResult<Expr> {
        let mut result = self.parse_atom()?;

        // Need to get span for possible arg expr before parse_atom() is
        // called because it consumes the token, so we can't get the span
        // afterwards.
        let mut arg_span = self.span()?;
        while let Ok(arg_expr) = self.parse_atom() {
            result = Expr::App {
                node_id: self.register_source(arg_span),
                fun: Box::new(result),
                arg: Box::new(arg_expr),
            };
            arg_span = self.span()?;
        }
        // When parsing atom fails, restore token consumed by parse_atom()
        self.backtrack();

        Ok(result)
    }

    // Parse expression
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        match self.peek()? {
            Token::KwIf => self.parse_if_expr(),
            _ => self.parse_fn_app(),
        }
    }

    // Parse and desugar top-level function definition
    fn parse_fn_item(&mut self) -> ParseResult<Item> {
        self.expect(Token::KwFn)?;
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
                Token::Identifier(param) => params.push((param, span)),
                Token::Equal => break,
                _ => return self.error(ParseError::TokenMismatch(String::from("identifier or =")), span),
            }
        }

        let body = self.parse_expr()?;
        self.expect(Token::Dedent)?;
        
        // Reverse for proper fold direction
        params.reverse();

        // Desugar function into definition with curried lambdas
        let lambda = params.into_iter().fold(body, |child, (param_name, span)| {
            Expr::Lam {
                node_id: self.register_source(span),
                param_def_id: DefId(0),
                param: param_name,
                body: Box::new(child),
            }
        });

        Ok(Item {
            name: fn_name,
            type_ann,
            expr: lambda,
        })
    }

    fn parse_let_item(&mut self) -> ParseResult<Item> {
        self.expect(Token::KwLet)?;
        let name = self.expect_identifier()?;

        self.expect(Token::Colon)?;
        let type_ann = self.parse_type()?;

        self.expect(Token::Equal)?;
        let expr = self.parse_expr()?;
        
        self.expect(Token::Newline)?;

        Ok(Item {
            name,
            type_ann,
            expr,
        })
    }

    fn parse_import(&mut self) -> ParseResult<Import> {
        let span = self.span()?;
        self.expect(Token::KwImport)?;

        // Use parse_name() to help parse the module name
        let module_name = self.parse_module_name()?;
        self.expect(Token::Newline)?;

        Ok(Import {
            node_id: self.register_source(span),
            module_name,
        })
    }

    fn parse_export(&mut self) -> ParseResult<Export> {
        let span = self.span()?;
        self.expect(Token::KwExport)?;

        let export = if let Token::KwModule = self.peek()? {
            self.next()?;
            
            Export::Module {
                node_id: self.register_source(span),
                module_name: self.parse_module_name()?,
            }
        } else {
            Export::Item {
                node_id: self.register_source(span),
                def_id: DefId(0),
                name: self.expect_identifier()?,
            }
        };

        self.expect(Token::Newline)?;

        Ok(export)
    }

    pub fn parse(&mut self, input: &String) -> ParseResult<Module> {
        // Tokenize input
        let lexer = Token::lexer(input).spanned();

        self.tokens.clear();
        for lex in lexer {
            match lex {
                (Ok(token), span) => self.tokens.push_back((token, span)),
                (Err(lexer_error), span) => return {
                    match lexer_error {
                        LexerError::IncorrectIndent(got, exp) => self.error(ParseError::IncorrectIndent(got, exp), span),
                        LexerError::Default => self.error(ParseError::UnexpectedToken, span),
                    }
                }
            }
        }

        for tok in self.tokens.clone() {
            println!("{:#?}", tok);
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
                        return self.error(ParseError::TokenMismatch(String::from("import, export, or top-level definition")), span);
                    },
                }
            } else {
                break;
            }
        }

        Ok(Module {
            items,
            imports,
            exports,
        })
    }
}
