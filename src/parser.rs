use std::collections::{HashMap, VecDeque};

use crate::{
    ast::{Expr, Item, Module, NodeId, NodeSource},
    error::CompilerError,
    lexer::{LexerError, Token},
    types::Type
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
    node_id: usize,
    tokens: VecDeque<(Token, Span)>,
    file_path: String,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            source_map: HashMap::new(),
            node_id: 0,
            tokens: VecDeque::new(),
            file_path: String::new()
        }
    }

    // Maybe use a macro instead?
    // self.error(...)?; or return self.error(); looks weird
    fn error<T>(&self, error: ParseError, span: Span) -> ParseResult<T> {
        Err(CompilerError::with_source(error, NodeSource::new(span, self.file_path.clone())))
    }

    // Advance token stream and return popped token
    fn next(&mut self) -> ParseResult<Token> {
        self.tokens
            .pop_front()
            .map(|t| t.0)
            .ok_or(CompilerError::from(ParseError::ReachedEnd))
    }

    // Advance token stream and return popped (token, span)
    fn next_with_span(&mut self) -> ParseResult<(Token, Span)> {
        self.tokens
            .pop_front()
            .ok_or(CompilerError::from(ParseError::ReachedEnd))
    }

    // Peek current token
    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens
            .front()
            .map(|t| &t.0)
            .ok_or(CompilerError::from(ParseError::ReachedEnd))
    }

    // Span of current token
    fn span(&mut self) -> ParseResult<Span> {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .ok_or(CompilerError::from(ParseError::ReachedEnd))
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

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        todo!()
    }

    fn parse_type_ann(&mut self) -> ParseResult<Type> {
        todo!()
    }

    // Parse and desugar top-level fn definition
    fn parse_function(&mut self) -> ParseResult<Item> {
        self.expect(Token::KwFn)?;

        let fn_name = match self.next_with_span()? {
            (Token::Identifier(ident), _) => ident,
            (_, span) => return self.error(ParseError::TokenMismatch(String::from("identifier")), span),
        };

        self.expect(Token::Colon)?;
        let type_ann = self.parse_type_ann()?;

        self.expect(Token::Indent)?;

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

        // TODO:
        // Parse and desugar multiple pattern matching branches
        self.expect(Token::Dedent)?;

        // Desugar function into definition with curried lambdas

        todo!()
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
        let items = vec![self.parse_function()?];
        Ok(Module {
            items,
        })
    }
}
