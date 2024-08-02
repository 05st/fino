use std::collections::VecDeque;

use crate::{
    ast::{Expr, Item, Module},
    lexer::{LexerError, Token}, types::Type,
};
use logos::{Lexer, Logos, Span};

#[derive(Debug)]
pub enum ParseError {
    ReachedEnd,
    UnexpectedToken(Span),
    TokenMismatch(String, Span),
    IncorrectIndent(usize, usize, Span),
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: VecDeque<(Token, Span)>,
}

impl Parser {
    pub fn new(input: &String) -> ParseResult<Self> {
        let mut tokens = VecDeque::new();
        let lexer = Token::lexer(input).spanned();

        for lex in lexer {
            match lex {
                (Ok(token), span) => tokens.push_back((token, span)),
                (Err(lexer_error), span) => return {
                    match lexer_error {
                        LexerError::IncorrectIndent(got, exp) => Err(ParseError::IncorrectIndent(got, exp, span)),
                        LexerError::Default => Err(ParseError::UnexpectedToken(span)),
                    }
                }
            }
        }

        for tok in tokens.clone() {
            println!("{:#?}", tok);
        }

        Ok(Self { tokens })
    }

    // Advance token stream
    fn next(&mut self) -> ParseResult<Token> {
        self.tokens
            .pop_front()
            .map(|t| t.0)
            .ok_or(ParseError::ReachedEnd)
    }

    // Peek current token
    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens
            .front()
            .map(|t| &t.0)
            .ok_or(ParseError::ReachedEnd)
    }

    // Span of current token
    fn span(&mut self) -> ParseResult<Span> {
        self.tokens
            .front()
            .map(|t| t.1.clone())
            .ok_or(ParseError::ReachedEnd)
    }

    // Consume specific token variant
    fn expect(&mut self, expected: Token) -> ParseResult<Token> {
        let current = self.peek()?;

        if std::mem::discriminant(current) == std::mem::discriminant(&expected) {
            Ok(self.next()?)
        } else {
            Err(ParseError::TokenMismatch(
                format!("{:?}", expected),
                self.span()?,
            ))
        }
    }

    fn parse_type_ann(&mut self) -> ParseResult<Type> {
        todo!()
    }

    // Parse and desugar top-level fn definition
    fn parse_function(&mut self) -> ParseResult<Item> {
        self.expect(Token::KwFn)?;

        if let Token::Identifier(fn_name) = self.peek()? {
            self.next()?;

            self.expect(Token::Colon)?;
            let type_ann = self.parse_type_ann()?;
            self.expect(Token::Indent)?;

            todo!()
        } else {
            Err(ParseError::TokenMismatch(String::from("identifier"), self.span()?))
        }
    }

    pub fn parse(&mut self) -> ParseResult<Module> {
        let items = vec![self.parse_function()?];
        Ok(Module {
            items,
        })
    }
}
