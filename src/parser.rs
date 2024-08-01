use std::collections::VecDeque;

use crate::{
    ast::{Expr, Module, Var},
    lexer::Token,
};
use logos::{Lexer, Logos, Span};

#[derive(Debug)]
pub enum ParseError {
    ReachedEnd,
    UnexpectedToken(Span),
    TokenMismatch(Token, Token, Span)
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
                (Err(_), span) => return Err(ParseError::UnexpectedToken(span)),
            }
        }

        Ok(Self { tokens })
    }

    // Advance token stream
    fn next(&mut self) -> ParseResult<Token> {
        self.tokens.pop_front().map(|t| t.0).ok_or(ParseError::ReachedEnd)
    }

    // Peek current token
    fn peek(&mut self) -> ParseResult<&Token> {
        self.tokens.front().map(|t| &t.0).ok_or(ParseError::ReachedEnd)
    }

    // Span of current token
    fn span(&mut self) -> ParseResult<Span> {
        self.tokens.front().map(|t| t.1.clone()).ok_or(ParseError::ReachedEnd)
    }

    // Consume specific token variant
    fn expect(&mut self, expected: Token) -> ParseResult<Token> {
        let current = self.peek()?;
        if std::mem::discriminant(current) == std::mem::discriminant(&expected) {
            Ok(self.next()?)
        } else {
            Err(ParseError::TokenMismatch(current.clone(), expected, self.span()?))
        }
    }

    pub fn parse(&mut self) -> ParseResult<Module<Var>> {
        let next = self.next()?;
        println!("{:#?}", next);
        for (token, span) in self.tokens.clone().into_iter() {
            println!("{:#?}", token);
        }
        Ok(Module { items: Vec::new() })
    }
}
