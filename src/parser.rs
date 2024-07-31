use std::iter::Peekable;

use crate::{
    ast::{Expr, Var},
    lexer::Token,
};
use logos::{Lexer, Logos, Span};

enum ParseError {
    EmptyInput,
    UnexpectedToken(Span),
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'s> {
    lexer: Lexer<'s, Token>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s String) -> Self {
        Self {
            lexer: Token::lexer(source),
        }
    }

    fn next(&mut self) -> ParseResult<Token> {
        todo!()
    }

    fn peek(&self) -> Option<Token> {
        todo!()
    }

    fn expect(&mut self, token: Token) -> ParseResult<()> {
        todo!()
    }

    pub fn parse(&mut self) {
        for res in self.lexer.clone() {
            if let Ok(tok) = res {
                println!("{:#?}", tok);
            } else {
                panic!("Error");
            }
        }
    }
}
