use std::iter::Peekable;

use logos::Lexer;

use super::error::Result;
use super::{Position, Token};

pub trait Read<'source> {
    fn peek(&mut self) -> Result<Option<Token>>;
    fn next(&mut self) -> Result<Option<Token>>;
    fn discard(&mut self);
    fn position(&mut self) -> Position;
}

pub struct TokenReader<'source> {
    pub lexer: Peekable<Lexer<'source, Token>>,
    pub position: Position,
}

impl<'source> Read<'source> for TokenReader<'source> {
    fn peek(&mut self) -> Result<Option<Token>> {
        if let Some(Ok(token)) = self.lexer.peek() {
            Ok(Some(token.clone()))
        } else if let Some(Err(e)) = self.lexer.peek() {
            Err(e.clone())
        } else {
            Ok(None)
        }
    }

    fn next(&mut self) -> Result<Option<Token>> {
        if let Some(token) = self.lexer.next() {
            let token = token?;
            self.position = token.position();
            Ok(Some(token.clone()))
        } else {
            Ok(None)
        }
    }

    fn discard(&mut self) {
        self.lexer.next();
    }

    fn position(&mut self) -> Position {
        self.position.clone()
    }
}
