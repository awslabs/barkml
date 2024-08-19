use std::iter::Peekable;

use logos::Lexer;

use crate::ast::Location;

use super::lexer::Token;
use crate::Result;

pub trait Read<'source> {
    fn peek(&mut self) -> Result<Option<Token>>;
    fn next(&mut self) -> Result<Option<Token>>;
    fn discard(&mut self);
    fn location(&mut self) -> Location;
}

pub struct TokenReader<'source> {
    pub module_name: String,
    pub lexer: Peekable<Lexer<'source, Token>>,
    pub location: Location,
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
            self.location = token.location(Some(self.module_name.clone()));
            Ok(Some(token.clone()))
        } else {
            Ok(None)
        }
    }

    fn discard(&mut self) {
        self.lexer.next();
    }

    fn location(&mut self) -> Location {
        self.location.clone()
    }
}
