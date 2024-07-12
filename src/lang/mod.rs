mod lexer;
mod parser;

use crate::lang::parser::module_parser;
use chumsky::{Parser, Stream};
pub use lexer::*;
use logos::Logos;

pub fn from_str(input: &str) -> crate::error::Result<crate::Value> {
    // Create a logos lexer over the source code
    let token_iter = Token::lexer(input)
        .spanned()
        // Convert logos errors into tokens. We want parsing to be recoverable and not fail at the lexing stage, so
        // we have a dedicated `Token::Error` variant that represents a token error that was previously encountered
        .map(|(tok, span)| match tok {
            // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
            // to work with
            Ok(tok) => (tok, span),
            Err(()) => (Token::Error, span),
        });

    // Turn the token iterator into a stream that chumsky can use for things like backtracking
    let token_stream = Stream::from_iter(input.len()..input.len(), token_iter);

    module_parser()
        .parse(token_stream)
        .map_err(|e| crate::error::Error::NewParse {
            reason: e
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
        })
}

#[cfg(test)]
mod test {

    #[test]
    fn test_basic() {
        let input = r#"
[section]
foo = "hello"
"#;
        super::from_str(input).unwrap();
    }
}
