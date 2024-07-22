pub(crate) mod error;
mod lexer;
mod parser;
mod read;

use crate::lang::parser::Parser;
pub use lexer::*;
use logos::Logos;

pub fn from_str(input: &str) -> error::Result<crate::Value> {
    let mut parser = Parser::new(Token::lexer(input));
    parser.parse()
}

#[cfg(test)]
mod test {

    #[test]
    fn test_basic() {
        let input = r#"
[section-a]
foo = "hello"
"#;
        super::from_str(input).unwrap();
    }
}
