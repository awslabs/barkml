pub(crate) mod error;
mod lexer;
mod parser;

use crate::lang::parser::Parser;
pub use lexer::*;
use logos::Logos;
use snafu::ResultExt;

pub fn from_str(input: &str) -> crate::error::Result<crate::Value> {
    let mut parser = Parser::new(Token::lexer(input));
    parser.parse().context(crate::error::ParseSnafu)
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
