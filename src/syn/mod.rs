mod lexer;
mod parser;
mod read;

pub use lexer::*;
pub use parser::*;

type Result<T> = std::result::Result<T, error::Error>;

pub(crate) mod error {
    use snafu::Snafu;
    use std::num::{ParseFloatError, ParseIntError};

    use crate::ast::Location;
    use crate::{Token, ValueType};

    #[derive(Snafu, Debug, PartialEq, Default, Clone)]
    #[snafu(visibility(pub))]
    pub enum Error {
        #[snafu(display(
            "{location} - cannot assign a value of type '{right}' to a field with type '{left}'"
        ))]
        Assign {
            location: Location,
            left: ValueType,
            right: ValueType,
        },
        #[snafu(display("{location} - invalid base64: {source}"))]
        Base64 {
            location: Location,
            source: base64::DecodeError,
        },
        #[snafu(display("{location} - invalid integer: {source}"))]
        Integer {
            location: Location,
            source: ParseIntError,
        },
        #[snafu(display("{location} - invalid floating point: {source}"))]
        Float {
            location: Location,
            source: ParseFloatError,
        },
        #[snafu(display("{location} - unexpected end of file reached"))]
        Eof { location: Location },
        #[snafu(display("{location} - invalid semantic version: {reason}"))]
        Version { location: Location, reason: String },
        #[snafu(display("{location} - invalid semantic version requirement: {reason}"))]
        Require { location: Location, reason: String },
        #[snafu(display("unknown error occured during lexical analysis"))]
        #[default]
        Lexical,
        #[snafu(display("{location} - unexpected {got} when expecting {expected}"))]
        Expected {
            location: Location,
            expected: String,
            got: Token,
        },
        #[snafu(display("{reason}"))]
        Ast { reason: String },
        #[snafu(display("{location} - unknown syntax detected"))]
        Unknown { location: Location },
    }

    impl From<crate::ast::error::Error> for Error {
        fn from(value: crate::ast::error::Error) -> Self {
            Self::Ast {
                reason: value.to_string(),
            }
        }
    }
}
