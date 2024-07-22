use crate::{lang::Token, ValueType};
use snafu::Snafu;
use std::num::{ParseFloatError, ParseIntError};

use super::Position;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Snafu, Debug, Default, PartialEq, Clone)]
#[snafu(visibility(pub))]
pub enum Error {
    #[snafu(display("[{position}] Invalid base64: {source}"))]
    InvalidBase64 {
        position: Position,
        source: base64::DecodeError,
    },
    #[snafu(display("Invalid integer value: {source}"))]
    InvalidInteger { source: ParseIntError },
    #[snafu(display("Invalid floating point value: {source}"))]
    InvalidFloat { source: ParseFloatError },
    #[snafu(display("[{position}] Invalid semantic version: {reason}"))]
    InvalidVersion { position: Position, reason: String },
    #[snafu(display("[{position}] Invalid semantic version requirement: {reason}"))]
    InvalidRequire { position: Position, reason: String },
    #[snafu(display("Unknown error during lexing"))]
    #[default]
    Lexer,
    #[snafu(display("[{position}] Expected one of {} but got '{got}'", list.join(", ")))]
    OneOf {
        position: Position,
        list: Vec<String>,
        got: Token,
    },
    #[snafu(display("[{position}] Expected a '{expected}' but got '{got}"))]
    Expected {
        position: Position,
        expected: String,
        got: Token,
    },
    #[snafu(display("[{position}] {source}"))]
    Value {
        position: Position,
        #[snafu(source(from(crate::error::Error, Box::new)))]
        source: Box<crate::error::Error>,
    },
    #[snafu(display("[{position}] Value type of '{vtype}' is not assignable to a fixed type assignment with type '{atype}'"))]
    Type {
        position: Position,
        vtype: ValueType,
        atype: ValueType,
    },
    #[snafu(display("[{position}] Invalid syntax detected or unkonwn error hit"))]
    Unknown { position: Position },
    #[snafu(display("[{position}] Unexpected end of file reached"))]
    Eof { position: Position },
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidInteger { source: value }
    }
}

impl From<ParseFloatError> for Error {
    fn from(value: ParseFloatError) -> Self {
        Self::InvalidFloat { source: value }
    }
}

impl From<&Self> for Error {
    fn from(value: &Self) -> Self {
        value.clone()
    }
}
