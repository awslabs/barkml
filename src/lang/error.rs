use crate::{lang::Token, ValueType};
use snafu::Snafu;
use std::{
    num::{ParseFloatError, ParseIntError},
    ops::Range,
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Snafu, Debug, Default, PartialEq, Clone)]
#[snafu(visibility(pub))]
pub enum Error {
    #[snafu(display("Invalid base64: {source}"))]
    InvalidBase64 { source: base64::DecodeError },
    #[snafu(display("Invalid integer value: {source}"))]
    InvalidInteger { source: ParseIntError },
    #[snafu(display("Invalid floating point value: {source}"))]
    InvalidFloat { source: ParseFloatError },
    #[snafu(display("Invalid semantic version: {reason}"))]
    InvalidVersion { reason: String },
    #[snafu(display("Invalid semantic version requirement: {reason}"))]
    InvalidRequire { reason: String },
    #[snafu(display("Unknown error during lexing"))]
    #[default]
    Lexer,
    #[snafu(display("[{}:{}] Expected one of {} but got '{got}'", span.start, span.end, list.join(", ")))]
    OneOf {
        span: Range<usize>,
        list: Vec<String>,
        got: Token,
    },
    #[snafu(display("[{}:{}] Expected a '{expected}' but got '{got}", span.start, span.end))]
    Expected {
        span: Range<usize>,
        expected: String,
        got: Token,
    },
    #[snafu(display("[{}:{}] {source}", span.start, span.end))]
    Value {
        span: Range<usize>,
        #[snafu(source(from(crate::error::Error, Box::new)))]
        source: Box<crate::error::Error>,
    },
    #[snafu(display("[{}:{}] Value type of '{vtype}' is not assignable to a fixed type assignment with type '{atype}'", span.start, span.end))]
    Type {
        span: Range<usize>,
        vtype: ValueType,
        atype: ValueType,
    },
    #[snafu(display("[{}:{}] Invalid syntax detected or unkonwn error hit", span.start, span.end))]
    Unknown { span: Range<usize> },
    #[snafu(display("[{}:{}] Unexpected end of file reached", span.start, span.end))]
    Eof { span: Range<usize> },
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
