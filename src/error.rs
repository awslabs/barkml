use snafu::Snafu;
use std::{
    num::{ParseFloatError, ParseIntError},
    path::PathBuf,
};

use crate::{
    ast::{Location, ValueType},
    Token,
};

#[derive(Debug, Snafu, Clone, Default, PartialEq)]
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
    #[snafu(display("Could not resolve basename of path"))]
    Basename,
    #[snafu(display("{location} - invalid base64: {source}"))]
    Base64 {
        location: Location,
        source: base64::DecodeError,
    },
    #[snafu(display(
        "Collision occured between {left_id}({left_location}) and {right_id}({right_location})"
    ))]
    Collision {
        left_id: String,
        left_location: Location,
        right_id: String,
        right_location: Location,
    },
    #[snafu(display("{location} - unexpected end of file reached"))]
    Eof { location: Location },
    #[snafu(display("{location} - unexpected {got} when expecting {expected}"))]
    Expected {
        location: Location,
        expected: String,
        got: Token,
    },
    #[snafu(display("{location} - invalid floating point: {source}"))]
    Float {
        location: Location,
        source: ParseFloatError,
    },
    #[snafu(display("implicit conversion from '{left}' to '{right}' is not allowed"))]
    ImplicitConvert { left: ValueType, right: ValueType },
    #[snafu(display("{location} - invalid integer: {source}"))]
    Integer {
        location: Location,
        source: ParseIntError,
    },
    #[snafu(display("IO error occured during loading: {reason}"))]
    Io { reason: String },
    #[snafu(display("{location} - an infinite loop was detected during macro resolution"))]
    Loop { location: Location },
    #[snafu(display("{location} - no element with index '{index}'"))]
    NoElement { location: Location, index: usize },
    #[snafu(display("{location} - field not found: {field}"))]
    NoField { location: Location, field: String },
    #[snafu(display("{location} - field with name '{field}' is not a value"))]
    NoValue { location: Location, field: String },
    #[snafu(display("{location} - could not locate value to resolve macro at path: {path}"))]
    NoMacro { location: Location, path: String },
    #[snafu(display(
        "No main module defined, the standard loader requires at least one main module to load"
    ))]
    NoMain,
    #[snafu(display("No such file or directory '{}'", path.display()))]
    NotFound { path: PathBuf },
    #[snafu(display("{location} - not a scope with fields"))]
    NotScope { location: Location },
    #[snafu(display("{location} - invalid semantic version requirement: {reason}"))]
    Require { location: Location, reason: String },
    #[snafu(display("Could not find file named {name}.bml or directory named {name}.d in any of the search paths:\n{}", search_paths.iter().map(|x| x.to_string_lossy().to_string()).collect::<Vec<_>>().join("\n")))]
    Search {
        name: String,
        search_paths: Vec<PathBuf>,
    },
    #[snafu(display("unknown error occured"))]
    #[default]
    Unknown,
    #[snafu(display("{location} - invalid semantic version: {reason}"))]
    Version { location: Location, reason: String },
}
