use std::fmt::Display;
use std::io;
use std::num::ParseIntError;
use std::path::PathBuf;

use peg::str::LineCol;
use snafu::Snafu;

use crate::ValueType;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum Error {
    #[snafu(display("Expected an id for a value"))]
    NoID,
    #[snafu(display("Array does not have expected number of elements ({})", count))]
    ArrayLen { count: usize },
    #[snafu(display("Value expected to be an array but was not"))]
    NotArray,
    #[snafu(display(
        "Value type does not match declared type definition {} != {}",
        left,
        right
    ))]
    TypeCollision { left: ValueType, right: ValueType },

    #[snafu(display("Invalid semantic version defined '{}': {}", version, source))]
    SemVer {
        version: String,
        source: semver::Error,
    },
    #[snafu(display("Invalid version requirement defined '{}': {}", version, source))]
    SemVerReq {
        version: String,
        source: semver::Error,
    },

    #[snafu(display("Internal error on resolution due to scopifying a none scopable statement"))]
    NotScopable,
    #[snafu(display("Error resolving macro, no such symbol found by path {}", path))]
    MacroNotFound { path: String },
    #[snafu(display("Macro resolution loop detected"))]
    MacroLoop,
    #[snafu(display(
        "Macro contains a non-index where one would be required '{}': {}",
        segment,
        source
    ))]
    MacroNonIndex {
        segment: String,
        source: ParseIntError,
    },
    #[snafu(display("Macro cannot resolve super when there is no parent scope"))]
    MacroNoSuper,
    #[snafu(display("Macro references a non-existing scope: {}", scope))]
    MacroScopeNotFound { scope: String },
    #[snafu(display("Error parsing BarkML file: {}", source))]
    Parse {
        #[snafu(source(from(peg::error::ParseError<LineCol>, Box::new)))]
        source: Box<peg::error::ParseError<LineCol>>,
    },
    #[cfg(feature = "binary")]
    #[snafu(display(
        "Encoded object is not a proper packed barkml message pack item: {}",
        source
    ))]
    MsgPackNotExpected {
        source: msgpack_simple::ConversionError,
    },
    #[cfg(feature = "binary")]
    #[snafu(display("Encoded value is not a proper msgpack value: {}", source))]
    MsgPackEncoded { source: msgpack_simple::ParseError },
    #[cfg(feature = "binary")]
    #[snafu(display("Unsupported encoded value in msgpack data"))]
    MsgPackUnsupported,

    #[snafu(display("Provided path does not exist: {}", path.display()))]
    LoaderNotFound { path: PathBuf },
    #[snafu(display("Provided path is not a file which is expected in single loader mode: {}", path.display()))]
    LoaderNotFile { path: PathBuf },
    #[snafu(display("A collision occurred merging configuration files on id {}", id))]
    LoaderMergeCollision { id: String },
    #[snafu(display("Could not parse the module name from the filename"))]
    LoaderModuleParse,
    #[snafu(display("A module with name '{}' is already present in this loader", name))]
    LoaderModuleCollision { name: String },
    #[snafu(display("No modules have been added to this loader to load from"))]
    LoaderNoModules,

    #[snafu(display("IO error occured: {}", source))]
    Io {
        #[snafu(source(from(std::io::Error, Box::new)))]
        source: Box<io::Error>,
    },
    #[snafu(display("{}", message))]
    Custom { message: String },
}

impl serde::ser::Error for Error {
    fn custom<T>(_msg: T) -> Self
    where
        T: Display,
    {
        todo!()
    }
}
