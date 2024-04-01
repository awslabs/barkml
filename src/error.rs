use peg::str::LineCol;
use snafu::Snafu;
use std::fmt::Display;
use std::io;
use std::path::PathBuf;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum Error {
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

    #[snafu(display("IO error occured: {}", source))]
    Io {
        #[snafu(source(from(std::io::Error, Box::new)))]
        source: Box<io::Error>,
    },
}

impl serde::ser::Error for Error {
    fn custom<T>(_msg: T) -> Self
    where
        T: Display,
    {
        todo!()
    }
}
