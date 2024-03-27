use snafu::Snafu;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum Error {
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
}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        todo!()
    }
}
