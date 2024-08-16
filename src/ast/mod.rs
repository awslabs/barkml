mod scope;
mod statement;
mod types;
mod value;

pub use scope::*;
pub use statement::*;
pub use types::*;
pub use value::*;

type Result<T> = std::result::Result<T, error::Error>;

pub(crate) mod error {
    use super::Location;
    use snafu::Snafu;

    #[derive(Snafu, Debug)]
    #[snafu(visibility(pub))]
    pub enum Error {
        #[snafu(display("{location} - an infinite loop was detected during macro resolution"))]
        Loop { location: Location },
        #[snafu(display("{location} - could not locate value to resolve macro at path: {path}"))]
        NotFound { location: Location, path: String },
        #[snafu(display("implicit conversion from '{left}' to '{right}' is not allowed"))]
        ImplicitConvert {
            left: super::types::ValueType,
            right: super::types::ValueType,
        },
    }
}
