#![allow(clippy::approx_constant)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::result_large_err)]
mod ast;
mod error;
mod load;
mod syn;

pub use ast::*;
pub use load::*;
use std::io::Cursor;

pub use syn::*;

pub use error::Error;
pub(crate) type Result<T> = std::result::Result<T, Error>;

/// Load a simple inline string as the main module and return the statement
pub fn from_str(input: &str) -> Result<Statement> {
    let mut cursor = Cursor::new(input.as_bytes());
    StandardLoader::default()
        .add_module("main", &mut cursor, None)?
        .load()
}
