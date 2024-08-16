#![allow(clippy::approx_constant)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::result_large_err)]
mod ast;
mod load;
mod syn;

pub use ast::*;
pub use load::*;
use snafu::Snafu;
use std::io::Cursor;

pub use syn::*;

/// Error occured during abstract syntax tree analysis
pub type ASTError = crate::ast::error::Error;
/// Error occured during syntax handling and lexical analysis
pub type SyntaxError = crate::syn::error::Error;
/// Error occured while loading barkml or walking modules
pub type LoadError = crate::load::error::Error;

/// Parent error type
#[derive(Snafu, Debug)]
#[snafu(visibility(pub))]
pub enum Error {
    #[snafu(display("{source}"))]
    Ast { source: ASTError },
    #[snafu(display("{source}"))]
    Syntax { source: SyntaxError },
    #[snafu(display("{source}"))]
    Load { source: LoadError },
}

impl From<ASTError> for Error {
    fn from(source: ASTError) -> Self {
        Self::Ast { source }
    }
}

impl From<SyntaxError> for Error {
    fn from(source: SyntaxError) -> Self {
        Self::Syntax { source }
    }
}

impl From<LoadError> for Error {
    fn from(source: LoadError) -> Self {
        Self::Load { source }
    }
}

/// Load a simple inline string as the main module and return the statement
pub fn from_str(input: &str) -> std::result::Result<Statement, Error> {
    let mut cursor = Cursor::new(input.as_bytes());
    Ok(StandardLoader::default()
        .add_module("main", &mut cursor, None)?
        .load()?)
}
