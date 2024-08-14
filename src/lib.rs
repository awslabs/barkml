#![allow(clippy::approx_constant)]
#![allow(clippy::from_str_radix_10)]
pub use loader::*;

pub mod error;
mod lang;
mod loader;

pub use lang::*;
/// Use standard loader settings to parse a BarkML file into a module
pub fn from_str(input: &str) -> error::Result<Statement> {
    StandardLoader::default().source("unknown", input)?.load()
}
