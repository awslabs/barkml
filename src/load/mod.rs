use crate::ast::{Scope, Statement};

mod standard;
mod walk;

use snafu::ResultExt;
pub use standard::*;
pub use walk::*;

/// LoaderInterface defines the shared interface for structs that
/// can read and load barkml files.
pub trait Loader {
    /// Does this loader enable macro resolution and support for macros
    fn is_resolution_enabled(&self) -> bool;
    /// Disable macros for this loader
    fn skip_macro_resolution(&mut self) -> Result<&mut Self>;
    /// Read all barkml configuration files by the loader's configuration and return
    /// the module statement
    fn read(&self) -> Result<Statement>;

    /// Load everything to a module statement.
    /// If macro resolution is enabled, will apply it
    fn load(&self) -> Result<Statement> {
        let mut module = self.read()?;
        if self.is_resolution_enabled() {
            let mut scope = Scope::new(&module);
            module = scope.apply().context(error::ResolutionSnafu)?;
        }
        Ok(module)
    }
}

type Result<T> = std::result::Result<T, error::Error>;

pub(crate) mod error {
    use std::path::PathBuf;

    use snafu::Snafu;

    use crate::ast::Location;

    #[derive(Snafu, Debug)]
    #[snafu(visibility(pub(crate)))]
    pub enum Error {
        #[snafu(display("Could not resolve basename of path"))]
        Basename,
        #[snafu(display(
            "Collision occured between {left_id}({left_location}) and {right_id}({right_location})"
        ))]
        Collision {
            left_id: String,
            left_location: Location,
            right_id: String,
            right_location: Location,
        },
        #[snafu(display("IO error occured during loading: {source}"))]
        Io { source: std::io::Error },
        #[snafu(display("Could not find file named {name}.bml or directory named {name}.d in any of the search paths:\n{}", search_paths.iter().map(|x| x.to_string_lossy().to_string()).collect::<Vec<_>>().join("\n")))]
        Search {
            name: String,
            search_paths: Vec<PathBuf>,
        },
        #[snafu(display("{location} - no element with index '{index}'"))]
        NoElement { location: Location, index: usize },
        #[snafu(display("{location} - field not found: {field}"))]
        NoField { location: Location, field: String },
        #[snafu(display("{location} - field with name '{field}' is not a value"))]
        NoValue { location: Location, field: String },
        #[snafu(display(
            "No main module defined, the standard loader requires at least one main module to load"
        ))]
        NoMain,
        #[snafu(display("No such file or directory '{}'", path.display()))]
        NotFound { path: PathBuf },
        #[snafu(display("{location} - not a scope with fields"))]
        NotScope { location: Location },
        #[snafu(display("Error occured parsing barkml.\n{source}"))]
        Parse { source: crate::syn::error::Error },
        #[snafu(display("Error occured during macro resolution.\n{source}"))]
        Resolution { source: crate::ast::error::Error },
    }
}
