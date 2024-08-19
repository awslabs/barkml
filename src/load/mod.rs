use crate::ast::{Scope, Statement};
use crate::{error, Result};

mod standard;
mod walk;

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
            module = scope.apply()?;
        }
        Ok(module)
    }
}
