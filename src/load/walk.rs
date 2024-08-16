use super::{error, Result};
use crate::ast::{Statement, Value};
use indexmap::IndexSet;
use snafu::OptionExt;

/// Implements a statement walker for easily fetching data out of barkml statements
/// and values. It is recommended to use this after you have used a standard loader as it
/// is far easier to get data and gives users flexibility in how to define the contents of a barkml
/// file.
pub enum Walk<'source> {
    Statement(&'source Statement),
    Value(&'source Value),
}

impl<'source> Walk<'source> {
    /// Create a new walk object over the given module
    pub fn new(module: &'source Statement) -> Self {
        Self::Statement(module)
    }

    /// Fetch and convert the value of a given field
    pub fn get<T>(&self, field: &str) -> Result<T>
    where
        T: TryFrom<&'source Value, Error = error::Error>,
    {
        match self {
            Self::Statement(stmt) => stmt
                .get_grouped()
                .or(stmt.get_labeled().map(|x| x.1))
                .context(error::NotScopeSnafu {
                    location: stmt.meta.location.clone(),
                })?
                .get(field)
                .context(error::NoFieldSnafu {
                    location: stmt.meta.location.clone(),
                    field,
                })?
                .get_value()
                .context(error::NoValueSnafu {
                    location: stmt.meta.location.clone(),
                    field,
                })?
                .try_into(),
            Self::Value(value) => value
                .as_table()
                .context(error::NotScopeSnafu {
                    location: value.meta.location.clone(),
                })?
                .get(field)
                .context(error::NoFieldSnafu {
                    location: value.meta.location.clone(),
                    field,
                })?
                .try_into(),
        }
    }

    /// Fetch and convert the label of a block
    pub fn get_label<T>(&self, index: usize) -> Result<T>
    where
        T: TryFrom<&'source Value, Error = error::Error>,
    {
        match self {
            Self::Statement(stmt) => stmt
                .get_labeled()
                .context(error::NotScopeSnafu {
                    location: stmt.meta.location.clone(),
                })?
                .0
                .get(index)
                .context(error::NoElementSnafu {
                    location: stmt.meta.location.clone(),
                    index,
                })?
                .try_into(),
            Self::Value(value) => error::NotScopeSnafu {
                location: value.meta.location.clone(),
            }
            .fail(),
        }
    }

    /// Get all blocks in this scope with the id field. This will return the fully resolved scope
    /// id of each block.
    pub fn get_blocks(&self, field: &str) -> Result<IndexSet<String>> {
        match self {
            Self::Statement(stmt) => Ok(stmt
                .get_grouped()
                .or(stmt.get_labeled().map(|x| x.1))
                .context(error::NotScopeSnafu {
                    location: stmt.meta.location.clone(),
                })?
                .into_iter()
                .filter_map(|(k, s)| {
                    if s.get_labeled().is_some() && s.id == field {
                        Some(k.clone())
                    } else {
                        None
                    }
                })
                .collect()),
            Self::Value(value) => error::NotScopeSnafu {
                location: value.meta.location.clone(),
            }
            .fail(),
        }
    }

    /// Create a walker over a scopeable statement inside this one
    pub fn walk(&self, field: &str) -> Result<Self> {
        match self {
            Self::Statement(stmt) => Ok(
                if let Some(children) = stmt.get_grouped().or(stmt.get_labeled().map(|x| x.1)) {
                    Self::new(children.get(field).context(error::NoFieldSnafu {
                        location: stmt.meta.location.clone(),
                        field,
                    })?)
                } else {
                    Self::Value(stmt.get_value().unwrap())
                },
            ),
            Self::Value(value) => Ok(Self::Value(
                value
                    .as_table()
                    .context(error::NotScopeSnafu {
                        location: value.meta.location.clone(),
                    })?
                    .get(field)
                    .context(error::NoFieldSnafu {
                        location: value.meta.location.clone(),
                        field,
                    })?,
            )),
        }
    }
}
