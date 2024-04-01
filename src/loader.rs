use crate::error::{self, Result};
use crate::Value;
use snafu::{ensure, ResultExt};
use std::fs::{read_dir, File};
use std::io::Read;
use std::path::{Path, PathBuf};

/// Mode of operation for the configuration
/// loader.
pub enum LoaderMode {
    /// Single will load a single file provided to path
    /// and expects path to be a file.
    Single,
    /// Merge will load a single file by the provided name if it exists
    /// or if there exists a directory named <path>.d will load and merge
    /// all configuration files inside said directory
    Merge,
    /// Append will load a single file by the provided name if it exists
    /// or if there exists a directory named <path>.d will load and append
    /// all configuration files inside said directory
    Append,
}

/// Loader can be used to load a single or directory of configuration files
/// with user control
pub struct Loader {
    mode: LoaderMode,
    discovered: Vec<PathBuf>,
    collisions: bool,
}

impl Loader {
    /// Create a new loader with the default settings
    /// which is:
    ///   mode = Single
    ///   allow_duplicates = false
    ///   allow_collisions = false
    pub fn new() -> Self {
        Self {
            mode: LoaderMode::Single,
            discovered: Vec::new(),
            collisions: false,
        }
    }

    /// Set the loader mode to use
    pub fn mode(&mut self, mode: LoaderMode) -> &mut Self {
        self.mode = mode;
        self
    }

    /// Add a path to this loader
    pub fn path<P>(&mut self, path: P) -> Result<&mut Self>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        ensure!(
            path.try_exists().context(error::IoSnafu)?,
            error::LoaderNotFoundSnafu {
                path: path.to_path_buf()
            }
        );
        match self.mode {
            LoaderMode::Single => {
                ensure!(
                    path.is_file(),
                    error::LoaderNotFileSnafu {
                        path: path.to_path_buf()
                    }
                );
                self.discovered.push(path.to_path_buf());
            }
            _ => {
                if path.is_file() {
                    self.discovered.push(path.to_path_buf());
                } else {
                    let dir_reader = read_dir(path).context(error::IoSnafu)?;

                    for entry in dir_reader {
                        let entry = entry.context(error::IoSnafu)?;
                        let entry_path = entry.path();
                        if entry_path.is_file() {
                            self.discovered.push(entry_path);
                        }
                    }
                }
            }
        }
        Ok(self)
    }

    /// When merging or appending multiple files together
    /// tell the loader to allow collisions and overwrite the first found one with
    /// the next
    pub fn allow_collisions(&mut self) -> &mut Self {
        self.collisions = true;
        self
    }

    /// Load all the configuration files and return everything as
    /// a single module
    pub fn load(&self) -> Result<Value> {
        let mut found = Value::Module(Vec::new());

        match self.mode {
            LoaderMode::Single => {
                let mut file =
                    File::open(self.discovered.first().unwrap()).context(error::IoSnafu)?;
                let mut file_code = String::default();
                file.read_to_string(&mut file_code)
                    .context(error::IoSnafu)?;
                found = crate::from_str(file_code.as_str())?;
            }
            LoaderMode::Merge => {
                if self.discovered.len() == 1 {
                    let mut file =
                        File::open(self.discovered.first().unwrap()).context(error::IoSnafu)?;
                    let mut file_code = String::default();
                    file.read_to_string(&mut file_code)
                        .context(error::IoSnafu)?;
                    found = crate::from_str(file_code.as_str())?;
                } else {
                    for entry in self.discovered.iter() {
                        let mut file = File::open(entry).context(error::IoSnafu)?;
                        let mut file_code = String::default();
                        file.read_to_string(&mut file_code)
                            .context(error::IoSnafu)?;
                        let right = crate::from_str(file_code.as_str())?;
                        self.merge_into(&mut found, &right)?;
                    }
                }
            }
            LoaderMode::Append => {
                if self.discovered.len() == 1 {
                    let mut file =
                        File::open(self.discovered.first().unwrap()).context(error::IoSnafu)?;
                    let mut file_code = String::default();
                    file.read_to_string(&mut file_code)
                        .context(error::IoSnafu)?;
                    found = crate::from_str(file_code.as_str())?;
                } else {
                    for entry in self.discovered.iter() {
                        let mut file = File::open(entry).context(error::IoSnafu)?;
                        let mut file_code = String::default();
                        file.read_to_string(&mut file_code)
                            .context(error::IoSnafu)?;
                        let right = crate::from_str(file_code.as_str())?;
                        self.append_into(&mut found, &right)?;
                    }
                }
            }
        }

        Ok(found)
    }

    fn merge_into(&self, left: &mut Value, right: &Value) -> Result<()> {
        match right {
            Value::Module(right_stmts) => {
                if let Some(left_stmts) = left.as_module_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if let Some(target) = left_stmts.iter_mut().find_map(|x| {
                                if let Some(lid) = x.get_id() {
                                    if lid == rid {
                                        Some(x)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }) {
                                self.merge_into(target, entry)?;
                            } else {
                                left_stmts.push(entry.clone());
                            }
                        }
                    }
                }
            }
            Value::Section { id, statements } => {
                let right_stmts = statements;
                if let Some((_, left_stmts)) = left.as_section_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if let Some(target) = left_stmts.iter_mut().find_map(|x| {
                                if let Some(lid) = x.get_id() {
                                    if lid == rid {
                                        Some(x)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }) {
                                self.merge_into(target, entry)?;
                            } else {
                                left_stmts.push(entry.clone());
                            }
                        }
                    }
                } else {
                    ensure!(self.collisions, error::LoaderMergeCollisionSnafu { id });
                    *left = right.clone();
                }
            }
            Value::Block { id, statements, .. } => {
                let right_stmts = statements;
                if let Some((_, _, left_stmts)) = left.as_block_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if let Some(target) = left_stmts.iter_mut().find_map(|x| {
                                if let Some(lid) = x.get_id() {
                                    if lid == rid {
                                        Some(x)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }) {
                                self.merge_into(target, entry)?;
                            } else {
                                left_stmts.push(entry.clone());
                            }
                        }
                    }
                } else {
                    ensure!(self.collisions, error::LoaderMergeCollisionSnafu { id });
                    *left = right.clone();
                }
            }
            Value::Assignment { label, value } => {
                if let Some((_, left_value)) = left.as_assignment_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu { id: label }
                    );
                    *left = right.clone();
                }
            }
            Value::Control { label, value } => {
                if let Some((_, left_value)) = left.as_control_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu { id: label }
                    );
                    *left = right.clone();
                }
            }
            Value::Table(right_map, ..) => {
                if let Some(left_map) = left.as_table_mut() {
                    for (key, value) in right_map {
                        if let Some(target) = left_map.get_mut(key) {
                            self.merge_into(target, value)?;
                        } else {
                            left_map.insert(key.clone(), value.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu {
                            id: "table definition"
                        }
                    );
                    *left = right.clone();
                }
            }
            _ => {
                // We ignore any none statement
            }
        }
        Ok(())
    }

    fn append_into(&self, left: &mut Value, right: &Value) -> Result<()> {
        match right {
            Value::Module(right_stmts) => {
                if let Some(left_stmts) = left.as_module_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if left_stmts
                                .iter_mut()
                                .find_map(|x| {
                                    if let Some(lid) = x.get_id() {
                                        if lid == rid {
                                            Some(x)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                })
                                .is_some()
                            {
                                ensure!(
                                    self.collisions,
                                    error::LoaderMergeCollisionSnafu { id: rid }
                                );
                            }
                            left_stmts.push(entry.clone());
                        }
                    }
                }
            }
            Value::Section { id, statements } => {
                let right_stmts = statements;
                if let Some((_, left_stmts)) = left.as_section_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if left_stmts
                                .iter_mut()
                                .find_map(|x| {
                                    if let Some(lid) = x.get_id() {
                                        if lid == rid {
                                            Some(x)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                })
                                .is_some()
                            {
                                ensure!(
                                    self.collisions,
                                    error::LoaderMergeCollisionSnafu { id: rid }
                                );
                            }
                            left_stmts.push(entry.clone());
                        }
                    }
                } else {
                    ensure!(self.collisions, error::LoaderMergeCollisionSnafu { id });
                    *left = right.clone();
                }
            }
            Value::Block { id, statements, .. } => {
                let right_stmts = statements;
                if let Some((_, _, left_stmts)) = left.as_block_mut() {
                    for entry in right_stmts {
                        if let Some(rid) = entry.get_id() {
                            if left_stmts
                                .iter_mut()
                                .find_map(|x| {
                                    if let Some(lid) = x.get_id() {
                                        if lid == rid {
                                            Some(x)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                })
                                .is_some()
                            {
                                ensure!(
                                    self.collisions,
                                    error::LoaderMergeCollisionSnafu { id: rid }
                                );
                            }
                            left_stmts.push(entry.clone());
                        }
                    }
                } else {
                    ensure!(self.collisions, error::LoaderMergeCollisionSnafu { id });
                    *left = right.clone();
                }
            }
            Value::Assignment { label, value } => {
                if let Some((_, left_value)) = left.as_assignment_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu { id: label }
                    );
                    *left = right.clone();
                }
            }
            Value::Control { label, value } => {
                if let Some((_, left_value)) = left.as_control_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu { id: label }
                    );
                    *left = right.clone();
                }
            }
            Value::Table(right_map, ..) => {
                if let Some(left_map) = left.as_table_mut() {
                    for (key, value) in right_map {
                        if left_map.get(key).is_some() {
                            ensure!(
                                self.collisions,
                                error::LoaderMergeCollisionSnafu { id: key }
                            );
                        }
                        left_map.insert(key.clone(), value.clone());
                    }
                } else {
                    ensure!(
                        self.collisions,
                        error::LoaderMergeCollisionSnafu {
                            id: "table definition"
                        }
                    );
                    *left = right.clone();
                }
            }
            _ => {
                // We ignore any none statement
            }
        }
        Ok(())
    }
}
