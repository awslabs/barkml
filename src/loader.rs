use snafu::{ensure, OptionExt};
use std::collections::BTreeMap;
use std::fs::{read_dir, File};
use std::io::{Read, Seek};
use std::path::Path;

use crate::error::{self, Result};
use crate::lang::from_str;
use crate::r#macro::Scope;
use crate::{Data, Value};

/// LoaderInterface defines the shared interface for loaders
pub trait Loader {
    fn is_resolution_enabled(&self) -> bool;
    fn is_collision_allowed(&self) -> bool;
    fn skip_macro_resolution(&mut self) -> Result<&mut Self>;
    fn mode(&mut self, mode: LoaderMode) -> Result<&mut Self>;
    fn allow_collisions(&mut self) -> Result<&mut Self>;
    fn read(&self) -> Result<Value>;

    fn macro_resolution(&self, module: &Value) -> Result<Value> {
        if self.is_resolution_enabled() {
            let mut scope = Scope::new(module)?;
            scope.apply()
        } else {
            Ok(module.clone())
        }
    }

    fn load(&self) -> Result<Value> {
        let module = self.read()?;
        self.macro_resolution(&module)
    }

    fn merge_into(&self, left: &mut Value, right: &Value) -> Result<()> {
        match right.inner() {
            Data::Module(right_stmts) => {
                if let Some(left_stmts) = left.as_module_mut() {
                    for (key, value) in right_stmts {
                        if let Some(target) = left_stmts.get_mut(key) {
                            self.merge_into(target, value)?;
                        } else {
                            left_stmts.insert(key.clone(), value.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu {
                            id: left.id().unwrap()
                        }
                    );
                    *left = right.clone();
                }
            }
            Data::Section(right_stmts) => {
                if let Some(left_stmts) = left.as_section_mut() {
                    for (key, value) in right_stmts {
                        if let Some(target) = left_stmts.get_mut(key) {
                            self.merge_into(target, value)?;
                        } else {
                            left_stmts.insert(key.clone(), value.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu {
                            id: left.id().unwrap()
                        }
                    );
                    *left = right.clone();
                }
            }
            Data::Block { children, .. } => {
                let right_stmts = children;
                if let Some((_, left_stmts)) = left.as_block_mut() {
                    for (key, value) in right_stmts {
                        if let Some(target) = left_stmts.get_mut(key) {
                            self.merge_into(target, value)?;
                        } else {
                            left_stmts.insert(key.clone(), value.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu {
                            id: left.id().unwrap()
                        }
                    );
                    *left = right.clone();
                }
            }
            Data::Assignment(value) => {
                if let Some(left_value) = left.as_assignment_mut() {
                    self.merge_into(left_value, value)?;
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu {
                            id: left.id().unwrap()
                        }
                    );
                    *left = right.clone();
                }
            }
            Data::Control(value) => {
                if let Some(left_value) = left.as_control_mut() {
                    self.merge_into(left_value, value)?;
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu {
                            id: left.id().unwrap()
                        }
                    );
                    *left = right.clone();
                }
            }
            Data::Table(right_map) => {
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
                        self.is_collision_allowed(),
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
}

/// Loader can be used to load a single or directory of configuration files
/// with user control
pub struct StandardLoader {
    mode: LoaderMode,
    modules: BTreeMap<String, String>,
    collisions: bool,
    resolve_macros: bool,
}

impl Default for StandardLoader {
    /// Create a new loader with the default settings
    /// which is:
    ///   mode = Single
    ///   allow_collisions = false
    ///   resolve_macros = true
    fn default() -> Self {
        Self {
            mode: LoaderMode::Single,
            modules: BTreeMap::new(),
            collisions: false,
            resolve_macros: true,
        }
    }
}

impl StandardLoader {
    fn add_from_file<P>(&mut self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        let name = basename(path)?;
        let mut file = File::open(path).map_err(|e| error::Error::Io {
            reason: e.to_string(),
        })?;
        self.add_from_reader(name.as_str(), &mut file)
    }

    fn add_from_reader<R>(&mut self, name: &str, reader: &mut R) -> Result<()>
    where
        R: Read + Seek,
    {
        let mut code = String::default();
        reader
            .read_to_string(&mut code)
            .map_err(|e| error::Error::Io {
                reason: e.to_string(),
            })?;
        self.modules.insert(name.to_string(), code.clone());
        Ok(())
    }

    /// Add a path to this loader
    pub fn path<P>(&mut self, path: P) -> Result<&mut Self>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        ensure!(
            path.try_exists().map_err(|e| error::Error::Io {
                reason: e.to_string()
            })?,
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
                self.add_from_file(path)?;
            }
            _ => {
                if path.is_file() {
                    self.add_from_file(path)?;
                } else {
                    // To fix inconsistencies we need to add to sorted list
                    let dir_reader = read_dir(path).map_err(|e| error::Error::Io {
                        reason: e.to_string(),
                    })?;
                    let mut files = Vec::new();
                    for entry in dir_reader {
                        let entry = entry.map_err(|e| error::Error::Io {
                            reason: e.to_string(),
                        })?;
                        let entry_path = entry.path();
                        if entry_path.is_file() {
                            files.push(entry_path.clone());
                        }
                    }
                    files.sort();
                    for file in files.iter() {
                        self.add_from_file(file)?;
                    }
                }
            }
        }
        Ok(self)
    }

    /// Adds an inline string as a configuration module
    pub fn source(&mut self, input: &str) -> Result<&mut Self> {
        ensure!(
            !self.modules.contains_key("root"),
            error::LoaderModuleCollisionSnafu { name: "root" }
        );
        self.modules.insert("root".to_string(), input.to_string());
        Ok(self)
    }
}

impl Loader for StandardLoader {
    fn is_resolution_enabled(&self) -> bool {
        self.resolve_macros
    }

    fn is_collision_allowed(&self) -> bool {
        self.collisions
    }

    fn skip_macro_resolution(&mut self) -> Result<&mut Self> {
        self.resolve_macros = false;
        Ok(self)
    }

    /// Set the loader mode to use
    fn mode(&mut self, mode: LoaderMode) -> Result<&mut Self> {
        self.mode = mode;
        Ok(self)
    }

    /// When merging or appending multiple files together
    /// tell the loader to allow collisions and overwrite the first found one with
    /// the next
    fn allow_collisions(&mut self) -> Result<&mut Self> {
        self.collisions = true;
        Ok(self)
    }

    /// Load all the configuration files and return everything as
    /// a single module
    fn read(&self) -> Result<Value> {
        ensure!(!self.modules.is_empty(), error::LoaderNoModulesSnafu);
        let code = self
            .modules
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        from_str(code.as_str())
    }
}

fn basename<P: AsRef<Path>>(path: P) -> Result<String> {
    let file_name = path
        .as_ref()
        .file_name()
        .context(error::LoaderModuleParseSnafu)?;
    let file_name = file_name.to_str().context(error::LoaderModuleParseSnafu)?;
    let extension = path
        .as_ref()
        .extension()
        .context(error::LoaderModuleParseSnafu)?;
    let extension = extension.to_str().context(error::LoaderModuleParseSnafu)?;
    Ok(file_name.strip_suffix(extension).unwrap().to_string())
}

#[cfg(test)]
mod test {
    use indexmap::IndexMap;

    use assert_matches::assert_matches;

    use crate::loader::{LoaderMode, StandardLoader};
    use crate::{Loader, Value};

    #[test]
    pub fn load_single() {
        let _expected = Value::new_module(
            IndexMap::from([
                (
                    "schema".into(),
                    Value::new_control(
                        "schema".into(),
                        Value::new_string("1.0.0".into(), Some("Test".to_string()), None).unwrap(),
                        None,
                        None,
                    )
                    .unwrap(),
                ),
                (
                    "section-a".into(),
                    Value::new_section(
                        "section-a".into(),
                        IndexMap::from([
                            (
                                "number".into(),
                                Value::new_assignment(
                                    "number".into(),
                                    Value::new_int(4, None, None).unwrap(),
                                    Some("Documentation".to_string()),
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "float".into(),
                                Value::new_assignment(
                                    "float".into(),
                                    Value::new_float(3.14, None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "string".into(),
                                Value::new_assignment(
                                    "string".into(),
                                    Value::new_string("foobar".into(), None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "array".to_string(),
                                Value::new_assignment(
                                    "array".into(),
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), None, None).unwrap(),
                                            Value::new_int(5, None, None).unwrap(),
                                            Value::new_float(3.14, None, None).unwrap(),
                                            Value::new_string("single".into(), None, None).unwrap(),
                                        ],
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Value::new_assignment(
                                    "object".into(),
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    None,
                                                    None,
                                                )
                                                .unwrap(),
                                            ),
                                            ("bar".into(), Value::new_int(4, None, None).unwrap()),
                                        ]),
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "block".into(),
                                Value::new_block(
                                    "block".into(),
                                    vec!["block-a".to_string(), "label-a".to_string()],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Value::new_string("me".into(), None, None).unwrap(),
                                    )]),
                                    None,
                                )
                                .unwrap(),
                            ),
                        ]),
                        Some("Documentation".to_string()),
                    )
                    .unwrap(),
                ),
                (
                    "section-b".into(),
                    Value::new_section(
                        "section-b".into(),
                        IndexMap::from([
                            (
                                "number".into(),
                                Value::new_assignment(
                                    "number".into(),
                                    Value::new_int(4, None, None).unwrap(),
                                    Some("Documentation".to_string()),
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "float".into(),
                                Value::new_assignment(
                                    "float".into(),
                                    Value::new_float(3.14, None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "string".into(),
                                Value::new_assignment(
                                    "string".into(),
                                    Value::new_string("foobar".into(), None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "array".to_string(),
                                Value::new_assignment(
                                    "array".into(),
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), None, None).unwrap(),
                                            Value::new_int(5, None, None).unwrap(),
                                            Value::new_float(3.14, None, None).unwrap(),
                                            Value::new_string("single".into(), None, None).unwrap(),
                                        ],
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Value::new_assignment(
                                    "object".into(),
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    None,
                                                    None,
                                                )
                                                .unwrap(),
                                            ),
                                            ("bar".into(), Value::new_int(4, None, None).unwrap()),
                                        ]),
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "block".into(),
                                Value::new_block(
                                    "block".into(),
                                    vec!["block-a".to_string(), "label-a".to_string()],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Value::new_string("me".into(), None, None).unwrap(),
                                    )]),
                                    None,
                                )
                                .unwrap(),
                            ),
                        ]),
                        Some("Documentation".to_string()),
                    )
                    .unwrap(),
                ),
            ]),
            None,
        );
        let result = StandardLoader::default()
            .mode(LoaderMode::Single)
            .expect("failed to set mode")
            .path("examples/example.bml")
            .expect("path detection failed")
            .load()
            .expect("load failed");
        let children = result.as_module().expect("was not a module");
        assert_matches!(children, _expected);
    }

    #[test]
    pub fn load_multiple() {
        let _expected = Value::new_module(
            IndexMap::from([
                (
                    "schema".into(),
                    Value::new_control(
                        "schema".into(),
                        Value::new_string("1.0.0".into(), Some("Test".to_string()), None).unwrap(),
                        None,
                        None,
                    )
                    .unwrap(),
                ),
                (
                    "section-a".into(),
                    Value::new_section(
                        "section-a".into(),
                        IndexMap::from([
                            (
                                "number".into(),
                                Value::new_assignment(
                                    "number".into(),
                                    Value::new_int(4, None, None).unwrap(),
                                    Some("Documentation".to_string()),
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "float".into(),
                                Value::new_assignment(
                                    "float".into(),
                                    Value::new_float(3.14, None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "string".into(),
                                Value::new_assignment(
                                    "string".into(),
                                    Value::new_string("foobar".into(), None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "array".to_string(),
                                Value::new_assignment(
                                    "array".into(),
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), None, None).unwrap(),
                                            Value::new_int(5, None, None).unwrap(),
                                            Value::new_float(3.14, None, None).unwrap(),
                                            Value::new_string("single".into(), None, None).unwrap(),
                                        ],
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Value::new_assignment(
                                    "object".into(),
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    None,
                                                    None,
                                                )
                                                .unwrap(),
                                            ),
                                            ("bar".into(), Value::new_int(4, None, None).unwrap()),
                                        ]),
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "block".into(),
                                Value::new_block(
                                    "block".into(),
                                    vec!["block-a".to_string(), "label-a".to_string()],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Value::new_string("me".into(), None, None).unwrap(),
                                    )]),
                                    None,
                                )
                                .unwrap(),
                            ),
                        ]),
                        Some("Documentation".to_string()),
                    )
                    .unwrap(),
                ),
                (
                    "section-b".into(),
                    Value::new_section(
                        "section-b".into(),
                        IndexMap::from([
                            (
                                "number".into(),
                                Value::new_assignment(
                                    "number".into(),
                                    Value::new_int(4, None, None).unwrap(),
                                    Some("Documentation".to_string()),
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "float".into(),
                                Value::new_assignment(
                                    "float".into(),
                                    Value::new_float(3.14, None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "string".into(),
                                Value::new_assignment(
                                    "string".into(),
                                    Value::new_string("foobar".into(), None, None).unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "array".to_string(),
                                Value::new_assignment(
                                    "array".into(),
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), None, None).unwrap(),
                                            Value::new_int(5, None, None).unwrap(),
                                            Value::new_float(3.14, None, None).unwrap(),
                                            Value::new_string("single".into(), None, None).unwrap(),
                                        ],
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Value::new_assignment(
                                    "object".into(),
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    None,
                                                    None,
                                                )
                                                .unwrap(),
                                            ),
                                            ("bar".into(), Value::new_int(4, None, None).unwrap()),
                                        ]),
                                        None,
                                        None,
                                    )
                                    .unwrap(),
                                    None,
                                    None,
                                )
                                .unwrap(),
                            ),
                            (
                                "block".into(),
                                Value::new_block(
                                    "block".into(),
                                    vec!["block-a".to_string(), "label-a".to_string()],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Value::new_string("me".into(), None, None).unwrap(),
                                    )]),
                                    None,
                                )
                                .unwrap(),
                            ),
                        ]),
                        Some("Documentation".to_string()),
                    )
                    .unwrap(),
                ),
            ]),
            None,
        );
        let result = StandardLoader::default()
            .mode(LoaderMode::Merge)
            .expect("failed to set mode")
            .path("examples/config_append")
            .expect("path detection failed")
            .load()
            .expect("load failed");
        let children = result.as_module().expect("was not a module");
        assert_matches!(children, _expected);
    }
}
