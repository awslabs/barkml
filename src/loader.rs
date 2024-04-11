use std::cmp::max;
use std::collections::{BTreeMap, HashMap};
use std::fs::{read_dir, File};
use std::io::{Read, Seek};
use std::path::Path;

use snafu::{ensure, OptionExt, ResultExt};

use crate::error::{self, Result};
use crate::Value;

/// LoaderInterface defines the shared interface for loaders
pub trait Loader {
    fn is_resolution_enabled(&self) -> bool;
    fn is_collision_allowed(&self) -> bool;
    fn skip_macro_resolution(&mut self) -> Result<&mut Self>;
    fn mode(&mut self, mode: LoaderMode) -> Result<&mut Self>;
    fn allow_collisions(&mut self) -> Result<&mut Self>;
    fn read(&self) -> Result<Value>;

    fn macro_resolution(&self, module: &mut Value) -> Result<()> {
        if self.is_resolution_enabled() {
            let statements = module.as_module_mut().unwrap();
            resolve_macros(statements.as_mut_slice(), &mut HashMap::new(), None)?;
        }
        Ok(())
    }

    fn load(&self) -> Result<Value> {
        let mut module = self.read()?;
        self.macro_resolution(&mut module)?;
        Ok(module)
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
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu { id }
                    );
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
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu { id }
                    );
                    *left = right.clone();
                }
            }
            Value::Assignment { label, value } => {
                if let Some((_, left_value)) = left.as_assignment_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.is_collision_allowed(),
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
                        self.is_collision_allowed(),
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
                                    self.is_collision_allowed(),
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
                                    self.is_collision_allowed(),
                                    error::LoaderMergeCollisionSnafu { id: rid }
                                );
                            }
                            left_stmts.push(entry.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu { id }
                    );
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
                                    self.is_collision_allowed(),
                                    error::LoaderMergeCollisionSnafu { id: rid }
                                );
                            }
                            left_stmts.push(entry.clone());
                        }
                    }
                } else {
                    ensure!(
                        self.is_collision_allowed(),
                        error::LoaderMergeCollisionSnafu { id }
                    );
                    *left = right.clone();
                }
            }
            Value::Assignment { label, value } => {
                if let Some((_, left_value)) = left.as_assignment_mut() {
                    self.merge_into(left_value.as_mut(), value.as_ref())?;
                } else {
                    ensure!(
                        self.is_collision_allowed(),
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
                        self.is_collision_allowed(),
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
                                self.is_collision_allowed(),
                                error::LoaderMergeCollisionSnafu { id: key }
                            );
                        }
                        left_map.insert(key.clone(), value.clone());
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
    /// Append will load a single file by the provided name if it exists
    /// or if there exists a directory named <path>.d will load and append
    /// all configuration files inside said directory
    Append,
}

/// Loader can be used to load a single or directory of configuration files
/// with user control
pub struct StandardLoader {
    mode: LoaderMode,
    modules: BTreeMap<String, Value>,
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
        let mut file = File::open(path).context(error::IoSnafu)?;
        self.add_from_reader(name.as_str(), &mut file)
    }

    fn add_from_reader<R>(&mut self, name: &str, reader: &mut R) -> Result<()>
    where
        R: Read + Seek,
    {
        let mut code = String::default();
        reader.read_to_string(&mut code).context(error::IoSnafu)?;
        let module = crate::idl::parse(code.as_str())?;
        self.modules.insert(name.to_string(), module);
        Ok(())
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
                self.add_from_file(path)?;
            }
            _ => {
                if path.is_file() {
                    self.add_from_file(path)?;
                } else {
                    // To fix inconsistencies we need to add to sorted list
                    let dir_reader = read_dir(path).context(error::IoSnafu)?;
                    let mut files = Vec::new();
                    for entry in dir_reader {
                        let entry = entry.context(error::IoSnafu)?;
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
        let value = crate::idl::parse(input)?;
        self.modules.insert("root".to_string(), value);
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
        let mut found = Value::Module(Vec::new());
        ensure!(!self.modules.is_empty(), error::LoaderNoModulesSnafu);

        match self.mode {
            LoaderMode::Single => {
                let module_list: Vec<&Value> = self.modules.values().collect();
                found = module_list[0].clone();
            }
            LoaderMode::Merge => {
                for (_, right) in self.modules.iter() {
                    self.merge_into(&mut found, right)?;
                }
            }
            LoaderMode::Append => {
                for (_, right) in self.modules.iter() {
                    self.append_into(&mut found, right)?;
                }
            }
        }

        Ok(found)
    }
}

fn resolve_macros(
    input: &mut [Value],
    symbol_table: &mut HashMap<String, Value>,
    prefix: Option<String>,
) -> Result<()> {
    for stmt in input.iter_mut() {
        match stmt {
            Value::Section { id, statements } => {
                resolve_macros(
                    statements,
                    symbol_table,
                    if let Some(prefix) = prefix.as_ref() {
                        Some(format!("{}.{}", prefix, id))
                    } else {
                        Some(id.clone())
                    },
                )?;
            }
            Value::Block {
                id,
                labels,
                statements,
                ..
            } => {
                let mut new_labels = labels.clone();
                resolve_macros(
                    statements,
                    symbol_table,
                    if let Some(prefix) = prefix.as_ref() {
                        let mut items = vec![prefix.clone(), id.clone()];
                        items.append(&mut new_labels);
                        Some(items.join("."))
                    } else {
                        let mut items = vec![id.clone()];
                        items.append(&mut new_labels);
                        Some(items.join("."))
                    },
                )?;
            }
            Value::Assignment { label, value } => {
                resolve_macro(
                    value,
                    symbol_table,
                    if let Some(prefix) = prefix.as_ref() {
                        Some(format!("{}.{}", prefix, label))
                    } else {
                        Some(label.clone())
                    },
                )?;
            }
            Value::Control { label, value } => {
                resolve_macro(
                    value,
                    symbol_table,
                    if let Some(prefix) = prefix.as_ref() {
                        Some(format!("{}.${}", prefix, label))
                    } else {
                        Some(format!("${}", label))
                    },
                )?;
            }
            value => {
                resolve_macro(value, symbol_table, prefix.clone())?;
            }
        }
    }
    Ok(())
}

fn macrotize(
    input: &str,
    symbol_table: &mut HashMap<String, Value>,
    prefix: Option<String>,
) -> Result<String> {
    let mut start_index = -1;
    let original = input.to_string();
    let mut final_string = original.clone();
    let mut offset: i64 = 0;
    let mut count = 0;
    for (i, c) in input.chars().enumerate() {
        if c == '{' && start_index == -1 {
            start_index = i as i64;
        } else if c == '}' && start_index != -1 {
            count += 1;
            let copy = original.clone();
            let (before, after) = copy.split_at(start_index as usize);
            let (middle, _) = after.split_at(i - before.len());
            let mut key = middle[1..].to_string();
            if key.starts_with("self.") {
                key = key.strip_prefix("self.").unwrap().to_string();
                if let Some(prefix) = prefix.as_ref() {
                    let segments: Vec<&str> = prefix.split('.').collect();
                    let prefix = segments[..segments.len() - 1].join(".");
                    key = format!("{}.{}", prefix, key);
                }
            } else if key.starts_with("super.") {
                if let Some(prefix) = prefix.as_ref() {
                    let segments: Vec<&str> = prefix.split('.').collect();
                    let mut new_prefix = segments[..segments.len() - 1].join(".");
                    while key.starts_with("super.") {
                        key = key.strip_prefix("super.").unwrap().to_string();
                        let segments: Vec<&str> = new_prefix.split('.').collect();
                        new_prefix = segments[..segments.len() - 1].join(".");
                    }
                    key = format!("{}.{}", new_prefix, key);
                }
            }
            let replacement = match symbol_table.get(&key) {
                Some(data) => Ok(data.to_macro_string()),
                None => Err(error::Error::MacroNotFound { path: key }),
            }?;
            let sindex = if offset == 0 {
                i as i64 - count
            } else {
                offset + 1
            };
            let (before, rem) = final_string.split_at(max(sindex, 0) as usize);
            let (_, mut after) = rem.split_at((count + 1) as usize);
            after = if after.starts_with('}') {
                after.strip_prefix('}').unwrap()
            } else {
                after
            };
            final_string = before.to_string() + replacement.as_str() + after;
            offset += if replacement.len() >= count as usize {
                replacement.len() as i64
            } else {
                replacement.len() as i64 - count
            };
            start_index = -1;
            count = 0;
        } else if start_index != -1 {
            count += 1;
        }
    }
    Ok(final_string)
}

fn resolve_macro(
    value: &mut Value,
    symbol_table: &mut HashMap<String, Value>,
    prefix: Option<String>,
) -> Result<()> {
    match value {
        Value::Macro(pattern, label, is_string) => {
            let label = label.clone();
            if *is_string {
                let string = macrotize(pattern.as_str(), symbol_table, prefix.clone())?;
                *value = Value::String(string.clone(), label.clone());
            } else {
                *value = match symbol_table.get(pattern) {
                    Some(data) => Ok(data.clone()),
                    None => Err(error::Error::MacroNotFound {
                        path: pattern.clone(),
                    }),
                }?;
                if let Some(label) = label.as_ref() {
                    value.set_label(label.as_str());
                }
            }

            if let Some(prefix) = prefix.as_ref() {
                symbol_table.insert(prefix.clone(), value.clone());
            }
        }
        Value::Array(array, _) => {
            for (i, item) in array.iter_mut().enumerate() {
                resolve_macro(
                    item,
                    symbol_table,
                    prefix.as_ref().map(|prefix| format!("{}[{}]", prefix, i)),
                )?;
            }
        }
        Value::Table(table, _) => {
            for (key, value) in table.iter_mut() {
                resolve_macro(
                    value,
                    symbol_table,
                    prefix.as_ref().map(|prefix| format!("{}.{}", prefix, key)),
                )?;
            }
        }
        _ => {
            if let Some(prefix) = prefix.as_ref() {
                symbol_table.insert(prefix.clone(), value.clone());
            }
        }
    }
    Ok(())
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
    use std::collections::HashMap;

    use assert_matches::assert_matches;

    use crate::loader::{LoaderMode, StandardLoader};
    use crate::{Float, Int, Loader};

    #[test]
    pub fn load_single() {
        let _expected = vec![
            crate::Value::Control {
                label: "schema".to_string(),
                value: Box::new(crate::Value::String(
                    "1.0.0".to_string(),
                    Some("Test".to_string()),
                )),
            },
            crate::Value::Section {
                id: "section-a".to_string(),
                statements: vec![
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Assignment {
                        label: "number".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(4), None)),
                    },
                    crate::Value::Assignment {
                        label: "float".to_string(),
                        value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                    },
                    crate::Value::Assignment {
                        label: "string".to_string(),
                        value: Box::new(crate::Value::String("foobar".to_string(), None)),
                    },
                    crate::Value::Assignment {
                        label: "array".to_string(),
                        value: Box::new(crate::Value::Array(
                            vec![
                                crate::Value::String("hello".to_string(), None),
                                crate::Value::Int(Int::I64(5), None),
                                crate::Value::Float(Float::F64(3.14), None),
                                crate::Value::String("single".to_string(), None),
                            ],
                            None,
                        )),
                    },
                    crate::Value::Assignment {
                        label: "object".to_string(),
                        value: Box::new(crate::Value::Table(
                            HashMap::from([
                                (
                                    "foo".to_string(),
                                    crate::Value::Bytes(b"binarystring".to_vec(), None),
                                ),
                                ("bar".to_string(), crate::Value::Int(Int::I64(4), None)),
                            ]),
                            None,
                        )),
                    },
                    crate::Value::Block {
                        id: "block".to_string(),
                        labels: vec!["block-a".to_string(), "label-a".to_string()],
                        statements: vec![crate::Value::Assignment {
                            label: "simple".to_string(),
                            value: Box::new(crate::Value::String("me".to_string(), None)),
                        }],
                    },
                ],
            },
            crate::Value::Section {
                id: "section-b".to_string(),
                statements: vec![
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Assignment {
                        label: "number".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(4), None)),
                    },
                    crate::Value::Assignment {
                        label: "float".to_string(),
                        value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                    },
                    crate::Value::Assignment {
                        label: "string".to_string(),
                        value: Box::new(crate::Value::String("foobar".to_string(), None)),
                    },
                    crate::Value::Assignment {
                        label: "array".to_string(),
                        value: Box::new(crate::Value::Array(
                            vec![
                                crate::Value::String("hello".to_string(), None),
                                crate::Value::Int(Int::I64(5), None),
                                crate::Value::Float(Float::F64(3.14), None),
                                crate::Value::String("single".to_string(), None),
                            ],
                            None,
                        )),
                    },
                    crate::Value::Assignment {
                        label: "object".to_string(),
                        value: Box::new(crate::Value::Table(
                            HashMap::from([
                                (
                                    "foo".to_string(),
                                    crate::Value::Bytes(b"binarystring".to_vec(), None),
                                ),
                                ("bar".to_string(), crate::Value::Int(Int::I64(4), None)),
                            ]),
                            None,
                        )),
                    },
                    crate::Value::Block {
                        id: "block".to_string(),
                        labels: vec!["block-a".to_string(), "label-a".to_string()],
                        statements: vec![
                            crate::Value::Assignment {
                                label: "simple".to_string(),
                                value: Box::new(crate::Value::String(
                                    "foobar bar".to_string(),
                                    None,
                                )),
                            },
                            crate::Value::Assignment {
                                label: "replacement".to_string(),
                                value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                            },
                        ],
                    },
                ],
            },
        ];
        let result = StandardLoader::default()
            .mode(LoaderMode::Single)
            .expect("failed to set mode")
            .path("examples/example.bml")
            .expect("path detection failed")
            .load()
            .expect("load failed");
        let children = result.as_module().expect("was not a module");
        assert_matches!(children, expected);
    }

    #[test]
    fn load_append() {
        StandardLoader::default()
            .mode(LoaderMode::Append)
            .expect("failed to set mode")
            .path("examples/config_append")
            .expect("path detection failed")
            .load()
            .expect("load failed");
    }
}
