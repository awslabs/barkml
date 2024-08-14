use indexmap::IndexMap;
use snafu::{ensure, OptionExt, ResultExt};
use std::fs::{read_dir, File};
use std::io::{Cursor, Read, Seek};
use std::path::Path;

use crate::error::{self, Result};
use crate::lang::from_str;
use crate::lang::scope::Scope;
use crate::{Metadata, Statement, StatementData};

/// LoaderInterface defines the shared interface for loaders
pub trait Loader {
    fn is_resolution_enabled(&self) -> bool;
    fn is_collision_allowed(&self) -> bool;
    fn skip_macro_resolution(&mut self) -> Result<&mut Self>;
    fn mode(&mut self, mode: LoaderMode) -> Result<&mut Self>;
    fn allow_collisions(&mut self) -> Result<&mut Self>;
    fn read(&self) -> Result<Statement>;

    fn macro_resolution(&self, module: &Statement) -> Result<Statement> {
        if self.is_resolution_enabled() {
            let mut scope = Scope::new(module);
            scope.apply()
        } else {
            Ok(module.clone())
        }
    }

    fn load(&self) -> Result<Statement> {
        let module = self.read()?;
        self.macro_resolution(&module)
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

pub enum Modules {
    Empty,
    Single(Statement),
    Merged(Statement),
}

impl Modules {
    pub fn get(&self) -> Statement {
        match self {
            Self::Empty => Statement::new_module(".", IndexMap::new(), Metadata::default()),
            Self::Single(value) => value.clone(),
            Self::Merged(value) => value.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    pub fn add(&mut self, module: &Statement, is_collision_allowed: bool) -> Result<()> {
        match self {
            Self::Empty => Ok(()),
            Self::Single(_) => Ok(()),
            Self::Merged(value) => {
                let mut before = value.clone();
                Self::merge_into(&mut before, module, is_collision_allowed)?;
                *value = before;
                Ok(())
            }
        }
    }

    fn merge_into(
        left: &mut Statement,
        right: &Statement,
        is_collision_allowed: bool,
    ) -> Result<()> {
        match &right.data {
            StatementData::Group(right_stmts) | StatementData::Labeled(_, right_stmts) => {
                match &mut left.data {
                    StatementData::Group(ref mut left_stmts)
                    | StatementData::Labeled(_, ref mut left_stmts) => {
                        for (key, value) in right_stmts {
                            if let Some(target) = left_stmts.get_mut(key) {
                                Self::merge_into(target, value, is_collision_allowed)?;
                            } else {
                                left_stmts.insert(key.clone(), value.clone());
                            }
                        }
                    }
                    _ => {
                        ensure!(
                            is_collision_allowed,
                            error::LoaderMergeCollisionSnafu {
                                id: left.id.clone()
                            }
                        );
                        *left = right.clone();
                    }
                }
            }
            StatementData::Single(_) => {
                ensure!(
                    is_collision_allowed,
                    error::LoaderMergeCollisionSnafu {
                        id: left.id.clone()
                    }
                );
                *left = right.clone();
            }
        }
        Ok(())
    }
}

/// Loader can be used to load a single or directory of configuration files
/// with user control
pub struct StandardLoader {
    mode: LoaderMode,
    module: Modules,
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
            module: Modules::Empty,
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
        let module = from_str(code.as_str()).context(error::ParseSnafu { name })?;
        if self.module.is_empty() {
            match self.mode {
                LoaderMode::Single => self.module = Modules::Single(module),
                LoaderMode::Merge => self.module = Modules::Merged(module),
            }
        } else {
            self.module.add(&module, self.is_collision_allowed())?;
        }
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
    pub fn source(&mut self, name: &str, input: &str) -> Result<&mut Self> {
        let code_bytes = input.as_bytes().to_vec();
        let mut cursor = Cursor::new(code_bytes);
        self.add_from_reader(name, &mut cursor)?;
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
    fn read(&self) -> Result<Statement> {
        match self.module {
            Modules::Empty => error::CustomSnafu {
                message: "no barkml code was read by this loader",
            }
            .fail(),
            _ => Ok(self.module.get().clone()),
        }
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
    use semver::Version;

    use crate::loader::{LoaderMode, StandardLoader};
    use crate::{Loader, Metadata, Statement, Value};

    #[test]
    pub fn load_single() {
        let expected = Statement::new_module(
            ".",
            IndexMap::from([
                (
                    "tire".into(),
                    Statement::new_control(
                        "tire",
                        None,
                        Value::new_version(
                            Version::new(1, 0, 0),
                            Metadata {
                                comment: None,
                                label: Some("Test".into()),
                            },
                        ),
                        Metadata::default(),
                    )
                    .unwrap(),
                ),
                (
                    "section-1".into(),
                    Statement::new_section(
                        "section-1",
                        IndexMap::from([
                            (
                                "number".into(),
                                Statement::new_assign(
                                    "number",
                                    None,
                                    Value::new_int(4, Metadata::default()),
                                    Metadata {
                                        comment: Some("Documentation".into()),
                                        label: None,
                                    },
                                )
                                .unwrap(),
                            ),
                            (
                                "floating".into(),
                                Statement::new_assign(
                                    "floating",
                                    Some(crate::ValueType::F32),
                                    Value::new_f32(3.14, Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "versioning".into(),
                                Statement::new_assign(
                                    "versioning",
                                    None,
                                    Value::new_version(
                                        Version::parse("1.2.3-beta.6").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "requires".into(),
                                Statement::new_assign(
                                    "requires",
                                    None,
                                    Value::new_require(
                                        semver::VersionReq::parse("^1.3.3").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "strings".into(),
                                Statement::new_assign(
                                    "strings",
                                    None,
                                    Value::new_string("foobar".into(), Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "arrays".to_string(),
                                Statement::new_assign(
                                    "arrays",
                                    None,
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), Metadata::default()),
                                            Value::new_int(5, Metadata::default()),
                                            Value::new_float(3.14, Metadata::default()),
                                            Value::new_string("single".into(), Metadata::default()),
                                        ],
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Statement::new_assign(
                                    "object",
                                    None,
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    Metadata::default(),
                                                ),
                                            ),
                                            ("bar".into(), Value::new_int(4, Metadata::default())),
                                        ]),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "blocks.0.3.'label-a'".into(),
                                Statement::new_block(
                                    "blocks",
                                    vec![
                                        Value::new_float(0.3, Metadata::default()),
                                        Value::new_string(
                                            "label-a".to_string(),
                                            Metadata::default(),
                                        ),
                                    ],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Statement::new_assign(
                                            "simple",
                                            None,
                                            Value::new_string("me".into(), Metadata::default()),
                                            Metadata::default(),
                                        )
                                        .unwrap(),
                                    )]),
                                    Metadata::default(),
                                ),
                            ),
                        ]),
                        Metadata::default(),
                    ),
                ),
            ]),
            Metadata::default(),
        );
        let result = StandardLoader::default()
            .mode(LoaderMode::Single)
            .expect("failed to set mode")
            .path("examples/example.bml")
            .expect("path detection failed")
            .load()
            .expect("load failed");
        assert_eq!(result.data, expected.data);
    }

    #[test]
    pub fn load_multiple() {
        let _expected = Statement::new_module(
            ".",
            IndexMap::from([
                (
                    "tire".into(),
                    Statement::new_control(
                        "tire",
                        None,
                        Value::new_version(
                            Version::new(1, 0, 0),
                            Metadata {
                                comment: None,
                                label: Some("Test".into()),
                            },
                        ),
                        Metadata::default(),
                    )
                    .unwrap(),
                ),
                (
                    "section-1".into(),
                    Statement::new_section(
                        "section-1",
                        IndexMap::from([
                            (
                                "number".into(),
                                Statement::new_assign(
                                    "number",
                                    None,
                                    Value::new_int(4, Metadata::default()),
                                    Metadata {
                                        comment: Some("Documentation".into()),
                                        label: None,
                                    },
                                )
                                .unwrap(),
                            ),
                            (
                                "floating".into(),
                                Statement::new_assign(
                                    "floating",
                                    Some(crate::ValueType::F32),
                                    Value::new_f32(3.14, Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "versioning".into(),
                                Statement::new_assign(
                                    "versioning",
                                    None,
                                    Value::new_version(
                                        Version::parse("1.2.3-beta.6").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "requires".into(),
                                Statement::new_assign(
                                    "requires",
                                    None,
                                    Value::new_require(
                                        semver::VersionReq::parse("^1.3.3").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "strings".into(),
                                Statement::new_assign(
                                    "strings",
                                    None,
                                    Value::new_string("foobar".into(), Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "arrays".to_string(),
                                Statement::new_assign(
                                    "arrays",
                                    None,
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), Metadata::default()),
                                            Value::new_int(5, Metadata::default()),
                                            Value::new_float(3.14, Metadata::default()),
                                            Value::new_string("single".into(), Metadata::default()),
                                        ],
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Statement::new_assign(
                                    "object",
                                    None,
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    Metadata::default(),
                                                ),
                                            ),
                                            ("bar".into(), Value::new_int(4, Metadata::default())),
                                        ]),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "blocks.0.3.'label-a'".into(),
                                Statement::new_block(
                                    "blocks",
                                    vec![
                                        Value::new_float(0.3, Metadata::default()),
                                        Value::new_string(
                                            "label-a".to_string(),
                                            Metadata::default(),
                                        ),
                                    ],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Statement::new_assign(
                                            "simple",
                                            None,
                                            Value::new_string("me".into(), Metadata::default()),
                                            Metadata::default(),
                                        )
                                        .unwrap(),
                                    )]),
                                    Metadata::default(),
                                ),
                            ),
                        ]),
                        Metadata::default(),
                    ),
                ),
                (
                    "section-2".into(),
                    Statement::new_section(
                        "section-2",
                        IndexMap::from([
                            (
                                "number".into(),
                                Statement::new_assign(
                                    "number",
                                    None,
                                    Value::new_int(4, Metadata::default()),
                                    Metadata {
                                        comment: Some("Documentation".into()),
                                        label: None,
                                    },
                                )
                                .unwrap(),
                            ),
                            (
                                "floating".into(),
                                Statement::new_assign(
                                    "floating",
                                    Some(crate::ValueType::F32),
                                    Value::new_f32(3.14, Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "versioning".into(),
                                Statement::new_assign(
                                    "versioning",
                                    None,
                                    Value::new_version(
                                        Version::parse("1.2.3-beta.6").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "requires".into(),
                                Statement::new_assign(
                                    "requires",
                                    None,
                                    Value::new_require(
                                        semver::VersionReq::parse("^1.3.3").unwrap(),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "strings".into(),
                                Statement::new_assign(
                                    "strings",
                                    None,
                                    Value::new_string("foobar".into(), Metadata::default()),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "arrays".to_string(),
                                Statement::new_assign(
                                    "arrays",
                                    None,
                                    Value::new_array(
                                        vec![
                                            Value::new_string("hello".into(), Metadata::default()),
                                            Value::new_int(5, Metadata::default()),
                                            Value::new_float(3.14, Metadata::default()),
                                            Value::new_string("single".into(), Metadata::default()),
                                        ],
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "object".into(),
                                Statement::new_assign(
                                    "object",
                                    None,
                                    Value::new_table(
                                        IndexMap::from([
                                            (
                                                "foo".into(),
                                                Value::new_bytes(
                                                    b"binarystring".to_vec(),
                                                    Metadata::default(),
                                                ),
                                            ),
                                            ("bar".into(), Value::new_int(4, Metadata::default())),
                                        ]),
                                        Metadata::default(),
                                    ),
                                    Metadata::default(),
                                )
                                .unwrap(),
                            ),
                            (
                                "blocks.0.3.'label-a'".into(),
                                Statement::new_block(
                                    "blocks",
                                    vec![
                                        Value::new_float(0.3, Metadata::default()),
                                        Value::new_string(
                                            "label-a".to_string(),
                                            Metadata::default(),
                                        ),
                                    ],
                                    IndexMap::from([(
                                        "simple".into(),
                                        Statement::new_assign(
                                            "simple",
                                            None,
                                            Value::new_string("me".into(), Metadata::default()),
                                            Metadata::default(),
                                        )
                                        .unwrap(),
                                    )]),
                                    Metadata::default(),
                                ),
                            ),
                        ]),
                        Metadata::default(),
                    ),
                ),
            ]),
            Metadata::default(),
        );
        let result = StandardLoader::default()
            .mode(LoaderMode::Merge)
            .expect("failed to set mode")
            .path("examples/config_append")
            .expect("path detection failed")
            .load()
            .expect("load failed");
        assert_eq!(result.data, _expected.data);
    }
}
