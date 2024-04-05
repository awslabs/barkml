use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::path::PathBuf;

use base64::Engine;
#[cfg(feature = "binary")]
use msgpack_simple::{Extension, MapElement, MsgPack};
use snafu::{ensure, OptionExt, ResultExt};

use crate::error::{self, Result};

/// Stores integer values in their appropriate
/// precisioned type.
#[derive(Debug, Clone)]
pub enum Int {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

/// Helps define access methods for Int and Float
macro_rules! variant {
    ($name: ident, $mut_name: ident, $key: ident, $ret: ident) => {
        pub fn $name(&self) -> Option<&$ret> {
            match self {
                Self::$key(data) => Some(data),
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $ret> {
            match self {
                Self::$key(data) => Some(data),
                _ => None,
            }
        }
    };
}

impl Int {
    variant!(as_i8, as_i8_mut, I8, i8);
    variant!(as_i16, as_i16_mut, I16, i16);
    variant!(as_i32, as_i32_mut, I32, i32);
    variant!(as_i64, as_i64_mut, I64, i64);
    variant!(as_u8, as_u8_mut, U8, u8);
    variant!(as_u16, as_u16_mut, U16, u16);
    variant!(as_u32, as_u32_mut, U32, u32);
    variant!(as_u64, as_u64_mut, U64, u64);

    /// Returns the value inside an Int object
    /// but looses precision and converts to i64
    pub fn as_int(&self) -> i64 {
        match self {
            Self::I8(data) => *data as i64,
            Self::I16(data) => *data as i64,
            Self::I32(data) => *data as i64,
            Self::I64(data) => *data,
            Self::U8(data) => *data as i64,
            Self::U16(data) => *data as i64,
            Self::U32(data) => *data as i64,
            Self::U64(data) => *data as i64,
        }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::I8(data) => data.to_string(),
                Self::I16(data) => data.to_string(),
                Self::I32(data) => data.to_string(),
                Self::I64(data) => data.to_string(),
                Self::U8(data) => data.to_string(),
                Self::U16(data) => data.to_string(),
                Self::U32(data) => data.to_string(),
                Self::U64(data) => data.to_string(),
            }
            .as_str(),
        )
    }
}

/// Stores a precision based floating point value
#[derive(Debug, Clone)]
pub enum Float {
    F32(f32),
    F64(f64),
}

impl Float {
    variant!(as_f32, as_f32_mut, F32, f32);
    variant!(as_f64, as_f64_mut, F64, f64);

    /// Return the floating point value losing its precision
    /// by converting it to f64
    pub fn as_float(&self) -> f64 {
        match self {
            Self::F32(data) => *data as f64,
            Self::F64(data) => *data,
        }
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::F64(data) => data.to_string(),
                Self::F32(data) => data.to_string(),
            }
            .as_str(),
        )
    }
}

/// A `Value` will represent a given node in a barkml file
#[derive(Debug, Clone)]
pub enum Value {
    /// Modules represent a single BarkML file
    Module(Vec<Value>),
    /// Comments always start with #, and multiple lined comments will be appended (squashed) together
    Comment(String),
    /// Control statements begin with a $ and assign a value
    Control {
        /// Key of control statement without $
        label: String,
        /// Value of control statement
        value: Box<Self>,
    },
    /// Any assignment whose label/identifier does not start with $ is considered a normal
    /// assignment
    Assignment {
        /// Key of assignment
        label: String,
        /// Value
        value: Box<Self>,
    },
    /// A block is any set of values defined inside braces prefixed first by any identifier
    /// followed by 0 or more string labels
    Block {
        /// Identifier of the block
        id: String,
        /// Any number of labels defined as whitespace sperated strings
        labels: Vec<String>,
        /// Values defined inside the braces of a block
        statements: Vec<Self>,
    },
    /// A section is defined by an identifier inside square brackets and contains
    /// any number of values defined below this identifier until end of file or another
    /// section is defined
    Section {
        // Identifier of the section
        id: String,
        // All statements/values defined below
        statements: Vec<Self>,
    },

    /// A table is a set of string keys to values (set via =)
    /// This value type can be prefixed with a !Label
    Table(HashMap<String, Self>, Option<String>),

    /// An array of values defined inside square brackets.
    /// Arrays in BarkML can contain mixed types
    /// This value type can be prefixed with a !Label
    Array(Vec<Self>, Option<String>),

    /// A macro string is only utilized internally as an in-place
    /// representation of a macro, its value and label will be resolved after
    /// parsing and thus there never should be a raw Macro in a resulting fully parsed
    /// BarkML file.
    Macro(String, Option<String>, bool),

    /// Strings are defined either between single quotes or double quotes
    /// This value type can be prefixed with a !Label
    String(String, Option<String>),

    /// Bytes are a base64 encoded representation of binary data wrapped inside b'...', once the file
    /// is parsed the byte data is available decoded from this format.
    /// This value type can be prefixed with a !Label
    Bytes(Vec<u8>, Option<String>),

    /// Integers are defined as any decimal value, by default
    /// they are assumed to be a 64-bit signed integer, however
    /// precision can be defined by suffixing the number with no whitespace
    /// with one of `i` (for signed integers), `u` (for unsigned integers) followed by
    /// a precision (8, 16, 32, 64 currently supported)
    Int(Int, Option<String>),

    /// Floating point numbers are defined in numerical value with optional
    /// scientific notation provided. They are assumed to be
    /// a 64-bit floating point number, however precision can be
    /// defined by suffixing the number with 'f' followed by a precision (32, 64 currently supported).
    /// This value type can be prefixed with a !Label
    Float(Float, Option<String>),

    /// Boolean values  are defined as one of the below keywords for true and false
    ///
    /// true: true, True, TRUE, yes, Yes, YES, on, On, ON
    /// false: false, False, FALSE, no, No, No, off, Off, OFF
    ///
    /// This value type can be prefixed with a !Label
    Bool(bool, Option<String>),

    /// A label is any identifier prefixed with a !
    /// This node will only exist when a value is a label by itself
    Label(String),

    /// A null value can be defined using any of the below keywords
    ///
    /// * null
    /// * Null
    /// * NULL
    /// * nil
    /// * Nil
    /// * NIL
    /// * none
    /// * None
    /// * NONE
    /// This value type can be prefixed with a !Label
    Null(Option<String>),
}

macro_rules! as_fn {
    ($fn_name: ident, $mut_name: ident, $name: ident : $ty: ty where $key: ident) => {
        pub fn $fn_name(&self) -> Option<&$ty> {
            match self {
                Self::$key($name,..) => Some($name),
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $ty> {
            match self {
                Self::$key($name,..) => Some($name),
                _ => None,
            }
        }
    };


    ($fn_name: ident, $mut_name: ident,  $chain: ident . $mut_chain: ident ( $name: ident : $ty: ty) where $key: ident) => {
        pub fn $fn_name(&self) -> Option<&$ty> {
            match self {
                Self::$key($name,..) => {
                    $name.$chain()
                },
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $ty> {
            match self {
                Self::$key($name,..) => {
                    $name.$mut_chain()
                },
                _ => None,
            }
        }
    };

    ($fn_name: ident, $mut_name: ident,  { $($name: ident : $ty: ty),* } where $key: ident) => {
        pub fn $fn_name(&self) -> Option<($(&$ty),*)> {
            match self {
                Self::$key {
                    $($name),*
                } => {
                    Some(($($name),*))
                },
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<($(&mut $ty),*)> {
            match self {
                Self::$key {
                    $($name),*
                } => {
                    Some(($($name),*))
                },
                _ => None,
            }
        }
    };
}

impl Value {
    /// Returns the id of a statement value or None if not a statement
    pub fn get_id(&self) -> Option<&String> {
        match self {
            Self::Section { id, .. } => Some(id),
            Self::Block { id, .. } => Some(id),
            Self::Assignment { label, .. } => Some(label),
            Self::Control { label, .. } => Some(label),
            _ => None,
        }
    }

    as_fn!(as_module, as_module_mut, value: Vec<Value> where Module);
    as_fn!(as_section, as_section_mut, {id: String, statements: Vec<Value>} where Section);
    as_fn!(as_block, as_block_mut, {id: String, labels: Vec<String>, statements: Vec<Value>} where Block);
    as_fn!(as_assignment, as_assignment_mut, {label: String, value: Box<Value>} where Assignment);
    as_fn!(as_control, as_control_mut, {label: String, value: Box<Value>} where Control);
    as_fn!(as_comment, as_comment_mut, value: String where Comment);
    as_fn!(as_table, as_table_mut, value: HashMap<String, Value> where Table);
    as_fn!(as_array, as_array_mut, value: Vec<Value> where Array);
    as_fn!(as_macro, as_macro_mut, value: String where Macro);
    as_fn!(as_string, as_string_mut, value: String where String);
    as_fn!(as_bytes, as_bytes_mut, value: Vec<u8> where Bytes);

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Self::Int(int, ..) => Some(int.as_int()),
            _ => None,
        }
    }

    as_fn!(as_i8, as_i8_mut, as_i8.as_i8_mut(value: i8) where Int);
    as_fn!(as_i16, as_i16_mut, as_i16.as_i16_mut(value: i16) where Int);
    as_fn!(as_i32, as_i32_mut, as_i32.as_i32_mut(value: i32) where Int);
    as_fn!(as_i64, as_i64_mut, as_i64.as_i64_mut(value: i64) where Int);
    as_fn!(as_u8, as_u8_mut, as_u8.as_u8_mut(value: u8) where Int);
    as_fn!(as_u16, as_u16_mut, as_u16.as_u16_mut(value: u16) where Int);
    as_fn!(as_u32, as_u32_mut, as_u32.as_u32_mut(value: u32) where Int);
    as_fn!(as_u64, as_u64_mut, as_u64.as_u64_mut(value: u64) where Int);

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Self::Float(float, ..) => Some(float.as_float()),
            _ => None,
        }
    }

    as_fn!(as_f32, as_f32_mut, as_f32.as_f32_mut(value: f32) where Float);
    as_fn!(as_f64, as_f64_mut, as_f64.as_f64_mut(value: f64) where Float);
    as_fn!(as_bool, as_bool_mut, value: bool where Bool);
    as_fn!(as_label, as_label_mut, value: String where Label);

    /// Returns true if the value is a null value
    fn is_null(&self) -> bool {
        matches!(self, Self::Null(_))
    }

    /// Used internally to adjust the label of a node
    pub(crate) fn set_label(&mut self, value: &str) {
        match self {
            Self::Table(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Array(_, ref mut label) => *label = Some(value.to_owned()),
            Self::String(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Macro(_, ref mut label, _) => *label = Some(value.to_owned()),
            Self::Bytes(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Int(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Float(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Bool(_, ref mut label) => *label = Some(value.to_owned()),
            Self::Label(ref mut label) => *label = value.to_owned(),
            Self::Null(ref mut label) => *label = Some(value.to_owned()),
            _ => {}
        }
    }

    /// Used internally to return the value as what should be used
    /// to replace a macro in a macro string with this value
    pub(crate) fn to_macro_string(&self) -> String {
        match self {
            Self::Table(..) => self.to_string(),
            Self::Array(children, _) => children
                .iter()
                .map(|x| x.to_macro_string())
                .collect::<Vec<String>>()
                .join(", "),
            Self::Bytes(bytes, _) => {
                base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice())
            }
            Self::Int(value, _) => value.to_string(),
            Self::Float(value, _) => value.to_string(),
            Self::Bool(value, _) => value.to_string(),
            Self::Label(value) => value.clone(),
            Self::Null(_) => "null".to_owned(),
            Self::Macro(..) => self.to_string(),
            Self::String(value, _) => value.clone(),
            _ => String::default(),
        }
    }

    /// Read the value from a message pack encoded binary object
    #[cfg(feature = "binary")]
    pub fn from_binary(entry: MsgPack) -> Result<Self> {
        let ext = entry
            .as_extension()
            .context(error::MsgPackNotExpectedSnafu)?;
        match ext.type_id {
            0 => {
                let value =
                    MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
                let comment = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Comment(comment))
            }
            1 => {
                let value =
                    MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let label = Self::find_entry(&entries, "label")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let value = Value::from_binary(Self::find_entry(&entries, "value")?)?;
                Ok(Self::Control {
                    label,
                    value: Box::new(value),
                })
            }
            2 => {
                let value =
                    MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let label = Self::find_entry(&entries, "label")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let value = Value::from_binary(Self::find_entry(&entries, "value")?)?;
                Ok(Self::Assignment {
                    label,
                    value: Box::new(value),
                })
            }
            3 => {
                let value =
                    MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let id = Self::find_entry(&entries, "id")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let labels: Vec<String> = Self::find_entry(&entries, "labels")?
                    .as_array()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .map(|x| {
                        x.clone()
                            .as_string()
                            .context(error::MsgPackNotExpectedSnafu)
                    })
                    .flatten()
                    .collect();
                let statements: Vec<Value> = Self::find_entry(&entries, "statements")?
                    .as_array()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .map(|x| Self::from_binary(x.clone()))
                    .flatten()
                    .collect();
                Ok(Self::Block {
                    id,
                    labels,
                    statements,
                })
            }
            4 => {
                let value =
                    MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let id = Self::find_entry(&entries, "id")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let statements: Vec<Value> = Self::find_entry(&entries, "statements")?
                    .as_array()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .map(|x| Self::from_binary(x.clone()))
                    .flatten()
                    .collect();
                Ok(Self::Section { id, statements })
            }
            100 => {
                let (label, content) = Self::extract(&ext)?;
                let content = content.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let mut table_content = HashMap::new();
                for entry in content.iter() {
                    let key = entry
                        .key
                        .clone()
                        .as_string()
                        .context(error::MsgPackNotExpectedSnafu)?;
                    let value = Self::from_binary(entry.value.clone())?;
                    table_content.insert(key, value);
                }
                Ok(Self::Table(table_content, label))
            }
            101 => {
                let (label, content) = Self::extract(&ext)?;
                let content = content.as_array().context(error::MsgPackNotExpectedSnafu)?;
                let mut array_content = Vec::new();
                for entry in content.iter() {
                    array_content.push(Self::from_binary(entry.clone())?);
                }
                Ok(Self::Array(array_content, label))
            }
            102 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::String(value, label))
            }
            103 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content
                    .as_binary()
                    .context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Bytes(value, label))
            }
            104 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content.as_int().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Int(Int::I64(value), label))
            }
            105 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content.as_float().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Float(Float::F64(value), label))
            }
            106 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content
                    .as_boolean()
                    .context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Bool(value, label))
            }
            107 => {
                let label = MsgPack::parse(ext.value.clone().as_slice())
                    .context(error::MsgPackEncodedSnafu)?;
                let label = label.as_string().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Label(label))
            }
            108 => {
                let (label, content) = Self::extract(&ext)?;
                ensure!(content.is_nil(), error::MsgPackUnsupportedSnafu);
                Ok(Self::Null(label))
            }
            _ => Err(error::Error::MsgPackUnsupported),
        }
    }

    /// Find a specific entry in a message pack object
    #[cfg(feature = "binary")]
    fn find_entry(input: &Vec<MapElement>, key: &str) -> Result<MsgPack> {
        input
            .iter()
            .find_map(|x| {
                if let Ok(label) = x.key.clone().as_string() {
                    if label == key {
                        Some(x.value.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .context(error::MsgPackUnsupportedSnafu)
    }

    /// Extract a value encoded with a label from a MsgPack
    /// extension object
    #[cfg(feature = "binary")]
    fn extract(entry: &Extension) -> Result<(Option<String>, MsgPack)> {
        let table =
            MsgPack::parse(entry.value.clone().as_slice()).context(error::MsgPackEncodedSnafu)?;
        let table = table.as_map().context(error::MsgPackNotExpectedSnafu)?;
        // There should only be two entries in this encoding
        ensure!(table.len() == 2, error::MsgPackUnsupportedSnafu);
        let (key0, value0, value1) = (
            table[0]
                .key
                .clone()
                .as_int()
                .context(error::MsgPackNotExpectedSnafu)?,
            table[0].value.clone(),
            table[1].value.clone(),
        );
        let label = if key0 == 0 {
            value0.clone()
        } else {
            value1.clone()
        };
        let label = if label.is_nil() {
            None
        } else {
            Some(label.as_string().context(error::MsgPackNotExpectedSnafu)?)
        };
        let content = if key0 == 1 {
            value0.clone()
        } else {
            value1.clone()
        };
        Ok((label, content))
    }

    /// Convert this value into a message pack encoded
    /// binary value.
    #[cfg(feature = "binary")]
    pub fn to_binary(&self) -> MsgPack {
        let (type_id, value) = match self {
            Self::Comment(value) => (0, MsgPack::String(value.clone())),
            Self::Control { label, value } => (
                1,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("label".to_string()),
                        value: MsgPack::String(label.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("value".to_string()),
                        value: value.to_binary(),
                    },
                ]),
            ),
            Self::Assignment { label, value } => (
                2,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("label".to_string()),
                        value: MsgPack::String(label.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("value".to_string()),
                        value: value.to_binary(),
                    },
                ]),
            ),
            Self::Block {
                id,
                labels,
                statements,
            } => (
                3,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("id".to_string()),
                        value: MsgPack::String(id.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("labels".to_string()),
                        value: MsgPack::Array(
                            labels.iter().map(|x| MsgPack::String(x.clone())).collect(),
                        ),
                    },
                    MapElement {
                        key: MsgPack::String("statements".to_string()),
                        value: MsgPack::Array(statements.iter().map(|x| x.to_binary()).collect()),
                    },
                ]),
            ),
            Self::Section { id, statements } => (
                4,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("id".to_string()),
                        value: MsgPack::String(id.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("statements".to_string()),
                        value: MsgPack::Array(statements.iter().map(|x| x.to_binary()).collect()),
                    },
                ]),
            ),
            Self::Table(data, label) => (
                100,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Map(
                            data.iter()
                                .map(|(k, v)| MapElement {
                                    key: MsgPack::String(k.clone()),
                                    value: v.to_binary(),
                                })
                                .collect(),
                        ),
                    },
                ]),
            ),
            Self::Array(data, label) => (
                101,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Array(data.iter().map(|x| x.to_binary()).collect()),
                    },
                ]),
            ),
            Self::String(data, label) => (
                102,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::String(data.clone()),
                    },
                ]),
            ),
            Self::Bytes(data, label) => (
                103,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Binary(data.clone()),
                    },
                ]),
            ),
            Self::Int(data, label) => (
                104,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Int(data.as_int()),
                    },
                ]),
            ),
            Self::Float(data, label) => (
                105,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(0),
                        value: MsgPack::Float(data.as_float()),
                    },
                ]),
            ),
            Self::Bool(data, label) => (
                106,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Boolean(*data),
                    },
                ]),
            ),
            Self::Label(data) => (107, MsgPack::String(data.clone())),
            Self::Null(label) => (
                108,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::Int(0),
                        value: label
                            .as_ref()
                            .map(|x| MsgPack::String(x.clone()))
                            .unwrap_or(MsgPack::Nil),
                    },
                    MapElement {
                        key: MsgPack::Int(1),
                        value: MsgPack::Nil,
                    },
                ]),
            ),
            _ => return MsgPack::Nil,
        };

        MsgPack::Extension(Extension {
            type_id,
            value: value.encode(),
        })
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Self::Module(children) => children
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
            Self::Comment(comment) => format!("# {}", comment),
            Self::Control { label, value } => format!("${} = {}", label, value.to_string()),
            Self::Assignment { label, value } => format!("{} = {}", label, value.to_string()),
            Self::Block {
                id,
                labels,
                statements,
            } => {
                let mut s = format!("{} ", id);
                for label in labels {
                    s.push_str(&format!("'{}' ", label));
                }
                s.push('{');
                for statement in statements {
                    s.push_str(&format!("\n\t{}", statement.to_string()));
                }
                s.push_str("\n}");
                s
            }
            Self::Section { id, statements } => {
                let mut s = format!("[{}]", id);
                for statement in statements {
                    s.push_str(&format!("\n{}", statement.to_string()));
                }
                s
            }
            Self::Table(map, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += "{ ";
                for index in 0..map.len() {
                    let (key, value) = map.iter().nth(index).unwrap();
                    s += format!("{} = {} ", key, value.to_string()).as_str();
                    if index != map.len() - 1 {
                        s += ", ";
                    }
                }
                s += " }";
                s
            }
            Self::Array(array, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += format!(
                    "[{}]",
                    array
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
                .as_str();
                s
            }
            Self::String(string, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += format!("'{}'", string).as_str();
                s
            }
            Self::Macro(string, label, is_string) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                let m = if *is_string {
                    format!("m!'{}'", string)
                } else {
                    format!("m!{}", string)
                };
                s += m.as_str();
                s
            }
            Self::Bytes(bytes, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                let encoded =
                    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice());
                s += format!("b'{}'", encoded).as_str();
                s
            }
            Self::Int(int, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += int.to_string().as_str();
                s
            }
            Self::Float(float, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += float.to_string().as_str();
                s
            }
            Self::Bool(boolean, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += if *boolean { "true" } else { "false" };
                s
            }
            Self::Label(label) => {
                format!("!{}", label)
            }
            Self::Null(label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += "null";
                s
            }
        }
    }
}
