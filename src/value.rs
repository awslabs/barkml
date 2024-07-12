use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use base64::Engine;
#[cfg(feature = "binary")]
use msgpack_simple::{Extension, MapElement, MsgPack};
use semver::{self, VersionReq};
use snafu::ensure;
use uuid::Uuid;

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

    pub fn type_of(&self) -> ValueType {
        match self {
            Self::I8(_) => ValueType::I8,
            Self::I16(_) => ValueType::I16,
            Self::I32(_) => ValueType::I32,
            Self::I64(_) => ValueType::I64,
            Self::U8(_) => ValueType::U8,
            Self::U16(_) => ValueType::U16,
            Self::U32(_) => ValueType::U32,
            Self::U64(_) => ValueType::U64,
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

    pub fn type_of(&self) -> ValueType {
        match self {
            Self::F32(_) => ValueType::F32,
            Self::F64(_) => ValueType::F64,
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

#[derive(Debug, Clone)]
pub struct Value {
    pub(crate) uid: Uuid,
    pub data: Box<Data>,
    pub type_: ValueType,
    pub id: Option<String>,
    pub label: Option<String>,
    pub comment: Option<String>,
}

/// A `Value` will represent a given node in a barkml file
#[derive(Debug, Clone)]
pub enum Data {
    /// Modules represent a single BarkML file
    Module(HashMap<String, Value>),
    /// Control statements begin with a $ and assign a value
    Control(Value),
    /// Any assignment whose label/identifier does not start with $ is considered a normal
    /// assignment
    Assignment(Value),
    /// A block is any set of values defined inside braces prefixed first by any identifier
    /// followed by 0 or more string labels
    Block {
        labels: Vec<String>,
        children: HashMap<String, Value>,
    },
    /// A section is defined by an identifier inside square brackets and contains
    /// any number of values defined below this identifier until end of file or another
    /// section is defined
    Section(HashMap<String, Value>),

    /// A table is a set of string keys to values (set via =)
    /// This value type can be prefixed with a !Label
    Table(HashMap<String, Value>),

    /// An array of values defined inside square brackets.
    /// Arrays in BarkML can contain mixed types
    /// This value type can be prefixed with a !Label
    Array(Vec<Value>),

    /// A macro string is only utilized internally as an in-place
    /// representation of a macro, its value and label will be resolved after
    /// parsing and thus there never should be a raw Macro in a resulting fully parsed
    /// BarkML file.
    Macro(String, bool),

    /// Strings are defined either between single quotes or double quotes
    /// This value type can be prefixed with a !Label
    String(String),

    /// Bytes are a base64 encoded representation of binary data wrapped inside b'...', once the file
    /// is parsed the byte data is available decoded from this format.
    /// This value type can be prefixed with a !Label
    Bytes(Vec<u8>),

    /// Integers are defined as any decimal value, by default
    /// they are assumed to be a 64-bit signed integer, however
    /// precision can be defined by suffixing the number with no whitespace
    /// with one of `i` (for signed integers), `u` (for unsigned integers) followed by
    /// a precision (8, 16, 32, 64 currently supported)
    Int(Int),

    /// Floating point numbers are defined in numerical value with optional
    /// scientific notation provided. They are assumed to be
    /// a 64-bit floating point number, however precision can be
    /// defined by suffixing the number with 'f' followed by a precision (32, 64 currently supported).
    /// This value type can be prefixed with a !Label
    Float(Float),

    /// Boolean values  are defined as one of the below keywords for true and false
    ///
    /// true: true, True, TRUE, yes, Yes, YES, on, On, ON
    /// false: false, False, FALSE, no, No, No, off, Off, OFF
    ///
    /// This value type can be prefixed with a !Label
    Bool(bool),

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
    Null,

    /// A Version value represents a semantic version
    Version(semver::Version),

    /// A Require represents a semantic version requirement
    Require(VersionReq),
}

impl Data {
    /// Used internally to return the value as what should be used
    /// to replace a macro in a macro string with this value
    pub(crate) fn to_macro_string(&self) -> String {
        match self {
            Data::Array(children) => children
                .iter()
                .map(|x| x.inner().to_macro_string())
                .collect::<Vec<String>>()
                .join(", "),
            Data::Bytes(bytes) => {
                base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice())
            }
            Data::Int(value) => value.to_string(),
            Data::Float(value) => value.to_string(),
            Data::Bool(value) => value.to_string(),
            Data::Label(value) => value.clone(),
            Data::Null => "null".to_owned(),
            Data::String(value) => value.clone(),
            _ => String::default(),
        }
    }
}

/// ValueType stores the type of value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    String,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bytes,
    Bool,
    Version,
    Require,
    Macro,
    Label,
    Null,
    Array(Vec<Self>),
    Table(HashMap<String, Self>),
    Section(HashMap<String, Self>),
    Block(HashMap<String, Self>),
    Module(HashMap<String, Self>),
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::Bytes => f.write_str("bytes"),
            Self::Version => f.write_str("version"),
            Self::Require => f.write_str("require"),
            Self::Label => f.write_str("label"),
            Self::Null => f.write_str("typeof(null)"),
            Self::Bool => f.write_str("bool"),
            Self::Array(body) => f.write_fmt(format_args!(
                "array[{}]",
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(","),
            )),
            Self::Table(content) => f.write_fmt(format_args!(
                "table{{ {} }}",
                content
                    .iter()
                    .map(|(k, v)| format!("{} : {}", k, v))
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Self::Section(content) => f.write_fmt(format_args!(
                "section{{ {} }}",
                content
                    .iter()
                    .map(|(k, v)| format!("{} : {}", k, v))
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Self::Block(content) => f.write_fmt(format_args!(
                "block{{ {} }}",
                content
                    .iter()
                    .map(|(k, v)| format!("{} : {}", k, v))
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Self::Module(content) => f.write_fmt(format_args!(
                "module{{ {} }}",
                content
                    .iter()
                    .map(|(k, v)| format!("{} : {}", k, v))
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Self::Macro => f.write_str("macro"),
        }
    }
}

macro_rules! hint_assert {
    ($hint: expr, $value: expr) => {
        if let Some(hint) = $hint.as_ref() {
            ensure!(
                *hint == $value.type_of(),
                error::TypeCollisionSnafu {
                    left: hint.clone(),
                    right: $value.type_of(),
                }
            );
        }
    };
}

macro_rules! as_fn {
    ($fn_name: ident, $mut_name: ident, $name: ident : $ty: ty where $key: ident) => {
        pub fn $fn_name(&self) -> Option<&$ty> {
            match self.inner() {
                Data::$key($name,..) => Some($name),
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $ty> {
            match self.inner_mut() {
                Data::$key($name,..) => Some($name),
                _ => None,
            }
        }
    };


    ($fn_name: ident, $mut_name: ident,  $chain: ident . $mut_chain: ident ( $name: ident : $ty: ty) where $key: ident) => {
        pub fn $fn_name(&self) -> Option<&$ty> {
            match self.inner() {
                Data::$key($name,..) => {
                    $name.$chain()
                },
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $ty> {
            match self.inner_mut() {
                Data::$key($name,..) => {
                    $name.$mut_chain()
                },
                _ => None,
            }
        }
    };

    ($fn_name: ident, $mut_name: ident,  { $($name: ident : $ty: ty),* } where $key: ident) => {
        pub fn $fn_name(&self) -> Option<($(&$ty),*)> {
            match self.inner() {
                Data::$key {
                    $($name),*
                } => {
                    Some(($($name),*))
                },
                _ => None,
            }
        }

        pub fn $mut_name(&mut self) -> Option<($(&mut $ty),*)> {
            match self.inner_mut() {
                Data::$key {
                    $($name),*
                } => {
                    Some(($($name),*))
                },
                _ => None,
            }
        }
    };
}

macro_rules! new_number {
    ($fn_name: ident, $variant: ident as $sub: ident : $ty: ty) => {
        pub fn $fn_name(
            input: $ty,
            label: Option<String>,
            comment: Option<String>,
        ) -> Result<Self> {
            Ok(Self {
                uid: Uuid::now_v7(),
                data: Box::new(Data::$variant($variant::$sub(input))),
                id: None,
                label,
                comment,
                type_: ValueType::$sub,
            })
        }
    };
}

impl Value {
    pub fn new_module(statements: HashMap<String, Self>, comment: Option<String>) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Module(statements.clone())),
            id: None,
            comment,
            label: None,
            type_: ValueType::Module(
                statements
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            ),
        })
    }

    pub fn new_section(
        id: String,
        statements: HashMap<String, Self>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Section(statements.clone())),
            id: Some(id.clone()),
            comment,
            label: None,
            type_: ValueType::Section(
                statements
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            ),
        })
    }

    pub fn new_block(
        id: String,
        labels: Vec<String>,
        statements: HashMap<String, Self>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Block {
                labels,
                children: statements.clone(),
            }),
            id: Some(id.clone()),
            comment,
            label: None,
            type_: ValueType::Block(
                statements
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            ),
        })
    }

    pub fn new_assignment(
        id: String,
        value: Value,
        comment: Option<String>,
        hint: Option<ValueType>,
    ) -> Result<Self> {
        hint_assert!(hint, value);
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Assignment(value.clone())),
            id: Some(id),
            comment,
            label: None,
            type_: hint.unwrap_or(value.type_of().clone()),
        })
    }

    pub fn new_control(
        id: String,
        value: Value,
        comment: Option<String>,
        hint: Option<ValueType>,
    ) -> Result<Self> {
        hint_assert!(hint, value);
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Control(value.clone())),
            id: Some(id),
            comment,
            label: None,
            type_: hint.unwrap_or(value.type_of().clone()),
        })
    }

    pub fn new_table(
        content: HashMap<String, Self>,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Table(content.clone())),
            id: None,
            label,
            comment,
            type_: ValueType::Table(
                content
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            ),
        })
    }

    pub fn new_array(
        content: Vec<Self>,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Array(content.clone())),
            id: None,
            label,
            comment,
            type_: ValueType::Array(content.iter().map(|x| x.type_of()).collect()),
        })
    }

    pub fn new_macro(
        macro_string: String,
        is_string: bool,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Macro(macro_string, is_string)),
            id: None,
            label,
            comment,
            type_: ValueType::Macro,
        })
    }

    pub fn new_string(
        input: String,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::String(input)),
            id: None,
            label,
            comment,
            type_: ValueType::String,
        })
    }

    pub fn new_bytes(
        input: Vec<u8>,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Bytes(input)),
            id: None,
            label,
            comment,
            type_: ValueType::Bytes,
        })
    }

    pub fn new_precise_int(
        input: Int,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Int(input.clone())),
            id: None,
            label,
            comment,
            type_: input.type_of(),
        })
    }

    pub fn new_precise_float(
        input: Float,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Float(input.clone())),
            id: None,
            label,
            comment,
            type_: input.type_of(),
        })
    }

    new_number!(new_int, Int as I64 : i64);
    new_number!(new_i8, Int as I8 : i8);
    new_number!(new_i16, Int as I16 : i16);
    new_number!(new_i32, Int as I32 : i32);
    new_number!(new_i64, Int as I64 : i64);
    new_number!(new_u8, Int as U8 : u8);
    new_number!(new_u16, Int as U16 : u16);
    new_number!(new_u32, Int as U32 : u32);
    new_number!(new_u64, Int as U64 : u64);
    new_number!(new_float, Float as F64 : f64);
    new_number!(new_f32, Float as F32 : f32);
    new_number!(new_f64, Float as F64 : f64);

    pub fn new_null(label: Option<String>, comment: Option<String>) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Null),
            id: None,
            label,
            comment,
            type_: ValueType::Null,
        })
    }

    pub fn new_bool(value: bool, label: Option<String>, comment: Option<String>) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Bool(value)),
            id: None,
            label,
            comment,
            type_: ValueType::Bool,
        })
    }

    pub fn new_label(value: String, comment: Option<String>) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Label(value)),
            id: None,
            label: None,
            comment,
            type_: ValueType::Label,
        })
    }

    pub fn new_version(
        value: semver::Version,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Version(value)),
            id: None,
            label,
            comment,
            type_: ValueType::Version,
        })
    }

    pub fn new_require(
        value: VersionReq,
        label: Option<String>,
        comment: Option<String>,
    ) -> Result<Self> {
        Ok(Self {
            uid: Uuid::now_v7(),
            data: Box::new(Data::Require(value)),
            id: None,
            label,
            comment,
            type_: ValueType::Require,
        })
    }

    pub fn id(&self) -> Option<String> {
        self.id.as_ref().map(|id| match self.data.as_ref() {
            Data::Block { labels, .. } => {
                if labels.is_empty() {
                    id.clone()
                } else {
                    format!("{}.{}", id, labels.join("."))
                }
            }
            _ => id.clone(),
        })
    }

    pub(crate) fn inject_id(&self) -> Option<String> {
        if let Some(id) = self.id.as_ref() {
            match self.data.as_ref() {
                Data::Block { labels, .. } => {
                    if labels.is_empty() {
                        Some(id.clone())
                    } else {
                        Some(format!("{}.{}", id, labels.join(".")))
                    }
                }
                _ => Some(id.clone()),
            }
        } else {
            None
        }
    }

    pub fn type_of(&self) -> ValueType {
        self.type_.clone()
    }

    pub fn inner(&self) -> &Data {
        &self.data
    }

    pub fn inner_mut(&mut self) -> &mut Data {
        &mut self.data
    }

    /// Returns the id of a statement value or None if not a statement
    pub fn get_id(&self) -> Option<&String> {
        self.id.as_ref()
    }

    as_fn!(as_module, as_module_mut, value: HashMap<String, Value> where Module);
    as_fn!(as_section, as_section_mut, value: HashMap<String, Value> where Section);
    as_fn!(as_block, as_block_mut, {labels: Vec<String>, children: HashMap<String, Value>} where Block);
    as_fn!(as_assignment, as_assignment_mut, value: Value where Assignment);
    as_fn!(as_control, as_control_mut, value: Value where Control);
    as_fn!(as_table, as_table_mut, value: HashMap<String, Value> where Table);
    as_fn!(as_array, as_array_mut, value: Vec<Value> where Array);
    as_fn!(as_macro, as_macro_mut, value: String where Macro);
    as_fn!(as_string, as_string_mut, value: String where String);
    as_fn!(as_bytes, as_bytes_mut, value: Vec<u8> where Bytes);

    pub fn as_int(&self) -> Option<i64> {
        match self.inner() {
            Data::Int(int, ..) => Some(int.as_int()),
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
        match self.inner() {
            Data::Float(float, ..) => Some(float.as_float()),
            _ => None,
        }
    }

    as_fn!(as_f32, as_f32_mut, as_f32.as_f32_mut(value: f32) where Float);
    as_fn!(as_f64, as_f64_mut, as_f64.as_f64_mut(value: f64) where Float);
    as_fn!(as_bool, as_bool_mut, value: bool where Bool);
    as_fn!(as_label, as_label_mut, value: String where Label);
    as_fn!(as_version, as_version_mut, value: semver::Version where Version);
    as_fn!(as_require, as_require_mut, value: VersionReq where Require);

    /// Returns true if the value is a null value
    fn is_null(&self) -> bool {
        matches!(self.inner(), Data::Null)
    }

    /// Change the label on this value
    pub fn set_label(&mut self, label: &str) {
        self.label = Some(label.to_string());
    }

    /// Remove label
    pub fn remove_label(&mut self) {
        self.label = None;
    }

    /// Read the value from a message pack encoded binary object
    #[cfg(feature = "binary")]
    pub fn from_binary(entry: MsgPack) -> Result<Self> {
        let parent = entry.as_map().context(error::MsgPackNotExpectedSnafu)?;
        let id = if let Some(id) = Self::find_entry(parent.as_slice(), "id") {
            if let MsgPack::String(id) = id {
                Some(id.clone())
            } else {
                None
            }
        } else {
            None
        };
        let label = if let Some(label) = Self::find_entry(parent.as_slice(), "label") {
            if let MsgPack::String(label) = label {
                Some(label.clone())
            } else {
                None
            }
        } else {
            None
        };
        let comment = if let Some(comment) = Self::find_entry(parent.as_slice(), "comment") {
            if let MsgPack::String(comment) = comment {
                Some(comment.clone())
            } else {
                None
            }
        } else {
            None
        };
        let ext = if let Some(data) = Self::find_entry(parent.as_slice(), "data") {
            data.as_extension().context(error::MsgPackNotExpectedSnafu)
        } else {
            Err(error::Error::MsgPackUnsupported)
        }?;

        let value = MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
        let (data, value_type) = match ext.type_id {
            0 => {
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let statements: HashMap<String, Value> = entries
                    .iter()
                    .map(|x| {
                        (
                            x.key.clone().as_string().unwrap().clone(),
                            Self::from_binary(x.value.clone()).unwrap(),
                        )
                    })
                    .collect();
                (
                    Data::Module(statements.clone()),
                    ValueType::Module(
                        statements
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                )
            }
            1 => {
                let value = Value::from_binary(value)?;
                (Data::Control(value.clone()), value.type_of())
            }
            2 => {
                let value = Value::from_binary(value)?;
                (Data::Assignment(value.clone()), value.type_of())
            }
            3 => {
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let labels: Vec<String> = Self::find_entry(&entries, "labels")
                    .context(error::MsgPackUnsupportedSnafu)?
                    .as_array()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .flat_map(|x| {
                        x.clone()
                            .as_string()
                            .context(error::MsgPackNotExpectedSnafu)
                    })
                    .collect();
                let statements: HashMap<String, Value> = Self::find_entry(&entries, "statements")
                    .context(error::MsgPackUnsupportedSnafu)?
                    .as_map()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .map(|x| {
                        (
                            x.key.clone().as_string().unwrap(),
                            Self::from_binary(x.value.clone()).unwrap(),
                        )
                    })
                    .collect();
                (
                    Data::Block {
                        labels,
                        children: statements.clone(),
                    },
                    ValueType::Block(
                        statements
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                )
            }
            4 => {
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let statements: HashMap<String, Value> = entries
                    .iter()
                    .map(|x| {
                        (
                            x.key.clone().as_string().unwrap(),
                            Self::from_binary(x.value.clone()).unwrap(),
                        )
                    })
                    .collect();
                (
                    Data::Section(statements.clone()),
                    ValueType::Section(
                        statements
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                )
            }
            100 => {
                let content = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
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
                (
                    Data::Table(table_content.clone()),
                    ValueType::Table(
                        table_content
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                )
            }
            101 => {
                let content = value.as_array().context(error::MsgPackNotExpectedSnafu)?;
                let mut array_content = Vec::new();
                for entry in content.iter() {
                    array_content.push(Self::from_binary(entry.clone())?);
                }
                (
                    Data::Array(array_content.clone()),
                    ValueType::Array(array_content.iter().map(|x| x.type_of()).collect()),
                )
            }
            102 => {
                let value = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                (Data::String(value), ValueType::String)
            }
            103 => {
                let value = value.as_binary().context(error::MsgPackNotExpectedSnafu)?;
                (Data::Bytes(value), ValueType::Bytes)
            }
            104 => {
                let value = value.as_int().context(error::MsgPackNotExpectedSnafu)?;
                (Data::Int(Int::I64(value)), ValueType::I64)
            }
            105 => {
                let value = value.as_float().context(error::MsgPackNotExpectedSnafu)?;
                (Data::Float(Float::F64(value)), ValueType::F64)
            }
            106 => {
                let value = value.as_boolean().context(error::MsgPackNotExpectedSnafu)?;
                (Data::Bool(value), ValueType::Bool)
            }
            107 => {
                let label = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                (Data::Label(label), ValueType::Label)
            }
            108 => {
                ensure!(value.is_nil(), error::MsgPackUnsupportedSnafu);
                (Data::Null, ValueType::Null)
            }
            109 => {
                let version = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                let version =
                    semver::Version::parse(version.as_str()).context(error::SemVerSnafu {
                        version: version.clone(),
                    })?;
                (Data::Version(version), ValueType::Version)
            }
            110 => {
                let version = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                let version =
                    semver::VersionReq::parse(version.as_str()).context(error::SemVerReqSnafu {
                        version: version.clone(),
                    })?;
                (Data::Require(version), ValueType::Require)
            }
            _ => {
                return error::MsgPackUnsupportedSnafu.fail();
            }
        };
        Ok(Self {
            uid: Uuid::now_v7(),
            id,
            label,
            comment,
            data: Box::new(data),
            type_: value_type,
        })
    }

    /// Find a specific entry in a message pack object
    #[cfg(feature = "binary")]
    fn find_entry(input: &[MapElement], key: &str) -> Option<MsgPack> {
        input.iter().find_map(|x| {
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
    }

    /// Convert this value into a message pack encoded
    /// binary value.
    #[cfg(feature = "binary")]
    pub fn to_binary(&self) -> MsgPack {
        let (type_id, value) = match self.inner() {
            Data::Module(value) => (
                0,
                MsgPack::Map(
                    value
                        .iter()
                        .map(|(k, v)| MapElement {
                            key: MsgPack::String(k.clone()),
                            value: v.to_binary(),
                        })
                        .collect(),
                ),
            ),
            Data::Control(value) => (1, value.to_binary()),
            Data::Assignment(value) => (2, value.to_binary()),
            Data::Block {
                labels,
                children: statements,
            } => (
                3,
                MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("labels".to_string()),
                        value: MsgPack::Array(
                            labels.iter().map(|x| MsgPack::String(x.clone())).collect(),
                        ),
                    },
                    MapElement {
                        key: MsgPack::String("statements".to_string()),
                        value: MsgPack::Map(
                            statements
                                .iter()
                                .map(|(k, v)| MapElement {
                                    key: MsgPack::String(k.clone()),
                                    value: v.to_binary(),
                                })
                                .collect(),
                        ),
                    },
                ]),
            ),
            Data::Section(statements) => (
                4,
                MsgPack::Map(
                    statements
                        .iter()
                        .map(|(k, v)| MapElement {
                            key: MsgPack::String(k.clone()),
                            value: v.to_binary(),
                        })
                        .collect(),
                ),
            ),
            Data::Table(data) => (
                100,
                MsgPack::Map(
                    data.iter()
                        .map(|(k, v)| MapElement {
                            key: MsgPack::String(k.clone()),
                            value: v.to_binary(),
                        })
                        .collect(),
                ),
            ),
            Data::Array(data) => (
                101,
                MsgPack::Array(data.iter().map(|x| x.to_binary()).collect()),
            ),
            Data::String(data) => (102, MsgPack::String(data.clone())),
            Data::Bytes(data) => (103, MsgPack::Binary(data.clone())),
            Data::Int(data) => (104, MsgPack::Int(data.as_int())),
            Data::Float(data) => (105, MsgPack::Float(data.as_float())),
            Data::Bool(data) => (106, MsgPack::Boolean(*data)),
            Data::Label(data) => (107, MsgPack::String(data.clone())),
            Data::Null => (108, MsgPack::Nil),
            Data::Version(version) => (109, MsgPack::String(version.to_string())),
            Data::Require(version) => (110, MsgPack::String(version.to_string())),
            _ => return MsgPack::Nil,
        };

        MsgPack::Map(vec![
            MapElement {
                key: MsgPack::String("id".to_string()),
                value: if let Some(id) = self.id.as_ref() {
                    MsgPack::String(id.clone())
                } else {
                    MsgPack::Nil
                },
            },
            MapElement {
                key: MsgPack::String("label".to_string()),
                value: if let Some(label) = self.label.as_ref() {
                    MsgPack::String(label.clone())
                } else {
                    MsgPack::Nil
                },
            },
            MapElement {
                key: MsgPack::String("comment".to_string()),
                value: if let Some(comment) = self.comment.as_ref() {
                    MsgPack::String(comment.clone())
                } else {
                    MsgPack::Nil
                },
            },
            MapElement {
                key: MsgPack::String("data".to_string()),
                value: MsgPack::Extension(Extension {
                    type_id,
                    value: value.encode(),
                }),
            },
        ])
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self.inner() {
            Data::Module(children) => children
                .iter()
                .map(|x| x.1.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
            Data::Control(value) => format!("${} = {}", self.id().unwrap(), value),
            Data::Assignment(value) => format!("{} = {}", self.id().unwrap(), value),
            Data::Block {
                labels,
                children: statements,
            } => {
                let mut s = format!("{} ", self.id().unwrap());
                for label in labels {
                    s.push_str(&format!("'{}' ", label));
                }
                s.push('{');
                for statement in statements {
                    s.push_str(&format!("\n\t{}", statement.1));
                }
                s.push_str("\n}");
                s
            }
            Data::Section(statements) => {
                let mut s = format!("[{}]", self.id().unwrap());
                for statement in statements {
                    s.push_str(&format!("\n{}", statement.1));
                }
                s
            }
            Data::Table(map) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += "{ ";
                for (index, (key, value)) in map.iter().enumerate() {
                    s += format!("{} = {} ", key, value).as_str();
                    if index != map.len() - 1 {
                        s += ", ";
                    }
                }
                s += " }";
                s
            }
            Data::Array(array) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
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
            Data::String(string) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += format!("'{}'", string).as_str();
                s
            }
            Data::Macro(string, is_string) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
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
            Data::Bytes(bytes) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                let encoded =
                    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice());
                s += format!("b'{}'", encoded).as_str();
                s
            }
            Data::Int(int) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += int.to_string().as_str();
                s
            }
            Data::Float(float) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += float.to_string().as_str();
                s
            }
            Data::Bool(boolean) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += if *boolean { "true" } else { "false" };
                s
            }
            Data::Label(label) => {
                format!("!{}", label)
            }
            Data::Null => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += "null";
                s
            }
            Data::Version(version) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += format!("{}", version).as_str();
                s
            }
            Data::Require(version) => {
                let mut s = String::default();
                if let Some(label) = self.label.as_ref() {
                    s += format!("!{} ", label).as_str();
                }
                s += version.to_string().as_str();
                s
            }
        };
        write!(f, "{}", str)
    }
}
