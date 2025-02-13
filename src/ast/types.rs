use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Defines the type of a given value
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum ValueType {
    /// String values
    String,

    /// Non-precise signed integer (defaults to 64-bit)
    Signed,
    /// 8-bit signed integer
    I8,
    /// 16-bit signed integer
    I16,
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 128-bit signed integer
    I128,

    /// Non-precise unsigned integer (defaults to 64-bit)
    Unsigned,
    /// 8-bit unsigned integer
    U8,
    /// 16-bit unsigned integer
    U16,
    /// 32-bit unsigned integer
    U32,
    /// 64-bit unsigned integer
    U64,
    /// 128-bit unsigned integer
    U128,

    /// Non-precise floating point (defaults to 64-bit)
    Float,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,

    /// Byte Array (Vec<u8>)
    Bytes,

    /// Boolean
    Bool,

    /// Semantic Version
    Version,
    /// Semantic Version Requirement
    Require,

    /// Macro string
    Macro,

    /// Label identifier
    Label,

    /// :Symbol identifier
    Symbol,

    /// Null
    Null,

    /// Array
    Array(Vec<Self>),

    /// Table
    Table(IndexMap<String, Self>),
}

impl ValueType {
    /// Checks if the right type can be assigned to the left when a type requirement
    /// is not specified.
    pub fn can_assign(&self, right: &Self) -> bool {
        match self {
            Self::String => matches!(right, Self::String),
            Self::I64 => matches!(right, Self::I64 | Self::Signed | Self::Unsigned),
            Self::Signed => matches!(
                right,
                Self::I8
                    | Self::I16
                    | Self::I32
                    | Self::I64
                    | Self::Signed
                    | Self::U8
                    | Self::U16
                    | Self::U32
                    | Self::U64
                    | Self::Unsigned
            ),
            Self::U64 => matches!(right, Self::U64) || matches!(right, Self::Unsigned),
            Self::F64 => matches!(right, Self::F64) || matches!(right, Self::Float),
            Self::Unsigned => matches!(
                right,
                Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Unsigned
            ),
            Self::Float => matches!(right, Self::Float | Self::F64 | Self::F32),
            value => value == right,
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::Signed => f.write_str("int"),
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::I128 => f.write_str("i128"),
            Self::Unsigned => f.write_str("uint"),
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::U128 => f.write_str("u128"),
            Self::Float => f.write_str("float"),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::Bytes => f.write_str("bytes"),
            Self::Bool => f.write_str("bool"),
            Self::Version => f.write_str("version"),
            Self::Require => f.write_str("require"),
            Self::Macro => f.write_str("macro"),
            Self::Label => f.write_str("label"),
            Self::Symbol => f.write_str("symbol"),
            Self::Null => f.write_str("null"),
            Self::Array(children) => f.write_fmt(format_args!(
                "[{}]",
                children
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Self::Table(children) => f.write_fmt(format_args!(
                "{{ {} }}",
                children
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(",\n")
            )),
        }
    }
}

/// Represents the type of a statement
#[derive(Debug, Clone, Eq, PartialEq, Deserialize, Serialize)]
pub enum StatementType {
    Control(ValueType),
    Assignment(ValueType),
    Block {
        labels: Vec<ValueType>,
        contents: IndexMap<String, Self>,
    },
    Section(IndexMap<String, Self>),
    Module(IndexMap<String, Self>),
}

/// Stores the metadata associated with a value or statement
/// currently supports a !label and a # Comment
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct Metadata {
    pub location: Location,
    pub comment: Option<String>,
    pub label: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Location {
    pub module: Option<String>,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub(crate) fn set_module(&mut self, module: &str) {
        self.module = Some(module.to_string());
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(module) = self.module.as_ref() {
            f.write_fmt(format_args!("[{}@{}:{}]", module, self.line, self.column))
        } else {
            f.write_fmt(format_args!("[{}:{}]", self.line, self.column))
        }
    }
}
