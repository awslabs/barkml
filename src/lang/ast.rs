use crate::error::{self, Result};
use base64::Engine;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Formatter;
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum ValueType {
    String,
    IGeneric,
    I8,
    I16,
    I32,
    I64,
    I128,
    UGeneric,
    U8,
    U16,
    U32,
    U64,
    U128,
    FGeneric,
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
    Table(IndexMap<String, Self>),
}

impl ValueType {
    pub fn can_assign(&self, right: &Self) -> bool {
        match self {
            Self::String => matches!(right, Self::String),
            Self::I64 => matches!(right, Self::I64 | Self::IGeneric | Self::UGeneric),
            Self::IGeneric => matches!(
                right,
                Self::I8
                    | Self::I16
                    | Self::I32
                    | Self::I64
                    | Self::IGeneric
                    | Self::U8
                    | Self::U16
                    | Self::U32
                    | Self::U64
                    | Self::UGeneric
            ),
            Self::U64 => matches!(right, Self::U64) || matches!(right, Self::UGeneric),
            Self::F64 => matches!(right, Self::F64) || matches!(right, Self::FGeneric),
            Self::UGeneric => matches!(
                right,
                Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::UGeneric
            ),
            Self::FGeneric => matches!(right, Self::FGeneric | Self::F64 | Self::F32),
            value => value == right,
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::IGeneric => f.write_str("int"),
            Self::I8 => f.write_str("i8"),
            Self::I16 => f.write_str("i16"),
            Self::I32 => f.write_str("i32"),
            Self::I64 => f.write_str("i64"),
            Self::I128 => f.write_str("i128"),
            Self::UGeneric => f.write_str("uint"),
            Self::U8 => f.write_str("u8"),
            Self::U16 => f.write_str("u16"),
            Self::U32 => f.write_str("u32"),
            Self::U64 => f.write_str("u64"),
            Self::U128 => f.write_str("u128"),
            Self::FGeneric => f.write_str("float"),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::Bytes => f.write_str("bytes"),
            Self::Bool => f.write_str("bool"),
            Self::Version => f.write_str("version"),
            Self::Require => f.write_str("require"),
            Self::Macro => f.write_str("macro"),
            Self::Label => f.write_str("label"),
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

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Data {
    String(String),
    IGeneric(i64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    UGeneric(u64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    FGeneric(f64),
    F32(f32),
    F64(f64),
    Bytes(Vec<u8>),
    Bool(bool),
    Version(semver::Version),
    Require(semver::VersionReq),
    Macro(String),
    Label(String),
    Null,
    Array(Vec<Value>),
    Table(IndexMap<String, Value>),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct Metadata {
    pub(crate) comment: Option<String>,
    pub(crate) label: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Value {
    pub(crate) uid: Uuid,
    pub(crate) data: Data,
    pub(crate) meta: Metadata,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

macro_rules! variant {
    (fn $fn_new: ident, $fn_as: ident, $fn_mut: ident ($data_type: ty => $key: ident)) => {
        pub fn $fn_new(data: $data_type, meta: Metadata) -> Self {
            Self {
                uid: Uuid::now_v7(),
                data: Data::$key(data.into()),
                meta,
            }
        }

        pub fn $fn_as(&self) -> Option<&$data_type> {
            match &self.data {
                Data::$key(value) => Some(value),
                _ => None,
            }
        }

        pub fn $fn_mut(&mut self) -> Option<&mut $data_type> {
            match &mut self.data {
                Data::$key(value) => Some(value),
                _ => None,
            }
        }
    };
}

impl Value {
    variant!(fn new_string, as_string, as_string_mut (String => String));
    variant!(fn new_int, as_int, as_int_mut (i64 => IGeneric));
    variant!(fn new_i8, as_i8, as_i8_mut (i8 => I8));
    variant!(fn new_i16, as_i16, as_i16_mut (i16 => I16));
    variant!(fn new_i32, as_i32, as_i32_mut (i32 => I32));
    variant!(fn new_i64, as_i64, as_i64_mut (i64 => I64));
    variant!(fn new_i128, as_i128, as_i128_mut (i128 => I128));
    variant!(fn new_uint, as_uint, as_uint_mut (u64 => UGeneric));
    variant!(fn new_u8, as_u8, as_u8_mut (u8 => U8));
    variant!(fn new_u16, as_u16, as_u16_mut (u16 => U16));
    variant!(fn new_u32, as_u32, as_u32_mut (u32 => U32));
    variant!(fn new_u64, as_u64, as_u64_mut (u64 => U64));
    variant!(fn new_u128, as_u128, as_u128_mut (u128 => U128));
    variant!(fn new_float, as_float, as_float_mut (f64 => FGeneric));
    variant!(fn new_f32, as_f32, as_f32_mut (f32 => F32));
    variant!(fn new_f64, as_f64, as_f64_mut (f64 => F64));
    variant!(fn new_bytes, as_bytes, as_bytes_mut (Vec<u8> => Bytes));
    variant!(fn new_bool, as_bool, as_bool_mut (bool => Bool));
    variant!(fn new_version, as_version, as_version_mut (semver::Version => Version));
    variant!(fn new_require, as_require, as_require_mut (semver::VersionReq => Require));
    variant!(fn new_macro, as_macro, as_macro_mut (String => Macro));
    variant!(fn new_label, as_label, as_label_mut (String => Label));
    variant!(fn new_array, as_array, as_array_mut (Vec<Value> => Array));
    variant!(fn new_table, as_table, as_table_mut (IndexMap<String, Value> => Table));

    pub fn new_null(meta: Metadata) -> Self {
        Self {
            uid: Uuid::now_v7(),
            data: Data::Null,
            meta,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self.data, Data::Null)
    }

    pub(crate) fn to_macro_string(&self) -> String {
        match &self.data {
            Data::Macro(value) | Data::Label(value) | Data::String(value) => value.clone(),
            Data::Array(array) => array
                .iter()
                .map(|x| x.to_macro_string())
                .collect::<Vec<_>>()
                .join(","),
            Data::Table(children) => children
                .iter()
                .map(|x| format!("{}:{}", x.0, x.1.to_macro_string()))
                .collect::<Vec<_>>()
                .join(","),
            Data::UGeneric(value) => value.to_string(),
            Data::U8(value) => value.to_string(),
            Data::U16(value) => value.to_string(),
            Data::U32(value) => value.to_string(),
            Data::U64(value) => value.to_string(),
            Data::U128(value) => value.to_string(),
            Data::IGeneric(value) => value.to_string(),
            Data::I8(value) => value.to_string(),
            Data::I16(value) => value.to_string(),
            Data::I32(value) => value.to_string(),
            Data::I64(value) => value.to_string(),
            Data::I128(value) => value.to_string(),
            Data::FGeneric(value) => value.to_string(),
            Data::F32(value) => value.to_string(),
            Data::F64(value) => value.to_string(),
            Data::Bool(value) => if *value { "true" } else { "false" }.to_string(),
            Data::Bytes(value) => {
                base64::engine::general_purpose::STANDARD.encode(value.as_slice())
            }
            Data::Null => "null".to_string(),
            Data::Version(value) => value.to_string(),
            Data::Require(value) => value.to_string(),
        }
    }

    pub fn type_of(&self) -> ValueType {
        match &self.data {
            Data::String(_) => ValueType::String,
            Data::UGeneric(_) => ValueType::UGeneric,
            Data::U8(_) => ValueType::U8,
            Data::U16(_) => ValueType::U16,
            Data::U32(_) => ValueType::U32,
            Data::U64(_) => ValueType::U64,
            Data::U128(_) => ValueType::U128,
            Data::IGeneric(_) => ValueType::IGeneric,
            Data::I8(_) => ValueType::I8,
            Data::I16(_) => ValueType::I16,
            Data::I32(_) => ValueType::I32,
            Data::I64(_) => ValueType::I64,
            Data::I128(_) => ValueType::I128,
            Data::FGeneric(_) => ValueType::FGeneric,
            Data::F32(_) => ValueType::F32,
            Data::F64(_) => ValueType::F64,
            Data::Bytes(_) => ValueType::Bytes,
            Data::Bool(_) => ValueType::Bool,
            Data::Macro(_) => ValueType::Macro,
            Data::Label(_) => ValueType::Label,
            Data::Version(_) => ValueType::Version,
            Data::Require(_) => ValueType::Require,
            Data::Array(values) => ValueType::Array(values.iter().map(|x| x.type_of()).collect()),
            Data::Table(values) => ValueType::Table(
                values
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_of()))
                    .collect(),
            ),
            Data::Null => ValueType::Null,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(comment) = self.meta.comment.as_ref() {
            f.write_fmt(format_args!("/* {comment} /*"))?;
        }
        if let Some(label) = self.meta.label.as_ref() {
            f.write_fmt(format_args!("!{label} "))?;
        }
        match self.data.clone() {
            Data::String(value) => f.write_fmt(format_args!("'{value}'")),
            Data::IGeneric(value) => f.write_fmt(format_args!("{value}")),
            Data::I8(value) => f.write_fmt(format_args!("{value}i8")),
            Data::I16(value) => f.write_fmt(format_args!("{value}i16")),
            Data::I32(value) => f.write_fmt(format_args!("{value}i32")),
            Data::I64(value) => f.write_fmt(format_args!("{value}i64")),
            Data::I128(value) => f.write_fmt(format_args!("{value}i128")),
            Data::UGeneric(value) => f.write_fmt(format_args!("{value}")),
            Data::U8(value) => f.write_fmt(format_args!("{value}u8")),
            Data::U16(value) => f.write_fmt(format_args!("{value}u16")),
            Data::U32(value) => f.write_fmt(format_args!("{value}u32")),
            Data::U64(value) => f.write_fmt(format_args!("{value}u64")),
            Data::U128(value) => f.write_fmt(format_args!("{value}u128")),
            Data::FGeneric(value) => f.write_fmt(format_args!("{value}")),
            Data::F32(value) => f.write_fmt(format_args!("{value}f32")),
            Data::F64(value) => f.write_fmt(format_args!("{value}f64")),
            Data::Bool(value) => {
                if value {
                    f.write_fmt(format_args!("true"))
                } else {
                    f.write_fmt(format_args!("false"))
                }
            }
            Data::Bytes(value) => f.write_fmt(format_args!(
                "b'{}'",
                base64::engine::general_purpose::STANDARD.encode(value.as_slice())
            )),
            Data::Macro(value) => f.write_fmt(format_args!("m!'{value}'")),
            Data::Label(value) => f.write_fmt(format_args!("!{value}")),
            Data::Null => f.write_fmt(format_args!("null")),
            Data::Version(value) => f.write_fmt(format_args!("{value}")),
            Data::Require(value) => f.write_fmt(format_args!("{value}")),
            Data::Array(value) => f.write_fmt(format_args!(
                "[{}]",
                value
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Data::Table(value) => f.write_fmt(format_args!(
                "{{\n{}}}",
                value
                    .iter()
                    .map(|(k, v)| format!("'{k}' = {v}"))
                    .collect::<Vec<_>>()
                    .join(",\n")
            )),
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum StatementData {
    Labeled(Vec<Value>, IndexMap<String, Statement>),
    Group(IndexMap<String, Statement>),
    Single(Value),
}

#[derive(Clone, Deserialize, Serialize)]
pub struct Statement {
    pub(crate) uid: Uuid,
    pub(crate) id: String,
    pub(crate) type_: StatementType,
    pub(crate) meta: Metadata,
    pub(crate) data: StatementData,
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.data == other.data
    }
}

impl Statement {
    fn convert(type_: &ValueType, value: &Value) -> Result<Value> {
        match type_ {
            ValueType::UGeneric => Ok(Value {
                uid: value.uid,
                data: Data::UGeneric(match value.type_of() {
                    ValueType::U8 => Ok(*value.as_u8().unwrap() as u64),
                    ValueType::U16 => Ok(*value.as_u16().unwrap() as u64),
                    ValueType::U32 => Ok(*value.as_u32().unwrap() as u64),
                    ValueType::U64 => Ok(*value.as_u64().unwrap()),
                    ValueType::UGeneric => Ok(*value.as_uint().unwrap()),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::U64 => Ok(Value {
                uid: value.uid,
                data: Data::U64(match value.type_of() {
                    ValueType::UGeneric => Ok(*value.as_uint().unwrap()),
                    ValueType::U64 => Ok(*value.as_u64().unwrap()),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::IGeneric => Ok(Value {
                uid: value.uid,
                data: Data::IGeneric(match value.type_of() {
                    ValueType::U8 => Ok(*value.as_u8().unwrap() as i64),
                    ValueType::U16 => Ok(*value.as_u16().unwrap() as i64),
                    ValueType::U32 => Ok(*value.as_u32().unwrap() as i64),
                    ValueType::U64 => Ok(*value.as_u64().unwrap() as i64),
                    ValueType::UGeneric => Ok(*value.as_uint().unwrap() as i64),
                    ValueType::I8 => Ok(*value.as_i8().unwrap() as i64),
                    ValueType::I16 => Ok(*value.as_i16().unwrap() as i64),
                    ValueType::I32 => Ok(*value.as_i32().unwrap() as i64),
                    ValueType::I64 => Ok(*value.as_i64().unwrap()),
                    ValueType::IGeneric => Ok(*value.as_int().unwrap()),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::I64 => Ok(Value {
                uid: value.uid,
                data: Data::I64(match value.type_of() {
                    ValueType::IGeneric => Ok(*value.as_int().unwrap()),
                    ValueType::UGeneric => Ok(*value.as_uint().unwrap() as i64),
                    ValueType::I64 => Ok(*value.as_i64().unwrap()),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::FGeneric => Ok(Value {
                uid: value.uid,
                data: Data::FGeneric(match value.type_of() {
                    ValueType::F64 => Ok(*value.as_f64().unwrap()),
                    ValueType::FGeneric => Ok(*value.as_float().unwrap()),
                    ValueType::F32 => Ok(*value.as_f32().unwrap() as f64),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::F64 => Ok(Value {
                uid: value.uid,
                data: Data::F64(match value.type_of() {
                    ValueType::FGeneric => Ok(*value.as_float().unwrap()),
                    ValueType::F64 => Ok(*value.as_f64().unwrap()),
                    _ => error::TypeCollisionSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            left => {
                if *left == value.type_of() {
                    Ok(value.clone())
                } else {
                    error::TypeCollisionSnafu {
                        left: left.clone(),
                        right: value.type_of(),
                    }
                    .fail()
                }
            }
        }
    }

    pub fn new_control(
        id: &str,
        type_: Option<ValueType>,
        value: Value,
        meta: Metadata,
    ) -> Result<Self> {
        let type_hint = type_.unwrap_or(value.type_of());
        let value = Self::convert(&type_hint, &value)?;
        Ok(Self {
            uid: Uuid::now_v7(),
            id: id.to_string(),
            type_: StatementType::Control(type_hint),
            meta,
            data: StatementData::Single(value),
        })
    }

    pub fn new_assign(
        id: &str,
        type_: Option<ValueType>,
        value: Value,
        meta: Metadata,
    ) -> Result<Self> {
        let type_hint = type_.unwrap_or(value.type_of());
        let value = Self::convert(&type_hint, &value)?;
        Ok(Self {
            uid: Uuid::now_v7(),
            id: id.to_string(),
            type_: StatementType::Assignment(type_hint),
            meta,
            data: StatementData::Single(value),
        })
    }

    pub fn new_block(
        id: &str,
        labels: Vec<Value>,
        children: IndexMap<String, Statement>,
        meta: Metadata,
    ) -> Self {
        Self {
            uid: Uuid::now_v7(),
            id: id.to_string(),
            type_: StatementType::Block {
                labels: labels.iter().map(|x| x.type_of()).collect(),
                contents: children
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_.clone()))
                    .collect(),
            },
            meta,
            data: StatementData::Labeled(labels, children),
        }
    }

    pub fn new_section(id: &str, children: IndexMap<String, Statement>, meta: Metadata) -> Self {
        Self {
            uid: Uuid::now_v7(),
            id: id.to_string(),
            type_: StatementType::Section(
                children
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_.clone()))
                    .collect(),
            ),
            meta,
            data: StatementData::Group(children),
        }
    }

    pub fn new_module(id: &str, children: IndexMap<String, Statement>, meta: Metadata) -> Self {
        Self {
            uid: Uuid::now_v7(),
            id: id.to_string(),
            type_: StatementType::Module(
                children
                    .iter()
                    .map(|(k, v)| (k.clone(), v.type_.clone()))
                    .collect(),
            ),
            meta,
            data: StatementData::Group(children),
        }
    }

    pub fn get_value(&self) -> Option<&Value> {
        match &self.data {
            StatementData::Single(value) => Some(value),
            _ => None,
        }
    }

    pub fn get_labeled(&self) -> Option<(&Vec<Value>, &IndexMap<String, Statement>)> {
        match &self.data {
            StatementData::Labeled(labels, contents) => Some((labels, contents)),
            _ => None,
        }
    }

    pub fn get_grouped(&self) -> Option<&IndexMap<String, Statement>> {
        match &self.data {
            StatementData::Group(contents) => Some(contents),
            _ => None,
        }
    }

    pub fn inject_id(&self) -> String {
        match &self.data {
            StatementData::Labeled(labels, ..) => format!(
                "{}.{}",
                self.id,
                labels
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            _ => self.id.clone(),
        }
    }

    #[cfg(feature = "binary")]
    pub fn from_binary(input: &[u8]) -> crate::error::Result<Self> {
        rmp_serde::from_slice(input).map_err(|e| error::Error::MsgPackDeserialize {
            reason: e.to_string(),
        })
    }

    #[cfg(feature = "binary")]
    pub fn to_binary(&self) -> crate::error::Result<Vec<u8>> {
        rmp_serde::to_vec(&self).map_err(|e| error::Error::MsgPackSerialize {
            reason: e.to_string(),
        })
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(comment) = self.meta.comment.as_ref() {
            f.write_fmt(format_args!("/*\n{comment}\n*/"))?;
        }
        match &self.type_ {
            StatementType::Control(type_) => f.write_fmt(format_args!(
                "${}: {type_} = {}",
                self.id,
                self.get_value().unwrap()
            )),
            StatementType::Assignment(type_) => f.write_fmt(format_args!(
                "{}: {type_} = {}",
                self.id,
                self.get_value().unwrap()
            )),
            StatementType::Block { .. } => {
                let (labels, body) = self.get_labeled().unwrap();
                let labels = labels
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                let children = body
                    .iter()
                    .map(|x| x.1.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                f.write_fmt(format_args!(
                    "{} {} {{\n {} }}\n",
                    self.id, labels, children
                ))
            }
            StatementType::Section(_) => {
                let body = self.get_grouped().unwrap();
                let children = body
                    .iter()
                    .map(|x| x.1.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                f.write_fmt(format_args!("[{}]\n{}", self.id, children))
            }
            StatementType::Module(_) => {
                let body = self.get_grouped().unwrap();
                let children = body
                    .iter()
                    .map(|x| x.1.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");
                f.write_fmt(format_args!("{}", children))
            }
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let code = serde_json::to_string_pretty(self).unwrap();
        f.write_str(code.as_str())
    }
}
