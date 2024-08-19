use super::types::{Metadata, StatementType, ValueType};
use super::value::{Data, Value};
use crate::{error, Result};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

/// Contains the actual set of data for a statement
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum StatementData {
    /// Blocks need to store the labels and contents
    Labeled(Vec<Value>, IndexMap<String, Statement>),
    /// Sections and Modules are just the substatements
    Group(IndexMap<String, Statement>),
    /// Control and Assignments are to a single value
    Single(Value),
}

/// Represents top-level statements and groupings
#[derive(Clone, Deserialize, Serialize)]
pub struct Statement {
    /// Unique uuid of the statement
    pub uid: Uuid,

    /// Id of the statement
    pub id: String,

    /// Statement type
    pub type_: StatementType,

    /// Metadata for the statement
    pub meta: Metadata,

    /// Data for statement
    pub data: StatementData,
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.data == other.data
    }
}

impl Statement {
    /// Check and convert a value for an assignment
    fn convert(type_: &ValueType, value: &Value) -> Result<Value> {
        match type_ {
            ValueType::Unsigned => Ok(Value {
                uid: value.uid,
                data: Data::Unsigned(match value.type_of() {
                    ValueType::U8 => Ok(*value.as_u8().unwrap() as u64),
                    ValueType::U16 => Ok(*value.as_u16().unwrap() as u64),
                    ValueType::U32 => Ok(*value.as_u32().unwrap() as u64),
                    ValueType::U64 => Ok(*value.as_u64().unwrap()),
                    ValueType::Unsigned => Ok(*value.as_uint().unwrap()),
                    _ => error::ImplicitConvertSnafu {
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
                    ValueType::Unsigned => Ok(*value.as_uint().unwrap()),
                    ValueType::U64 => Ok(*value.as_u64().unwrap()),
                    _ => error::ImplicitConvertSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::Signed => Ok(Value {
                uid: value.uid,
                data: Data::Signed(match value.type_of() {
                    ValueType::U8 => Ok(*value.as_u8().unwrap() as i64),
                    ValueType::U16 => Ok(*value.as_u16().unwrap() as i64),
                    ValueType::U32 => Ok(*value.as_u32().unwrap() as i64),
                    ValueType::U64 => Ok(*value.as_u64().unwrap() as i64),
                    ValueType::Unsigned => Ok(*value.as_uint().unwrap() as i64),
                    ValueType::I8 => Ok(*value.as_i8().unwrap() as i64),
                    ValueType::I16 => Ok(*value.as_i16().unwrap() as i64),
                    ValueType::I32 => Ok(*value.as_i32().unwrap() as i64),
                    ValueType::I64 => Ok(*value.as_i64().unwrap()),
                    ValueType::Signed => Ok(*value.as_int().unwrap()),
                    _ => error::ImplicitConvertSnafu {
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
                    ValueType::Signed => Ok(*value.as_int().unwrap()),
                    ValueType::Unsigned => Ok(*value.as_uint().unwrap() as i64),
                    ValueType::I64 => Ok(*value.as_i64().unwrap()),
                    _ => error::ImplicitConvertSnafu {
                        left: type_.clone(),
                        right: value.type_of(),
                    }
                    .fail(),
                }?),
                meta: value.meta.clone(),
            }),
            ValueType::Float => Ok(Value {
                uid: value.uid,
                data: Data::Float(match value.type_of() {
                    ValueType::F64 => Ok(*value.as_f64().unwrap()),
                    ValueType::Float => Ok(*value.as_float().unwrap()),
                    ValueType::F32 => Ok(*value.as_f32().unwrap() as f64),
                    _ => error::ImplicitConvertSnafu {
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
                    ValueType::Float => Ok(*value.as_float().unwrap()),
                    ValueType::F64 => Ok(*value.as_f64().unwrap()),
                    _ => error::ImplicitConvertSnafu {
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
                    error::ImplicitConvertSnafu {
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
            StatementData::Labeled(labels, ..) => {
                if labels.is_empty() {
                    self.id.clone()
                } else {
                    format!(
                        "{}.{}",
                        self.id,
                        labels
                            .iter()
                            .map(|x| x
                                .to_string()
                                .trim_matches('\'')
                                .trim_matches('"')
                                .to_string())
                            .collect::<Vec<_>>()
                            .join(".")
                    )
                }
            }
            _ => self.id.clone(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = serde_json::to_string_pretty(self).unwrap();
        f.write_str(code.as_str())
    }
}
