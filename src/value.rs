use crate::error::{self, Result};
use base64::Engine;
#[cfg(feature = "binary")]
use msgpack_simple::{Extension, MapElement, MsgPack};
use snafu::{ensure, OptionExt, ResultExt};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Comment(String),
    Control {
        label: String,
        value: Box<Self>,
    },
    Assignment {
        label: String,
        value: Box<Self>,
    },
    Block {
        id: String,
        labels: Vec<String>,
        statements: Vec<Self>,
    },
    Section {
        id: String,
        statements: Vec<Self>,
    },

    Table(HashMap<String, Self>, Option<String>),
    Array(Vec<Self>, Option<String>),
    Macro(String, Option<String>, bool),
    String(String, Option<String>),
    Bytes(Vec<u8>, Option<String>),
    Int(i64, Option<String>),
    Float(f64, Option<String>),
    Bool(bool, Option<String>),
    Label(String),
    Null(Option<String>),
}

impl Value {
    pub fn as_section(&self) -> Option<(&String, &Vec<Value>)> {
        match self {
            Self::Section { id, statements } => Some((id, statements)),
            _ => None,
        }
    }

    pub fn as_block(&self) -> Option<(&String, &Vec<String>, &Vec<Value>)> {
        match self {
            Self::Block {
                id,
                labels,
                statements,
            } => Some((id, labels, statements)),
            _ => None,
        }
    }

    pub fn as_assignment(&self) -> Option<(&String, &Value)> {
        match self {
            Self::Assignment { label, value } => Some((label, value)),
            _ => None,
        }
    }

    pub fn as_control(&self) -> Option<(&String, &Value)> {
        match self {
            Self::Control { label, value } => Some((label, value)),
            _ => None,
        }
    }
    pub fn as_comment(&self) -> Option<&String> {
        match self {
            Self::Comment(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<&HashMap<String, Self>> {
        match self {
            Self::Table(value, ..) => Some(value),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Self>> {
        match self {
            Self::Array(value, ..) => Some(value),
            _ => None,
        }
    }

    pub fn as_macro(&self) -> Option<&String> {
        match self {
            Self::Macro(value, ..) => Some(value),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match self {
            Self::String(value, ..) => Some(value),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&Vec<u8>> {
        match self {
            Self::Bytes(value, ..) => Some(value),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Self::Int(value, ..) => Some(*value),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Self::Float(value, ..) => Some(*value),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value, ..) => Some(*value),
            _ => None,
        }
    }

    pub fn as_label(&self) -> Option<&String> {
        match self {
            Self::Label(value) => Some(value),
            _ => None,
        }
    }

    fn is_null(&self) -> bool {
        matches!(self, Self::Null(_))
    }

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

    #[cfg(feature = "binary")]
    pub fn from_binary(entry: msgpack_simple::MsgPack) -> Result<Self> {
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
                Ok(Self::Int(value, label))
            }
            105 => {
                let (label, content) = Self::extract(&ext)?;
                let value = content.as_float().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Float(value, label))
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

    #[cfg(feature = "binary")]
    pub fn to_binary(&self) -> msgpack_simple::MsgPack {
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
                        value: MsgPack::Int(*data),
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
                        value: MsgPack::Float(*data),
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
                s += format!("{}", int).as_str();
                s
            }
            Self::Float(float, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                s += format!("{}", float).as_str();
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
