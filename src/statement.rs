use crate::error::{self, Result};
use crate::Value;
#[cfg(feature = "binary")]
use msgpack_simple::{Extension, MapElement, MsgPack};
use snafu::{OptionExt, ResultExt};

#[derive(Debug, Clone)]
pub enum Statement {
    Comment(String),
    Control {
        label: String,
        value: Value,
    },
    Assignment {
        label: String,
        value: Value,
    },
    Block {
        id: String,
        labels: Vec<String>,
        statements: Vec<Statement>,
    },
    Section {
        id: String,
        statements: Vec<Statement>,
    },
    Value(Value),
}

impl Statement {
    pub fn as_value(&self) -> Option<&Value> {
        match self {
            Self::Value(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_section(&self) -> Option<(&String, &Vec<Statement>)> {
        match self {
            Self::Section { id, statements } => Some((id, statements)),
            _ => None,
        }
    }

    pub fn as_block(&self) -> Option<(&String, &Vec<String>, &Vec<Statement>)> {
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

    #[cfg(feature = "binary")]
    pub fn from_binary(entry: MsgPack) -> Result<Self> {
        let ext = entry
            .as_extension()
            .context(error::MsgPackNotExpectedSnafu)?;
        let value = MsgPack::parse(ext.value.as_slice()).context(error::MsgPackEncodedSnafu)?;
        match ext.type_id {
            0 => {
                let comment = value.as_string().context(error::MsgPackNotExpectedSnafu)?;
                Ok(Self::Comment(comment))
            }
            1 => {
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let label = Self::find_entry(&entries, "label")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let value = Value::from_binary(Self::find_entry(&entries, "value")?)?;
                Ok(Self::Control { label, value })
            }
            2 => {
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let label = Self::find_entry(&entries, "label")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let value = Value::from_binary(Self::find_entry(&entries, "value")?)?;
                Ok(Self::Assignment { label, value })
            }
            3 => {
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
                let statements: Vec<Statement> = Self::find_entry(&entries, "statements")?
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
                let entries = value.as_map().context(error::MsgPackNotExpectedSnafu)?;
                let id = Self::find_entry(&entries, "id")?
                    .as_string()
                    .context(error::MsgPackNotExpectedSnafu)?;
                let statements: Vec<Statement> = Self::find_entry(&entries, "statements")?
                    .as_array()
                    .context(error::MsgPackNotExpectedSnafu)?
                    .iter()
                    .map(|x| Self::from_binary(x.clone()))
                    .flatten()
                    .collect();
                Ok(Self::Section { id, statements })
            }
            5 => Ok(Self::Value(Value::from_binary(value)?)),
            _ => Err(error::Error::MsgPackUnsupported {}),
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
    pub fn to_binary(&self) -> MsgPack {
        match self {
            Self::Comment(value) => MsgPack::Extension(Extension {
                type_id: 0,
                value: MsgPack::String(value.clone()).encode(),
            }),
            Self::Control { label, value } => MsgPack::Extension(Extension {
                type_id: 1,
                value: MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("label".to_string()),
                        value: MsgPack::String(label.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("value".to_string()),
                        value: value.to_binary(),
                    },
                ])
                .encode(),
            }),
            Self::Assignment { label, value } => MsgPack::Extension(Extension {
                type_id: 2,
                value: MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("label".to_string()),
                        value: MsgPack::String(label.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("value".to_string()),
                        value: value.to_binary(),
                    },
                ])
                .encode(),
            }),
            Self::Block {
                id,
                labels,
                statements,
            } => MsgPack::Extension(Extension {
                type_id: 3,
                value: MsgPack::Map(vec![
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
                ])
                .encode(),
            }),
            Self::Section { id, statements } => MsgPack::Extension(Extension {
                type_id: 4,
                value: MsgPack::Map(vec![
                    MapElement {
                        key: MsgPack::String("id".to_string()),
                        value: MsgPack::String(id.clone()),
                    },
                    MapElement {
                        key: MsgPack::String("statements".to_string()),
                        value: MsgPack::Array(statements.iter().map(|x| x.to_binary()).collect()),
                    },
                ])
                .encode(),
            }),
            Self::Value(value) => MsgPack::Extension(Extension {
                type_id: 5,
                value: value.to_binary().encode(),
            }),
        }
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Comment(comment) => format!("# {}", comment),
            Statement::Control { label, value } => format!("${} = {}", label, value.to_string()),
            Statement::Assignment { label, value } => format!("{} = {}", label, value.to_string()),
            Statement::Block {
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
            Statement::Section { id, statements } => {
                let mut s = format!("[{}]", id);
                for statement in statements {
                    s.push_str(&format!("\n{}", statement.to_string()));
                }
                s
            }
            Statement::Value(val) => val.to_string(),
        }
    }
}
