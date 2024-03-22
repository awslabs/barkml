use std::collections::HashMap;
use base64::Engine;

#[derive(Debug, Clone)]
pub enum Value {
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
            _ => None
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
            _ => None
        }
    }

    pub fn as_label(&self) -> Option<&String> {
        match self {
            Self::Label(value) => Some(value),
            _ => None,
        }
    }

    fn is_null(&self) -> bool {
        match self {
            Self::Null(_) => true,
            _ => false,
        }
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
        }
    }

    pub(crate) fn to_macro_string(&self) -> String {
        match self {
            Self::Table(..) => self.to_string(),
            Self::Array(children, _) => children.iter().map(|x| x.to_macro_string()).collect::<Vec<String>>().join(", "),
            Self::Bytes(bytes, _) => base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice()),
            Self::Int(value, _) => value.to_string(),
            Self::Float(value, _ ) => value.to_string(),
            Self::Bool(value, _) => value.to_string(),
            Self::Label(value) => value.clone(),
            Self::Null(_) => "null".to_owned(),
            Self::Macro(..) => self.to_string(),
            Self::String(value, _) => value.clone(),
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
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
                s += format!("[{}]", array.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")).as_str();
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
                let m = if *is_string { format!("m!'{}'", string) } else { format!("m!{}", string) };
                s += m.as_str();
                s
            }
            Self::Bytes(bytes, label) => {
                let mut s = String::default();
                if let Some(label) = label {
                    s += format!("!{} ", label).as_str();
                }
                let encoded = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes.as_slice());
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
