use crate::Value;

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
            _ => None
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
            Self::Block { id, labels, statements } => {
                Some((id, labels, statements))
            }
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
            _ => None
        }
    }
    pub fn as_comment(&self) -> Option<&String> {
        match self {
            Self::Comment(value) => Some(value),
            _ => None
        }
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Comment(comment) => format!("# {}", comment),
            Statement::Control { label, value } => format!("${} = {}", label, value.to_string()),
            Statement::Assignment { label, value } => format!("{} = {}", label, value.to_string()),
            Statement::Block { id, labels, statements } => {
                let mut s = format!("{} ", id);
                for label in labels {
                    s.push_str(&format!("'{}' ", label));
                }
                s.push_str("{");
                for statement in statements {
                    s.push_str(&format!("\n\t{}", statement.to_string()));
                }
                s.push_str("\n}");
                s
            },
            Statement::Section { id, statements } => {
                let mut s = format!("[{}]", id);
                for statement in statements {
                    s.push_str(&format!("\n{}", statement.to_string()));
                }
                s
            }
            Statement::Value(val) => val.to_string()
        }
    }
}
