use super::types::{StatementType, ValueType};
use super::{error, Result};
use super::{Data, Statement, StatementData, Value};
use indexmap::{IndexMap, IndexSet};
use snafu::ensure;
use std::cmp::max;
use uuid::Uuid;

/// Scope is used to resolve a parent statement (should be a module)
/// to replace all macros with their appropriate values
pub struct Scope {
    root: Statement,
    symbol_table: IndexMap<String, Value>,
    path_lookup: IndexMap<Uuid, String>,
}

impl Scope {
    fn walk(scope: &mut Scope, node: &Statement, path: Vec<String>) {
        let mut new_path = path.clone();
        new_path.push(node.id.clone());
        match &node.data {
            StatementData::Group(children) | StatementData::Labeled(_, children) => {
                for child in children.values() {
                    Self::walk(scope, child, new_path.clone());
                }
            }
            StatementData::Single(value) => {
                Self::walk_value(scope, value, new_path);
            }
        }
    }

    fn walk_value(scope: &mut Scope, node: &Value, path: Vec<String>) {
        Self::add_symbol(scope, path.clone(), node);
        match &node.data {
            Data::Table(contents) => {
                for (key, value) in contents {
                    let mut new_path = path.clone();
                    new_path.push(key.clone());
                    Self::walk_value(scope, value, new_path);
                }
            }
            Data::Array(contents) => {
                for (index, child) in contents.iter().enumerate() {
                    let mut a_path = path.clone();
                    a_path.push(index.to_string());
                    Self::walk_value(scope, child, a_path);
                }
            }
            _ => {}
        }
    }

    fn add_symbol(scope: &mut Scope, path: Vec<String>, node: &Value) {
        let key = path.join(".");
        scope.symbol_table.insert(key.clone(), node.clone());
        scope.path_lookup.insert(node.uid, key.clone());
    }

    pub fn new(node: &Statement) -> Self {
        let mut scope = Self {
            root: node.clone(),
            symbol_table: IndexMap::new(),
            path_lookup: IndexMap::new(),
        };
        Self::walk(&mut scope, node, Vec::new());
        scope
    }

    pub fn apply(&mut self) -> Result<Statement> {
        // First we need to walk our root and populate the symbol table
        let mut visit_log = IndexSet::new();
        let root = self.root.clone();
        self.resolve_statement(&root, &mut visit_log)
    }

    fn resolve_path(&mut self, current: &Value, input: String) -> Result<String> {
        let operating_path: Vec<String> = if input.starts_with("self") || input.starts_with("super")
        {
            let mut input = input.clone();
            let current_path = self.path_lookup.get(&current.uid);
            ensure!(
                current_path.is_some(),
                error::NotFoundSnafu {
                    location: current.meta.location.clone(),
                    path: "unknown"
                }
            );
            if input.starts_with("super") {
                let mut current_segments: Vec<&str> = current_path.unwrap().split('.').collect();
                let mut new_segments: Vec<&str> = input.split('.').collect();
                current_segments.pop();
                current_segments.append(&mut new_segments);
                current_segments.iter().map(|x| x.to_string()).collect()
            } else {
                input = input.replace("self", current_path.unwrap());
                input.split('.').map(|x| x.to_string()).collect()
            }
        } else {
            input.split('.').map(|x| x.to_string()).collect()
        };

        let mut final_path: Vec<String> = Vec::new();
        for entry in operating_path {
            if entry == "this" || entry == "self" {
                continue;
            } else if entry == "super" {
                final_path.pop();
            } else {
                final_path.push(entry.to_string());
            }
        }

        Ok(final_path.join("."))
    }

    fn resolve_macro(
        &mut self,
        at: &Value,
        input: String,
        visit_log: &mut IndexSet<Uuid>,
    ) -> Result<Value> {
        // First check if the whole string is a singular reference to a macro value
        let path = self.resolve_path(at, input.clone())?;
        if let Some(data) = self.symbol_table.get(&path) {
            Ok(Value {
                uid: at.uid,
                data: data.data.clone(),
                meta: at.meta.clone(),
            })
        } else {
            // Otherwise it must be a macro string so lets try and process it. If there is no occurent of { we need to error
            ensure!(
                input.contains('{'),
                error::NotFoundSnafu {
                    location: at.meta.location.clone(),
                    path: input.clone()
                }
            );
            let mut start_index = -1;
            let original = input.clone();
            let mut final_string = original.clone();
            let mut offset: i64 = 0;
            let mut count = 0;
            for (i, c) in input.chars().enumerate() {
                if c == '{' && start_index == -1 {
                    start_index = i as i64;
                } else if c == '}' && start_index != -1 {
                    count += 1;
                    let copy = original.clone();
                    let (before, after) = copy.split_at(start_index as usize);
                    let (middle, _) = after.split_at(i - before.len());
                    let key_string = middle[1..].to_string();
                    let path = self.resolve_path(at, key_string.clone())?;

                    let mut found = match self.symbol_table.get_mut(&path) {
                        Some(value) => Ok(value.clone()),
                        None => error::NotFoundSnafu {
                            location: at.meta.location.clone(),
                            path: key_string.clone(),
                        }
                        .fail(),
                    }?;
                    if found.type_of() == ValueType::Macro {
                        // Found is still a macro attempt to resolve it and refetch
                        found = self.resolve_value(&found, visit_log)?;
                    }

                    let replacement = found.to_macro_string();
                    let sindex = if offset == 0 {
                        i as i64 - count
                    } else {
                        offset + 1
                    };
                    let (before, rem) = final_string.split_at(max(sindex, 0) as usize);
                    let (_, mut after) = rem.split_at((count + 1) as usize);
                    after = if after.starts_with('}') {
                        after.strip_prefix('}').unwrap()
                    } else {
                        after
                    };
                    final_string = before.to_string() + replacement.as_str() + after;
                    offset += if replacement.len() >= count as usize {
                        replacement.len() as i64
                    } else {
                        replacement.len() as i64 - count
                    };
                    start_index = -1;
                    count = 0;
                } else if start_index != -1 {
                    count += 1;
                }
            }
            Ok(Value {
                uid: at.uid,
                data: Data::String(final_string),
                meta: at.meta.clone(),
            })
        }
    }

    fn resolve_statement(
        &mut self,
        at: &Statement,
        visit_log: &mut IndexSet<Uuid>,
    ) -> Result<Statement> {
        let uid = at.uid;
        let result = match &at.type_ {
            StatementType::Module(_) => {
                let mut new_children = IndexMap::new();
                for (key, value) in at.get_grouped().unwrap() {
                    new_children.insert(key.clone(), self.resolve_statement(value, visit_log)?);
                }
                Ok(Statement {
                    uid,
                    id: at.id.clone(),
                    data: StatementData::Group(new_children.clone()),
                    type_: StatementType::Module(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_.clone()))
                            .collect(),
                    ),
                    meta: at.meta.clone(),
                })
            }
            StatementType::Section(_) => {
                let mut new_children = IndexMap::new();
                for (key, value) in at.get_grouped().unwrap() {
                    new_children.insert(key.clone(), self.resolve_statement(value, visit_log)?);
                }
                Ok(Statement {
                    uid,
                    id: at.id.clone(),
                    data: StatementData::Group(new_children.clone()),
                    type_: StatementType::Section(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_.clone()))
                            .collect(),
                    ),
                    meta: at.meta.clone(),
                })
            }
            StatementType::Block { .. } => {
                let mut new_children = IndexMap::new();
                let mut new_labels = Vec::new();
                let (labels, children) = at.get_labeled().unwrap();
                for label in labels {
                    new_labels.push(self.resolve_value(label, visit_log)?);
                }
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_statement(value, visit_log)?);
                }
                Ok(Statement {
                    uid,
                    id: at.id.clone(),
                    data: StatementData::Labeled(new_labels.clone(), new_children.clone()),
                    type_: StatementType::Block {
                        labels: new_labels.iter().map(|v| v.type_of()).collect(),
                        contents: new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_.clone()))
                            .collect(),
                    },
                    meta: at.meta.clone(),
                })
            }
            StatementType::Control(expected) => {
                let new_value = self.resolve_value(at.get_value().unwrap(), visit_log)?;
                ensure!(
                    new_value.type_of() == *expected,
                    error::ImplicitConvertSnafu {
                        left: expected.clone(),
                        right: new_value.type_of()
                    }
                );
                Ok(Statement {
                    uid,
                    id: at.id.clone(),
                    data: StatementData::Single(new_value.clone()),
                    type_: StatementType::Control(new_value.type_of()),
                    meta: at.meta.clone(),
                })
            }
            StatementType::Assignment(expected) => {
                let new_value = self.resolve_value(at.get_value().unwrap(), visit_log)?;
                ensure!(
                    new_value.type_of() == *expected,
                    error::ImplicitConvertSnafu {
                        left: expected.clone(),
                        right: new_value.type_of()
                    }
                );
                Ok(Statement {
                    uid,
                    id: at.id.clone(),
                    data: StatementData::Single(new_value.clone()),
                    type_: StatementType::Assignment(new_value.type_of()),
                    meta: at.meta.clone(),
                })
            }
        }?;
        visit_log.insert(uid);
        Ok(result)
    }

    fn resolve_value(&mut self, at: &Value, visit_log: &mut IndexSet<Uuid>) -> Result<Value> {
        let uid = at.uid;
        let result = match &at.data {
            Data::Macro(value) => {
                ensure!(
                    !visit_log.contains(&uid),
                    error::LoopSnafu {
                        location: at.meta.location.clone()
                    }
                );
                self.resolve_macro(at, value.clone(), visit_log)
            }
            Data::Table(children) => {
                let mut new_children = IndexMap::new();
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_value(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    data: Data::Table(new_children),
                    meta: at.meta.clone(),
                })
            }
            Data::Array(children) => {
                let mut new_children = Vec::new();
                for value in children.iter() {
                    new_children.push(self.resolve_value(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    data: Data::Array(new_children),
                    meta: at.meta.clone(),
                })
            }
            _ => Ok(at.clone()),
        }?;
        visit_log.insert(uid);
        Ok(result)
    }
}
