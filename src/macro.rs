use indexmap::{IndexMap, IndexSet};
use std::cmp::max;

use snafu::ensure;
use uuid::Uuid;

use crate::error::{self, Result};
use crate::{Data, Value, ValueType};

pub(crate) struct Scope {
    root: Value,
    symbol_table: IndexMap<String, Value>,
    path_lookup: IndexMap<Uuid, String>,
}

impl Scope {
    fn walk(node: &Value, path: Vec<String>) -> IndexMap<String, Value> {
        let mut symbol_table = IndexMap::new();
        let id = node.id().clone();
        match node.inner() {
            Data::Module(ref children)
            | Data::Section(ref children)
            | Data::Block { ref children, .. } => {
                let mut new_path = path.clone();
                if let Some(id) = id.as_ref() {
                    new_path.push(id.clone());
                }
                symbol_table.insert(new_path.join("."), node.clone());
                for child in children.values() {
                    let table = Scope::walk(child, new_path.clone());
                    symbol_table.extend(table);
                }
            }
            Data::Assignment(ref value) => match value.inner() {
                Data::Table(ref children) => {
                    let mut new_path = path.clone();
                    new_path.push(id.unwrap());
                    symbol_table.insert(new_path.join("."), node.clone());
                    for child in children.values() {
                        let table = Scope::walk(child, new_path.clone());
                        symbol_table.extend(table);
                    }
                }
                Data::Array(ref children) => {
                    let mut new_path = path.clone();
                    new_path.push(node.id().unwrap());
                    symbol_table.insert(new_path.join("."), node.clone());
                    for (index, child) in children.iter().enumerate() {
                        let mut a_path = new_path.clone();
                        a_path.push(index.to_string());
                        symbol_table.insert(a_path.join("."), child.clone());
                        // TODO: Consider how to handle nested table and arrays here
                    }
                }
                _ => {
                    let mut new_path = path.clone();
                    new_path.push(id.unwrap());
                    symbol_table.insert(new_path.join("."), value.clone());
                }
            },
            _ => {}
        }
        symbol_table
    }

    pub(crate) fn new(node: &Value) -> Result<Self> {
        let symbol_table = Self::walk(node, Vec::new());
        let mut path_lookup = IndexMap::new();
        for (key, value) in symbol_table.iter() {
            path_lookup.insert(value.uid, key.clone());
        }
        Ok(Self {
            root: node.clone(),
            symbol_table,
            path_lookup,
        })
    }

    pub(crate) fn apply(&mut self) -> Result<Value> {
        let mut visit_log = IndexSet::new();
        let root = self.root.clone();
        self.resolve_phase(&root, &mut visit_log)
    }

    fn resolve_path(&mut self, current: &Value, input: String) -> Result<String> {
        let operating_path: Vec<String> = if input.starts_with("self") || input.starts_with("super")
        {
            let mut input = input.clone();
            let current_path = self.path_lookup.get(&current.uid);
            ensure!(
                current_path.is_some(),
                error::MacroScopeNotFoundSnafu { scope: "null" }
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

    fn resolve_macro_string(
        &mut self,
        at: &Value,
        input: String,
        visit_log: &mut IndexSet<Uuid>,
    ) -> Result<String> {
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
                    None => error::MacroNotFoundSnafu {
                        path: key_string.clone(),
                    }
                    .fail(),
                }?;
                if found.type_of() == ValueType::Macro {
                    // Found is still a macro attempt to resolve it and refetch
                    found = self.resolve_phase(&found, visit_log)?;
                }

                let replacement = found.inner().to_macro_string();
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
        Ok(final_string)
    }

    fn resolve_macro(
        &mut self,
        visit_log: &mut IndexSet<Uuid>,
        value: &Value,
        input: String,
        is_string: bool,
    ) -> Result<Value> {
        // We now need to begin resolution
        let mut new_value = value.clone();
        if is_string {
            let replacement_string = self.resolve_macro_string(value, input.clone(), visit_log)?;
            new_value.data = Box::new(Data::String(replacement_string));
            new_value.type_ = ValueType::String;
        } else {
            // We can assume that the key is a proper path so split it apparent
            let path = self.resolve_path(value, input.clone())?;
            // We need to find the data of the referenced item.
            match self.symbol_table.get(&path) {
                Some(data) => {
                    new_value.data.clone_from(&data.data);
                    new_value.type_ = data.type_of();
                    Ok(())
                }
                None => error::MacroNotFoundSnafu {
                    path: input.clone(),
                }
                .fail(),
            }?;
        }
        Ok(new_value.clone())
    }

    fn resolve_phase(&mut self, at: &Value, visit_log: &mut IndexSet<Uuid>) -> Result<Value> {
        let uid = at.uid;
        let result = match at.inner() {
            Data::Macro(key, is_string) => {
                ensure!(!visit_log.contains(&uid), error::MacroLoopSnafu);
                self.resolve_macro(visit_log, at, key.clone(), *is_string)
            }
            Data::Module(children) => {
                let mut new_children = IndexMap::new();
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_phase(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    id: at.id.clone(),
                    data: Box::new(Data::Module(new_children.clone())),
                    type_: ValueType::Module(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                    label: at.label.clone(),
                })
            }
            Data::Section(children) => {
                let mut new_children = IndexMap::new();
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_phase(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    id: at.id.clone(),
                    data: Box::new(Data::Section(new_children.clone())),
                    type_: ValueType::Section(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                    label: at.label.clone(),
                })
            }
            Data::Table(children) => {
                let mut new_children = IndexMap::new();
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_phase(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    id: at.id.clone(),
                    data: Box::new(Data::Table(new_children.clone())),
                    type_: ValueType::Table(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                    label: at.label.clone(),
                })
            }
            Data::Block { labels, children } => {
                let mut new_children = IndexMap::new();
                for (key, value) in children.iter() {
                    new_children.insert(key.clone(), self.resolve_phase(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    id: at.id.clone(),
                    data: Box::new(Data::Block {
                        labels: labels.clone(),
                        children: new_children.clone(),
                    }),
                    type_: ValueType::Block(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                    label: at.label.clone(),
                })
            }
            Data::Array(children) => {
                let mut new_contents = Vec::new();
                for value in children.iter() {
                    new_contents.push(self.resolve_phase(value, visit_log)?);
                }
                Ok(Value {
                    uid,
                    id: at.id.clone(),
                    data: Box::new(Data::Array(new_contents.clone())),
                    type_: ValueType::Array(new_contents.iter().map(|x| x.type_of()).collect()),
                    comment: at.comment.clone(),
                    label: at.label.clone(),
                })
            }
            Data::Assignment(value) | Data::Control(value) => self.resolve_phase(value, visit_log),
            _ => Ok(at.clone()),
        }?;

        visit_log.insert(uid);
        Ok(result)
    }
}
