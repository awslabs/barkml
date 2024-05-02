use crate::error::{self, Result};
use crate::{Data, Value, ValueType};
use parking_lot::{Mutex, RwLock};
use snafu::{ensure, OptionExt, ResultExt};
use std::cmp::max;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Clone)]
pub(crate) struct Scope {
    inner: Arc<RwLock<Inner>>,
}

struct Inner {
    node: Value,
    children: HashMap<String, Scope>,
    parent: Option<Scope>,
}

impl Scope {
    pub(crate) fn new(node: &Value, parent: Option<Self>) -> Result<Self> {
        let me = Self {
            inner: Arc::new(RwLock::new(Inner {
                node: node.clone(),
                children: HashMap::new(),
                parent,
            })),
        };
        println!("Scoping {:?}", node.inner());

        match node.inner() {
            Data::Module(children) | Data::Section(children) | Data::Block { children, .. } => {
                for child in children.values() {
                    let me_ref = me.clone();
                    match child.inner() {
                        Data::Module(_) | Data::Section(_) | Data::Block { .. } => {
                            let scope = Scope::new(child, Some(me_ref.clone()))?;
                            let mut lock = me_ref.inner.write();
                            {
                                lock.children.insert(child.id().unwrap().clone(), scope);
                            }
                        }
                        Data::Assignment(value) => {
                            let id = child.id().unwrap();
                            if value.as_table().is_some() || value.as_array().is_some() {
                                let scope = Scope::new(child, Some(me_ref.clone()))?;
                                let mut lock = me_ref.inner.write();
                                {
                                    lock.children.insert(id.clone(), scope);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
            Data::Assignment(value) => match value.inner() {
                Data::Table(children) => {
                    for child in children.values() {
                        let me_ref = me.clone();
                        match child.inner() {
                            Data::Module(_) | Data::Section(_) | Data::Block { .. } => {
                                let scope = Scope::new(child, Some(me_ref.clone()))?;
                                let mut lock = me_ref.inner.write();
                                {
                                    lock.children.insert(child.id().unwrap().clone(), scope);
                                }
                            }
                            Data::Assignment(value) => {
                                let id = child.id().unwrap();
                                if value.as_table().is_some() || value.as_array().is_some() {
                                    let scope = Scope::new(child, Some(me_ref.clone()))?;
                                    let mut lock = me_ref.inner.write();
                                    {
                                        lock.children.insert(id.clone(), scope);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Ok(())
                }
                Data::Array(children) => {
                    for child in children {
                        let me_ref = me.clone();
                        match child.inner() {
                            Data::Module(_) | Data::Section(_) | Data::Block { .. } => {
                                let scope = Scope::new(child, Some(me_ref.clone()))?;
                                let mut lock = me_ref.inner.write();
                                {
                                    lock.children.insert(child.id().unwrap().clone(), scope);
                                }
                            }
                            Data::Assignment(value) => {
                                let id = child.id().unwrap();
                                if value.as_table().is_some() || value.as_array().is_some() {
                                    let scope = Scope::new(value, Some(me_ref.clone()))?;
                                    let mut lock = me_ref.inner.write();
                                    {
                                        lock.children.insert(id.clone(), scope);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Ok(())
                }
                _ => Err(error::Error::NotScopable {}),
            },
            _ => Err(error::Error::NotScopable {}),
        }?;

        Ok(me.clone())
    }

    pub(crate) fn apply(&self) -> Result<Value> {
        let mut visit_log = HashSet::new();
        let read_lock = self.inner.read();
        let at = { read_lock.node.clone() };
        self.resolve_phase(&at, &mut visit_log)
    }

    fn find(
        &self,
        visit_log: &mut HashSet<Uuid>,
        mut path: VecDeque<String>,
    ) -> Result<Option<Value>> {
        let current = path.pop_front().unwrap();
        if path.is_empty() {
            // We have reached our scope for search
            let read_lock = self.inner.read();
            match read_lock.node.inner().clone() {
                Data::Module(mut children)
                | Data::Section(mut children)
                | Data::Block { mut children, .. }
                | Data::Table(mut children) => {
                    if let Some(child) = children.get(&current) {
                        Ok(Some(self.resolve_phase(child, visit_log)?))
                    } else {
                        Ok(None)
                    }
                }
                Data::Array(mut data) => {
                    let index = current
                        .parse::<usize>()
                        .context(error::MacroNonIndexSnafu {
                            segment: current.clone(),
                        })?;
                    if let Some(child) = data.get(index) {
                        Ok(Some(self.resolve_phase(child, visit_log)?))
                    } else {
                        Ok(None)
                    }
                }
                _ => Ok(None),
            }
        } else {
            // We need to continue searching
            if current == "this" || current == "self" {
                // We are searching for the current scope
                return self.find(visit_log, path);
            } else if current == "super" {
                // We are searching for the parent scope
                let read_lock = self.inner.read();
                if let Some(parent) = read_lock.parent.as_ref() {
                    return parent.find(visit_log, path);
                } else {
                    return Ok(None);
                }
            }
            self.inner
                .read()
                .children
                .get(&current)
                .context(error::MacroScopeNotFoundSnafu {
                    scope: current.clone(),
                })?
                .clone()
                .find(visit_log, path)
        }
    }

    fn resolve_macro_string(&self, input: String, visit_log: &mut HashSet<Uuid>) -> Result<String> {
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
                let key: VecDeque<String> = key_string.split(".").map(|x| x.to_string()).collect();

                let replacement = match self.find(visit_log, key)? {
                    Some(value) => Ok(value.inner().to_macro_string()),
                    None => Err(error::Error::MacroNotFound {
                        path: key_string.clone(),
                    }),
                }?;
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
        &self,
        visit_log: &mut HashSet<Uuid>,
        value: Value,
        input: String,
        is_string: bool,
    ) -> Result<Value> {
        // We now need to begin resolution
        if is_string {
            let replacement_string = self.resolve_macro_string(input.clone(), visit_log)?;
            Ok(Value {
                uid: value.uid.clone(),
                id: value.id(),
                type_: ValueType::String,
                data: Box::new(Data::String(replacement_string)),
                label: value.label.clone(),
                comment: value.comment.clone(),
            })
        } else {
            // We can assume that the key is a proper path so split it apparent
            let path: VecDeque<String> = input.split('.').map(|x| x.to_string()).collect();
            // We need to find the data of the referenced item.
            let data = self
                .find(visit_log, path)?
                .context(error::MacroNotFoundSnafu {
                    path: input.clone(),
                })?;
            Ok(Value {
                uid: value.uid.clone(),
                id: value.id(),
                type_: data.type_of(),
                data: data.data.clone(),
                label: value.label.clone(),
                comment: value.comment.clone(),
            })
        }
    }

    fn resolve_phase(&self, at: &Value, visit_log: &mut HashSet<Uuid>) -> Result<Value> {
        let uid = at.uid.clone();
        let scope = self.inner.read();
        let result = match at.inner() {
            Data::Macro(key, is_string) => {
                ensure!(!visit_log.contains(&uid), error::MacroLoopSnafu);
                self.resolve_macro(visit_log, at.clone(), key.clone(), *is_string)?
            }
            Data::Module(children) => {
                let scope = if uid == scope.node.uid {
                    self
                } else {
                    scope.children.get(at.id().as_ref().unwrap()).context(
                        error::MacroScopeNotFoundSnafu {
                            scope: at.id().unwrap().clone(),
                        },
                    )?
                };
                let mut new_children = HashMap::new();
                for (key, value) in children {
                    new_children.insert(key.clone(), scope.resolve_phase(value, visit_log)?);
                }
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Module(new_children.clone())),
                    type_: ValueType::Module(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                }
            }
            Data::Section(children) => {
                let scope = if uid == scope.node.uid {
                    self
                } else {
                    scope.children.get(at.id().as_ref().unwrap()).context(
                        error::MacroScopeNotFoundSnafu {
                            scope: at.id().unwrap().clone(),
                        },
                    )?
                };
                let mut new_children = HashMap::new();
                for (key, value) in children {
                    new_children.insert(key.clone(), scope.resolve_phase(value, visit_log)?);
                }
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Section(new_children.clone())),
                    type_: ValueType::Section(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                }
            }
            Data::Block { labels, children } => {
                let scope = if uid == scope.node.uid {
                    self
                } else {
                    scope.children.get(at.id().as_ref().unwrap()).context(
                        error::MacroScopeNotFoundSnafu {
                            scope: at.id().unwrap().clone(),
                        },
                    )?
                };
                let mut new_children = HashMap::new();
                for (key, value) in children {
                    new_children.insert(key.clone(), scope.resolve_phase(value, visit_log)?);
                }
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
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
                }
            }
            Data::Assignment(value) => {
                let new_value = if value.as_table().is_some() || value.as_array().is_some() {
                    let scope = if uid == scope.node.uid {
                        self
                    } else {
                        scope.children.get(at.id().as_ref().unwrap()).context(
                            error::MacroScopeNotFoundSnafu {
                                scope: at.id().unwrap().clone(),
                            },
                        )?
                    };

                    scope.resolve_phase(value, visit_log)?
                } else {
                    self.resolve_phase(value, visit_log)?
                };
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Assignment(new_value.clone())),
                    type_: new_value.type_of(),
                    comment: at.comment.clone(),
                }
            }
            Data::Control(value) => {
                let new_value = if value.as_table().is_some() || value.as_array().is_some() {
                    let scope = if uid == scope.node.uid {
                        self
                    } else {
                        scope.children.get(at.id().as_ref().unwrap()).context(
                            error::MacroScopeNotFoundSnafu {
                                scope: at.id().unwrap().clone(),
                            },
                        )?
                    };

                    scope.resolve_phase(value, visit_log)?
                } else {
                    self.resolve_phase(value, visit_log)?
                };
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Control(new_value.clone())),
                    type_: new_value.type_of(),
                    comment: at.comment.clone(),
                }
            }
            Data::Table(children) => {
                let mut new_children = HashMap::new();
                for (key, value) in children {
                    new_children.insert(key.clone(), self.resolve_phase(value, visit_log)?);
                }
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Table(new_children.clone())),
                    type_: ValueType::Table(
                        new_children
                            .iter()
                            .map(|(k, v)| (k.clone(), v.type_of()))
                            .collect(),
                    ),
                    comment: at.comment.clone(),
                }
            }
            Data::Array(data) => {
                let mut new_data = Vec::new();
                for child in data {
                    new_data.push(self.resolve_phase(child, visit_log)?);
                }
                Value {
                    uid: uid.clone(),
                    id: at.id(),
                    label: at.label.clone(),
                    data: Box::new(Data::Array(new_data.clone())),
                    type_: ValueType::Array(new_data.iter().map(|v| v.type_of()).collect()),
                    comment: at.comment.clone(),
                }
            }
            _ => at.clone(),
        };
        let mut write_lock = self.inner.write();
        {
            write_lock.node = result.clone();
        }
        visit_log.insert(uid);
        Ok(result.clone())
    }
}
