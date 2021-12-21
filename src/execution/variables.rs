// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::collections::HashMap;

use crate::graph::SyntaxNodeRef;
use crate::graph::Value;
use crate::Identifier;

/// An environment of named variables
pub(super) trait Variables {
    /// Adds a new variable to an environment, returning an error if the variable already
    /// exists.
    fn add(&mut self, name: Identifier, value: Value, mutable: bool) -> Result<(), ()>;

    /// Sets the variable, returning an error if it does not exists in this environment.
    fn set(&mut self, name: Identifier, value: Value) -> Result<(), ()>;

    /// Returns the value of a variable, if it exists in this environment.
    fn get(&self, name: Identifier) -> Option<&Value>;
}

/// Global immutable variables
pub struct Globals<'a>(VariableMap<'a>);

impl<'a> Globals<'a> {
    pub fn new() -> Self {
        Self(VariableMap::new())
    }

    pub fn add(&mut self, name: Identifier, value: Value) -> Result<(), ()> {
        self.0.add(name, value, false)
    }

    pub fn get(&self, name: Identifier) -> Option<&Value> {
        self.0.get(name)
    }

    pub fn remove(&mut self, name: Identifier) {
        self.0.remove(name)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }
}

/// A map-like implementation of an environment of named variables
#[derive(Default)]
pub(super) struct VariableMap<'a> {
    parent: Option<&'a mut dyn Variables>,
    values: Vec<NamedVariable>,
}

struct NamedVariable {
    name: Identifier,
    value: Value,
    mutable: bool,
}

impl<'a> VariableMap<'a> {
    /// Creates a new, empty environment of variables.
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Creates a new, empty environment of variables.
    pub(super) fn new_child(parent: &'a mut dyn Variables) -> Self {
        Self {
            parent: Some(parent),
            values: Vec::default(),
        }
    }

    pub(super) fn remove(&mut self, name: Identifier) {
        if let Ok(index) = self.values.binary_search_by_key(&name, |v| v.name) {
            self.values.remove(index);
        }
    }

    /// Clears this list of variables.
    pub(super) fn clear(&mut self) {
        self.values.clear();
    }
}

impl Variables for VariableMap<'_> {
    fn add(&mut self, name: Identifier, value: Value, mutable: bool) -> Result<(), ()> {
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(_) => Err(()),
            Err(index) => {
                let variable = NamedVariable {
                    name,
                    value,
                    mutable,
                };
                self.values.insert(index, variable);
                Ok(())
            }
        }
    }

    fn set(&mut self, name: Identifier, value: Value) -> Result<(), ()> {
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => {
                let variable = &mut self.values[index];
                if variable.mutable {
                    variable.value = value;
                    Ok(())
                } else {
                    Err(())
                }
            }
            Err(_) => self
                .parent
                .as_mut()
                .map(|parent| parent.set(name, value))
                .unwrap_or(Err(())),
        }
    }

    fn get(&self, name: Identifier) -> Option<&Value> {
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => Some(&self.values[index].value),
            Err(_) => self
                .parent
                .as_ref()
                .map(|parent| parent.get(name))
                .flatten(),
        }
    }
}

#[derive(Default)]
pub(super) struct ScopedVariables<'a> {
    scopes: HashMap<SyntaxNodeRef, VariableMap<'a>>,
}

impl<'a> ScopedVariables<'a> {
    pub(super) fn new() -> Self {
        ScopedVariables::default()
    }

    pub(super) fn get(&mut self, scope: SyntaxNodeRef) -> &mut VariableMap<'a> {
        self.scopes.entry(scope).or_default()
    }
}
