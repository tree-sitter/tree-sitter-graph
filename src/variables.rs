// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::collections::hash_map::Entry::Occupied;
use std::collections::hash_map::Entry::Vacant;
use std::collections::HashMap;
use thiserror::Error;

use crate::graph::Value;
use crate::Identifier;

#[derive(Debug, Error)]
pub enum VariableError {
    #[error("Cannot assign immutable variable")]
    CannotAssignImmutableVariable(String),
    #[error("Variable already defined")]
    VariableAlreadyDefined(String),
    #[error("Undefined variable")]
    UndefinedVariable(String),
}

/// An environment of named variables
pub(crate) trait Variables<V> {
    /// Returns the value of a variable, if it exists in this environment.
    fn get(&self, name: &Identifier) -> Option<&V>;
}

pub(crate) trait MutVariables<V>: Variables<V> {
    /// Adds a new variable to this environment, returning an error if the variable already
    /// exists.
    fn add(&mut self, name: Identifier, value: V, mutable: bool) -> Result<(), VariableError>;

    /// Sets the variable, returning an error if it does not exists in this environment.
    fn set(&mut self, name: Identifier, value: V) -> Result<(), VariableError>;
}

/// A map-like implementation of an environment of named variables
pub(crate) struct VariableMap<'a, V> {
    context: Option<&'a mut dyn MutVariables<V>>,
    values: HashMap<Identifier, Variable<V>>,
}

struct Variable<V> {
    value: V,
    mutable: bool,
}

impl<'a, V> VariableMap<'a, V> {
    /// Creates a new, empty variable environment.
    pub(crate) fn new() -> Self {
        Self {
            context: None,
            values: HashMap::new(),
        }
    }

    /// Creates a nested variable environment, that inherits from the given
    /// context environment.
    pub(crate) fn nested(context: &'a mut dyn MutVariables<V>) -> Self {
        Self {
            context: Some(context),
            values: HashMap::new(),
        }
    }

    /// Clears this enviroment.
    pub(crate) fn clear(&mut self) {
        self.values.clear();
    }
}

impl<V> Variables<V> for VariableMap<'_, V> {
    fn get(&self, name: &Identifier) -> Option<&V> {
        self.values
            .get(name)
            .map(|v| &v.value)
            .or_else(|| self.context.as_ref().map(|p| p.get(name)).flatten())
    }
}

impl<V> MutVariables<V> for VariableMap<'_, V> {
    fn add(&mut self, name: Identifier, value: V, mutable: bool) -> Result<(), VariableError> {
        match self.values.entry(name) {
            Vacant(v) => {
                let variable = Variable { value, mutable };
                v.insert(variable);
                Ok(())
            }
            Occupied(o) => Err(VariableError::VariableAlreadyDefined(o.key().to_string())),
        }
    }

    fn set(&mut self, name: Identifier, value: V) -> Result<(), VariableError> {
        match self.values.entry(name) {
            Vacant(v) => self
                .context
                .as_mut()
                .map(|context| context.set(v.key().clone(), value))
                .unwrap_or(Err(VariableError::UndefinedVariable(
                    v.into_key().to_string(),
                ))),
            Occupied(mut o) => {
                let mut variable = o.get_mut();
                if variable.mutable {
                    variable.value = value;
                    Ok(())
                } else {
                    Err(VariableError::CannotAssignImmutableVariable(
                        o.key().to_string(),
                    ))
                }
            }
        }
    }
}

/// Environment of immutable variables
pub struct Globals<'a> {
    context: Option<&'a dyn Variables<Value>>,
    values: HashMap<Identifier, Value>,
}

impl<'a> Globals<'a> {
    /// Creates a new, empty variable environment.
    pub fn new() -> Self {
        Self {
            context: None,
            values: HashMap::new(),
        }
    }

    /// Creates a nested variable environment, that inherits from the given
    /// context environment.
    pub fn nested(context: &'a Globals<'a>) -> Self {
        Self {
            context: Some(context),
            values: HashMap::new(),
        }
    }

    /// Adds a new variable to this environment, returning an error if the variable already
    /// exists.
    pub fn add(&mut self, name: Identifier, value: Value) -> Result<(), VariableError> {
        match self.values.entry(name) {
            Vacant(v) => {
                v.insert(value);
                Ok(())
            }
            Occupied(o) => Err(VariableError::VariableAlreadyDefined(o.key().to_string())),
        }
    }

    /// Returns the value of a variable, if it exists in this environment.
    pub fn get(&self, name: &Identifier) -> Option<&Value> {
        self.values
            .get(name)
            .or_else(|| self.context.as_ref().map(|p| p.get(name)).flatten())
    }

    /// Remove a variable from this enviroment, if it exists.
    pub fn remove(&mut self, name: &Identifier) {
        self.values.remove(name);
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn iter<'b>(&'b self) -> Iter<'b> {
        Iter(self.values.iter())
    }

    /// Clears this enviroment.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

pub struct Iter<'a>(std::collections::hash_map::Iter<'a, Identifier, Value>);

impl<'a> std::iter::Iterator for Iter<'a> {
    type Item = (&'a Identifier, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Variables<Value> for Globals<'_> {
    fn get(&self, name: &Identifier) -> Option<&Value> {
        self.get(name)
    }
}
