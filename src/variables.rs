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
    /// Adds a new variable to this environment, returning an error if the variable already
    /// exists.
    fn add(&mut self, name: Identifier, value: V, mutable: bool) -> Result<(), VariableError>;

    /// Sets the variable, returning an error if it does not exists in this environment.
    fn set(&mut self, name: Identifier, value: V) -> Result<(), VariableError>;

    /// Returns the value of a variable, if it exists in this environment.
    fn get(&self, name: &Identifier) -> Option<&V>;
}

/// A map-like implementation of an environment of named variables
pub(crate) struct VariableMap<'a, V> {
    context: Option<&'a mut dyn Variables<V>>,
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
    pub(crate) fn nested(context: &'a mut dyn Variables<V>) -> Self {
        Self {
            context: Some(context),
            values: HashMap::new(),
        }
    }

    /// Remove a variable from this enviroment, if it exists.
    pub(crate) fn remove(&mut self, name: &Identifier) {
        self.values.remove(name);
    }

    /// Clears this enviroment.
    pub(crate) fn clear(&mut self) {
        self.values.clear();
    }
}

impl<V> Variables<V> for VariableMap<'_, V> {
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

    fn get(&self, name: &Identifier) -> Option<&V> {
        self.values
            .get(name)
            .map(|v| &v.value)
            .or_else(|| self.context.as_ref().map(|p| p.get(name)).flatten())
    }
}

/// Environment of immutable variables
pub struct Globals<'a>(VariableMap<'a, Value>);

impl<'a> Globals<'a> {
    /// Creates a new, empty variable environment.
    pub fn new() -> Self {
        Self(VariableMap::new())
    }

    /// Creates a nested variable environment, that inherits from the given
    /// context environment.
    pub fn nested(context: &'a mut Globals<'a>) -> Self {
        Self(VariableMap::nested(&mut context.0))
    }

    /// Adds a new variable to this environment, returning an error if the variable already
    /// exists.
    pub fn add(&mut self, name: Identifier, value: Value) -> Result<(), VariableError> {
        self.0.add(name, value, false)
    }

    /// Returns the value of a variable, if it exists in this environment.
    pub fn get(&self, name: &Identifier) -> Option<&Value> {
        self.0.get(name)
    }

    /// Remove a variable from this enviroment, if it exists.
    pub fn remove(&mut self, name: &Identifier) {
        self.0.remove(name)
    }

    /// Clears this enviroment.
    pub fn clear(&mut self) {
        self.0.clear()
    }
}
