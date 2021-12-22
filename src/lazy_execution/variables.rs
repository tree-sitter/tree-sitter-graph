// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines variable context for lazy DSL evaluation

use log::debug;

use crate::graph::Graph;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use super::store::DebugInfo;
use super::store::Store;
use super::values::Value;
use super::DisplayWithContextAndGraph;

/// An environment of named lazy variables
pub trait Variables {
    /// Adds a new variable to an environment, returning an error if the variable already
    /// exists.
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutable: bool,
        debug_info: DebugInfo,
        exec: &mut VariableContext,
    ) -> Result<(), ()>;

    /// Sets the variable, returning an error if it does not exists in this environment.
    fn set(
        &mut self,
        name: Identifier,
        value: Value,
        debug_info: DebugInfo,
        exec: &mut VariableContext,
    ) -> Result<(), ()>;

    /// Returns the value of a variable, if it exists in this environment.
    fn get(&self, name: Identifier, exec: &VariableContext) -> Option<&Value>;
}

pub struct VariableContext<'a, 'tree> {
    pub ctx: &'a Context,
    pub graph: &'a Graph<'tree>,
    pub store: &'a mut Store,
}

/// A map-like implementation of an environment of named lazy variables
#[derive(Default)]
pub struct VariableMap<'a> {
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
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new, empty environment of variables.
    pub fn new_child(parent: &'a mut dyn Variables) -> Self {
        Self {
            parent: Some(parent),
            values: Vec::default(),
        }
    }

    /// Clears this list of variables.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

impl Variables for VariableMap<'_> {
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutable: bool,
        debug_info: DebugInfo,
        exec: &mut VariableContext,
    ) -> Result<(), ()> {
        if mutable {
            debug!(
                "var {} = {}",
                name.display_with(exec.ctx),
                value.display_with(exec.ctx, exec.graph)
            );
        } else {
            debug!(
                "let {} = {}",
                name.display_with(exec.ctx),
                value.display_with(exec.ctx, exec.graph)
            );
        }
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(_) => Err(()),
            Err(index) => {
                let value = exec.store.add(value, debug_info, exec.ctx, exec.graph);
                let variable = NamedVariable {
                    name,
                    value: value.into(),
                    mutable,
                };
                self.values.insert(index, variable);
                Ok(())
            }
        }
    }

    fn set(
        &mut self,
        name: Identifier,
        value: Value,
        debug_info: DebugInfo,
        exec: &mut VariableContext,
    ) -> Result<(), ()> {
        debug!(
            "set {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => {
                let value = exec.store.add(value, debug_info, exec.ctx, exec.graph);
                let variable = &mut self.values[index];
                if variable.mutable {
                    variable.value = value.into();
                    Ok(())
                } else {
                    Err(())
                }
            }
            Err(_) => self
                .parent
                .as_mut()
                .map(|parent| parent.set(name, value, debug_info, exec))
                .unwrap_or(Err(())),
        }
    }

    fn get(&self, name: Identifier, exec: &VariableContext) -> Option<&Value> {
        debug!("get {}", name.display_with(exec.ctx));
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => Some(&self.values[index].value),
            Err(_) => self
                .parent
                .as_ref()
                .map(|parent| parent.get(name, exec))
                .flatten(),
        }
    }
}
