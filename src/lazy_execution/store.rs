// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines store and thunks for lazy DSL evaluation

use anyhow::Context as _;
use log::trace;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::rc::Rc;

use crate::execution::ExecutionError;
use crate::graph;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::SyntaxNodeRef;
use crate::parser::Location;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use super::values::*;
use super::DisplayWithContextAndGraph;
use super::EvaluationContext;

/// Variable that points to a thunk in the store
#[derive(Clone, Debug)]
pub struct Variable {
    store_location: usize,
}

impl Variable {
    fn new(store_location: usize) -> Variable {
        Variable { store_location }
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        exec.store.evaluate(self, exec)
    }
}

impl DisplayWithContextAndGraph for Variable {
    fn fmt(&self, f: &mut fmt::Formatter, _ctx: &Context, _graph: &Graph) -> fmt::Result {
        write!(f, "(load {})", self.store_location)
    }
}

/// Store holding thunks of lazy values
#[derive(Default)]
pub struct Store {
    elements: Vec<Thunk>,
}

impl Store {
    pub fn new() -> Store {
        Store {
            elements: Vec::new(),
        }
    }

    pub fn add(
        &mut self,
        value: Value,
        debug_info: DebugInfo,
        ctx: &Context,
        graph: &Graph,
    ) -> Variable {
        let store_location = self.elements.len();
        let variable = Variable::new(store_location);
        trace!(
            "store {} = {}",
            store_location,
            value.display_with(ctx, graph)
        );
        self.elements.push(Thunk::new(value, debug_info));
        variable
    }

    pub(super) fn evaluate(
        &self,
        variable: &Variable,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        let variable = &self.elements[variable.store_location];
        let debug_info = variable.debug_info;
        let value = variable.force(exec).with_context(|| {
            format!(
                "via {} at {}",
                (*variable.state.borrow()).display_with(exec.ctx, exec.graph),
                debug_info
            )
        })?;
        Ok(value)
    }
}

/// Data structure to hold scoped variables with lazy keys and values
pub struct ScopedVariables {
    variables: HashMap<Identifier, Cell<ScopedValues>>,
}

impl ScopedVariables {
    pub fn new() -> ScopedVariables {
        ScopedVariables {
            variables: HashMap::new(),
        }
    }

    pub fn add(
        &mut self,
        scope: Value,
        name: Identifier,
        value: Value,
        debug_info: DebugInfo,
        ctx: &Context,
    ) -> Result<(), ExecutionError> {
        let values = self
            .variables
            .entry(name)
            .or_insert_with(|| Cell::new(ScopedValues::new()));
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(mut pairs) => {
                pairs.push((scope, value, debug_info));
                values.replace(ScopedValues::Unforced(pairs));
                Ok(())
            }
            ScopedValues::Forcing => Err(ExecutionError::RecursivelyDefinedScopedVariable(
                format!("{}", name.display_with(ctx)),
            )),
            ScopedValues::Forced(map) => {
                values.replace(ScopedValues::Forced(map));
                Err(ExecutionError::VariableScopesAlreadyForced(format!(
                    "{}",
                    name.display_with(ctx)
                )))
            }
        }
    }

    pub(super) fn evaluate(
        &self,
        scope: SyntaxNodeRef,
        name: Identifier,
        exec: &mut EvaluationContext,
    ) -> Result<Value, ExecutionError> {
        let values = match self.variables.get(&name) {
            Some(v) => v,
            None => {
                return Err(ExecutionError::UndefinedScopedVariable(format!(
                    "{}.{}",
                    scope.display_with(exec.graph),
                    name.display_with(exec.ctx),
                )));
            }
        };
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(pairs) => {
                let mut map = HashMap::new();
                let mut debug_infos = HashMap::new();
                for (scope, value, debug_info) in pairs.into_iter() {
                    let node = scope.evaluate_as_syntax_node(exec)?;
                    let prev_debug_info = debug_infos.insert(node, debug_info.clone());
                    match map.insert(node, value.clone()) {
                        Some(_) => {
                            return Err(ExecutionError::DuplicateVariable(format!(
                                "{}.{} set at {} and {}",
                                node.display_with(exec.graph),
                                name.display_with(exec.ctx),
                                prev_debug_info.unwrap(),
                                debug_info,
                            )));
                        }
                        _ => {}
                    };
                }
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope.display_with(exec.graph),
                        name.display_with(exec.ctx),
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
            ScopedValues::Forcing => {
                Err(ExecutionError::RecursivelyDefinedScopedVariable(format!(
                    "_.{} requested on {}",
                    name.display_with(exec.ctx),
                    scope.display_with(exec.graph)
                )))
            }
            ScopedValues::Forced(map) => {
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope.display_with(exec.graph),
                        name.display_with(exec.ctx),
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
        }
    }
}

enum ScopedValues {
    Unforced(Vec<(Value, Value, DebugInfo)>),
    Forcing,
    Forced(HashMap<SyntaxNodeRef, Value>),
}

impl ScopedValues {
    fn new() -> ScopedValues {
        ScopedValues::Unforced(Vec::new())
    }
}

/// Thunk holding a lazy value or a forced graph value
struct Thunk {
    state: Rc<RefCell<ThunkState>>,
    debug_info: DebugInfo,
}

enum ThunkState {
    Unforced(Value),
    Forcing,
    Forced(graph::Value),
}

impl DisplayWithContextAndGraph for ThunkState {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        match self {
            Self::Unforced(value) => write!(f, "?{{{}}}", value.display_with(ctx, graph)),
            Self::Forcing => write!(f, "~{{?}}"),
            Self::Forced(value) => write!(f, "!{{{}}}", value.display_with(graph)),
        }
    }
}

impl Thunk {
    fn new(value: Value, debug_info: DebugInfo) -> Thunk {
        Thunk {
            state: Rc::new(RefCell::new(ThunkState::Unforced(value))),
            debug_info,
        }
    }

    fn force(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let state = self.state.replace(ThunkState::Forcing);
        trace!("force {}", state.display_with(exec.ctx, exec.graph));
        let value = match state {
            ThunkState::Unforced(value) => {
                // it is important that we do not hold a borrow of self.forced_values when executing self.value.evaluate
                let value = value.evaluate(exec)?;
                Ok(value)
            }
            ThunkState::Forced(value) => Ok(value),
            ThunkState::Forcing => Err(ExecutionError::RecursivelyDefinedVariable(format!(
                "{}",
                self.debug_info
            ))),
        }?;
        *self.state.borrow_mut() = ThunkState::Forced(value.clone());
        Ok(value)
    }
}

/// Debug info for tracking origins of values
#[derive(Debug, Clone, Copy)]
pub struct DebugInfo(Location);

impl From<Location> for DebugInfo {
    fn from(value: Location) -> Self {
        Self(value)
    }
}

impl fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
