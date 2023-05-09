// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines store and thunks for lazy DSL evaluation

use log::trace;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::execution::error::Context;
use crate::execution::error::ExecutionError;
use crate::execution::error::ResultWithExecutionError;
use crate::execution::error::StatementContext;
use crate::graph;
use crate::graph::SyntaxNodeRef;
use crate::Identifier;

use super::values::*;
use super::EvaluationContext;

/// Variable that points to a thunk in the store
#[derive(Clone, Debug)]
pub(super) struct LazyVariable {
    store_location: usize,
}

impl LazyVariable {
    fn new(store_location: usize) -> Self {
        Self { store_location }
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        exec.store.evaluate(self, exec)
    }
}

impl fmt::Display for LazyVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(load {})", self.store_location)
    }
}

/// Store holding thunks of lazy values
#[derive(Default)]
pub(super) struct LazyStore {
    elements: Vec<Thunk>,
}

impl LazyStore {
    pub(super) fn new() -> Self {
        Self {
            elements: Vec::new(),
        }
    }

    pub(super) fn add(&mut self, value: LazyValue, debug_info: DebugInfo) -> LazyVariable {
        let store_location = self.elements.len();
        let variable = LazyVariable::new(store_location);
        trace!("store {} = {}", store_location, value);
        self.elements.push(Thunk::new(value, debug_info));
        variable
    }

    pub(super) fn evaluate(
        &self,
        variable: &LazyVariable,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        let variable = &self.elements[variable.store_location];
        let debug_info = variable.debug_info.clone();
        let value = variable.force(exec).with_context(|| debug_info.0.into())?;
        Ok(value)
    }

    pub(super) fn evaluate_all(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        for variable in &self.elements {
            let debug_info = variable.debug_info.clone();
            variable.force(exec).with_context(|| debug_info.0.into())?;
        }
        Ok(())
    }
}

/// Data structure to hold scoped variables with lazy keys and values
pub(super) struct LazyScopedVariables {
    variables: HashMap<Identifier, Cell<ScopedValues>>,
}

impl LazyScopedVariables {
    pub(super) fn new() -> Self {
        LazyScopedVariables {
            variables: HashMap::new(),
        }
    }

    pub(super) fn add(
        &mut self,
        scope: LazyValue,
        name: Identifier,
        value: LazyValue,
        debug_info: DebugInfo,
    ) -> Result<(), ExecutionError> {
        let values = self
            .variables
            .entry(name.clone())
            .or_insert_with(|| Cell::new(ScopedValues::new()));
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(mut pairs) => {
                pairs.push((scope, value, debug_info));
                values.replace(ScopedValues::Unforced(pairs));
                Ok(())
            }
            ScopedValues::Forcing => Err(ExecutionError::RecursivelyDefinedScopedVariable(
                format!("{}", name),
            )),
            ScopedValues::Forced(map) => {
                values.replace(ScopedValues::Forced(map));
                Err(ExecutionError::VariableScopesAlreadyForced(format!(
                    "{}",
                    name
                )))
            }
        }
    }

    pub(super) fn evaluate(
        &self,
        scope: &SyntaxNodeRef,
        name: &Identifier,
        exec: &mut EvaluationContext,
    ) -> Result<LazyValue, ExecutionError> {
        let values = match self.variables.get(name) {
            Some(v) => v,
            None => {
                return Err(ExecutionError::UndefinedScopedVariable(format!(
                    "{}.{}",
                    scope, name,
                )));
            }
        };
        match values.replace(ScopedValues::Forcing) {
            ScopedValues::Unforced(pairs) => {
                let mut map = HashMap::new();
                let mut debug_infos = HashMap::new();
                for (scope, value, debug_info) in pairs.into_iter() {
                    let node = scope
                        .evaluate_as_syntax_node(exec)
                        .with_context(|| format!("Evaluating scope of variable _.{}", name,).into())
                        .with_context(|| debug_info.0.clone().into())?;
                    let prev_debug_info = debug_infos.insert(node, debug_info.clone());
                    match map.insert(node, value.clone()) {
                        Some(_) => {
                            return Err(ExecutionError::DuplicateVariable(format!(
                                "{}.{}",
                                node, name,
                            )))
                            .with_context(|| (prev_debug_info.unwrap().0, debug_info.0).into());
                        }
                        _ => {}
                    };
                }
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope, name,
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
            ScopedValues::Forcing => Err(ExecutionError::RecursivelyDefinedScopedVariable(
                format!("_.{} requested on {}", name, scope),
            )),
            ScopedValues::Forced(map) => {
                let result = map
                    .get(&scope)
                    .ok_or(ExecutionError::UndefinedScopedVariable(format!(
                        "{}.{}",
                        scope, name,
                    )))?
                    .clone();
                values.replace(ScopedValues::Forced(map));
                Ok(result)
            }
        }
    }
}

enum ScopedValues {
    Unforced(Vec<(LazyValue, LazyValue, DebugInfo)>),
    Forcing,
    Forced(HashMap<SyntaxNodeRef, LazyValue>),
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
    Unforced(LazyValue),
    Forcing,
    Forced(graph::Value),
}

impl fmt::Display for ThunkState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unforced(value) => write!(f, "?{{{}}}", value),
            Self::Forcing => write!(f, "~{{?}}"),
            Self::Forced(value) => write!(f, "!{{{}}}", value),
        }
    }
}

impl Thunk {
    fn new(value: LazyValue, debug_info: DebugInfo) -> Thunk {
        Thunk {
            state: Rc::new(RefCell::new(ThunkState::Unforced(value))),
            debug_info,
        }
    }

    fn force(&self, exec: &mut EvaluationContext) -> Result<graph::Value, ExecutionError> {
        let state = self.state.replace(ThunkState::Forcing);
        trace!("force {}", state);
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
#[derive(Debug, Clone)]
pub(super) struct DebugInfo(StatementContext);

impl From<StatementContext> for DebugInfo {
    fn from(value: StatementContext) -> Self {
        Self(value)
    }
}

impl From<DebugInfo> for StatementContext {
    fn from(value: DebugInfo) -> Self {
        value.0
    }
}

impl From<DebugInfo> for Context {
    fn from(value: DebugInfo) -> Self {
        value.0.into()
    }
}

impl fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.statement_location)
    }
}
