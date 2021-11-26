// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use log::trace;

use std::collections::BTreeMap;

use crate::execution::ExecutionError;
use crate::execution::Variables as StrictVariables;
use crate::graph as g;
use crate::Context;
use crate::DisplayWithContext as _;
use crate::Identifier;

use super::values::BranchValue;
use super::values::CurrentLoopListElementValue;
use super::values::DisplayWithContextAndGraph as _;
use super::values::EnterLoopValue;
use super::values::LoopValue;
use super::values::PreviousLoopValue;
use super::values::Store;
use super::values::Value;
use super::values::Variable;

macro_rules! debug_assert_loop_depth {
    ($depth:expr, $value:expr $(,)?) => {
        if cfg!(debug_assertions) {
            $crate::execution::lazy::values::AssertLoopDepth::new($depth, $value).into()
        } else {
            $value
        }
    };
}

pub struct VariableContext<'a, 'tree> {
    pub ctx: &'a Context,
    pub graph: &'a g::Graph<'tree>,
    pub store: &'a mut Store,
}

/// An environment of named variables
pub trait Variables {
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutability: Mutability,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError>;

    fn set(
        &mut self,
        name: Identifier,
        value: Value,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError>;

    fn has(&self, name: Identifier) -> bool;

    fn get(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&Value, ExecutionError>;

    fn loop_depth(&self) -> usize;
}

pub enum Mutability {
    Immutable,
    Mutable,
}

/// An environment that wraps strict variables
pub struct ConcreteVariables<'a> {
    parent: &'a StrictVariables,
    values: Vec<NamedLazyVariable>,
}

impl ConcreteVariables<'_> {
    /// Creates a new, empty environment of variables.
    pub fn new<'a>(parent: &'a StrictVariables) -> ConcreteVariables<'a> {
        ConcreteVariables {
            parent,
            values: Vec::default(),
        }
    }
}

impl Variables for ConcreteVariables<'_> {
    fn add(
        &mut self,
        name: Identifier,
        _value: Value,
        _mutability: Mutability,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        Err(ExecutionError::CannotAssignImmutableVariable(format!(
            "{} in strict environment",
            name.display_with(exec.ctx)
        )))
    }

    fn set(
        &mut self,
        name: Identifier,
        _value: Value,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        Err(ExecutionError::CannotAssignImmutableVariable(format!(
            "{} in strict environment",
            name.display_with(exec.ctx)
        )))
    }

    fn get(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&Value, ExecutionError> {
        let index = match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => index,
            Err(index) => {
                let parent_value = self.parent.get(name).ok_or_else(|| {
                    ExecutionError::UndefinedVariable(format!("{}", name.display_with(exec.ctx)))
                })?;
                let variable = NamedLazyVariable {
                    name: name,
                    value: parent_value.clone().into(),
                    mutability: Mutability::Immutable,
                };
                self.values.insert(index, variable);
                index
            }
        };
        let value = &self.values[index].value;
        trace!(
            "get-concrete {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        Ok(value)
    }

    fn has(&self, name: Identifier) -> bool {
        self.parent.get(name).is_some()
    }

    fn loop_depth(&self) -> usize {
        0
    }
}

/// An environment of named lazy variables
#[derive(Default)]
pub struct LazyVariables<'a> {
    parent: Option<&'a mut dyn Variables>,
    values: Vec<NamedLazyVariable>,
}

struct NamedLazyVariable {
    name: Identifier,
    value: Value,
    mutability: Mutability,
}

impl NamedLazyVariable {
    fn set(&mut self, value: Value, ctx: &Context) -> Result<(), ExecutionError> {
        if let Mutability::Immutable = self.mutability {
            return Err(ExecutionError::CannotAssignImmutableVariable(format!(
                "{}",
                self.name.display_with(ctx)
            )));
        }
        self.value = value;
        Ok(())
    }
}

impl<'a> LazyVariables<'a> {
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

impl Variables for LazyVariables<'_> {
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutability: Mutability,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(_) => Err(ExecutionError::DuplicateVariable(format!(
                "{}",
                name.display_with(exec.ctx),
            ))),
            Err(index) => {
                if false && self.parent.as_ref().map(|p| p.has(name)).unwrap_or(false) {
                    // FIXME disabled because this breaks nested scan, which shadows $
                    Err(ExecutionError::DuplicateVariable(format!(
                        "{}",
                        name.display_with(exec.ctx),
                    )))?;
                }
                let variable = NamedLazyVariable {
                    name,
                    value,
                    mutability,
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
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        trace!(
            "set-lazy {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => {
                let value = debug_assert_loop_depth!(self.loop_depth(), value);
                let variable = &mut self.values[index];
                variable.set(value.into(), exec.ctx)
            }
            Err(_) => self
                .parent
                .as_mut()
                .map(|parent| parent.set(name, value, exec))
                .unwrap_or_else(|| {
                    Err(ExecutionError::UndefinedVariable(format!(
                        "{}",
                        name.display_with(exec.ctx)
                    )))
                }),
        }
    }

    fn get(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&Value, ExecutionError> {
        let value = match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(index) => Ok(&self.values[index].value),
            Err(_) => {
                let parent = self
                    .parent
                    .as_mut()
                    .ok_or(ExecutionError::UndefinedVariable(format!(
                        "{}",
                        name.display_with(exec.ctx)
                    )))?;
                parent.get(name, exec)
            }
        }?;
        trace!(
            "get-lazy {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        Ok(value)
    }

    fn has(&self, name: Identifier) -> bool {
        return self.values.binary_search_by_key(&name, |v| v.name).is_ok()
            || self.parent.as_ref().map(|p| p.has(name)).unwrap_or(false);
    }

    fn loop_depth(&self) -> usize {
        0
    }
}

/// An branched environment of named lazy variables
pub struct BranchVariables<'a> {
    parent: &'a mut dyn Variables,
    current_branch_index: usize,
    local_variables: LazyVariables<'a>,
    inherited_variables: Vec<NamedBranchVariable>,
    selected_branch_index: Value,
}

struct NamedBranchVariable {
    name: Identifier,
    initial_value: Value,
    branch_values: BTreeMap<usize, Value>,
}

impl NamedBranchVariable {
    fn set(&mut self, branch_index: usize, value: Value) {
        self.branch_values.insert(branch_index, value);
    }

    fn get(&self, branch_index: usize) -> &Value {
        self.branch_values
            .get(&branch_index)
            .unwrap_or(&self.initial_value)
    }
}

impl<'a> BranchVariables<'a> {
    /// Creates a new, empty environment of variables.
    pub fn new(parent: &'a mut dyn Variables, selected_branch_index: Value) -> Self {
        Self {
            parent: parent,
            current_branch_index: 0,
            local_variables: LazyVariables::default(),
            inherited_variables: Vec::new(),
            selected_branch_index,
        }
    }

    // FIXME This aliases immutable variables as well, which could be used directly.
    //       We could do this only for mutable variables, if we know whether a variable
    //       in the parent is mutable or not.
    fn get_inherited(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&mut NamedBranchVariable, ExecutionError> {
        let index = match self
            .inherited_variables
            .binary_search_by_key(&name, |v| v.name)
        {
            Ok(index) => index,
            Err(index) => {
                let parent_value = self.parent.get(name, exec)?;
                let variable = NamedBranchVariable {
                    name: name,
                    initial_value: parent_value.clone(),
                    branch_values: BTreeMap::new(),
                };
                self.inherited_variables.insert(index, variable);
                index
            }
        };
        Ok(&mut self.inherited_variables[index])
    }

    /// Moves context to the next branch.
    pub fn next(&mut self) {
        self.current_branch_index += 1;
        self.local_variables.clear();
    }
}

impl Variables for BranchVariables<'_> {
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutability: Mutability,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        if false && self.parent.get(name, exec).is_ok() {
            // FIXME disabled because this breaks nested scan, which shadows $
            Err(ExecutionError::DuplicateVariable(format!(
                "{}",
                name.display_with(exec.ctx),
            )))?;
        }
        self.local_variables.add(name, value, mutability, exec)
    }

    fn set(
        &mut self,
        name: Identifier,
        value: Value,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        trace!(
            "set-branch {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        if self.local_variables.get(name, exec).is_ok() {
            self.local_variables.set(name, value, exec)
        } else {
            let current_branch_index = self.current_branch_index;
            let selected_branch_index = self.selected_branch_index.clone();

            let value = debug_assert_loop_depth!(self.loop_depth(), value);
            let variable = self.get_inherited(name, exec)?;
            variable.set(current_branch_index, value.into());

            let final_value = BranchValue::new(
                variable.initial_value.clone(),
                selected_branch_index,
                variable.branch_values.clone(),
            );
            self.parent.set(name, final_value.into(), exec)
        }
    }

    fn get(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&Value, ExecutionError> {
        let value = if self.local_variables.get(name, exec).is_ok() {
            self.local_variables.get(name, exec)
        } else {
            let current_branch_index = self.current_branch_index;

            let variable = self.get_inherited(name, exec)?;
            Ok(variable.get(current_branch_index))
        }?;
        trace!(
            "get-branch {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        Ok(value)
    }

    fn has(&self, name: Identifier) -> bool {
        return self.local_variables.has(name) || self.parent.has(name);
    }

    fn loop_depth(&self) -> usize {
        self.parent.loop_depth()
    }
}

/// An iteration environment of named lazy variables
pub struct LoopVariables<'a> {
    parent: &'a mut dyn Variables,
    local_variables: LazyVariables<'a>,
    inherited_variables: Vec<NamedLoopVariable>,
    iteration_values: Value, // scoped outside the loop
    iteration_value: Value,  // scoped inside the loop
}

struct NamedLoopVariable {
    name: Identifier,
    // variable which is updated by assignments inside the loop and ultimately captures the final assignment
    loop_variable: Variable,
    // the loop value that handles base case of iteration
    loop_value: Value,
    // the most recent assignment in the loop
    value: Value,
}

impl NamedLoopVariable {
    fn set(&mut self, value: Value) {
        self.value = value.clone();
    }

    fn get(&self) -> &Value {
        &self.value
    }
}

impl<'a> LoopVariables<'a> {
    /// Creates a new, empty environment of variables.
    pub fn new(parent: &'a mut dyn Variables, iteration_values: Value) -> Self {
        let iteration_values = debug_assert_loop_depth!(parent.loop_depth(), iteration_values);
        let iteration_value = debug_assert_loop_depth!(
            parent.loop_depth() + 1,
            CurrentLoopListElementValue::new(iteration_values.clone().into()).into(),
        );
        Self {
            parent: parent,
            local_variables: LazyVariables::default(),
            inherited_variables: Vec::new(),
            iteration_values: iteration_values.into(),
            iteration_value: iteration_value,
        }
    }

    // FIXME This aliases immutable variables as well, which could be used directly.
    //       We could do this only for mutable variables, if we know whether a variable
    //       in the parent is mutable or not.
    fn get_inherited(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&mut NamedLoopVariable, ExecutionError> {
        let index = match self
            .inherited_variables
            .binary_search_by_key(&name, |v| v.name)
        {
            Ok(index) => index,
            Err(index) => {
                let parent_value = self.parent.get(name, exec)?;
                let loop_variable =
                    exec.store
                        .add(g::Value::Null.into(), "".into(), exec.ctx, exec.graph);
                let loop_value: Value =
                    LoopValue::new(parent_value.clone(), loop_variable.clone().into()).into();
                let value: Value = PreviousLoopValue::new(loop_value.clone()).into();
                exec.store.set(
                    &loop_variable,
                    value.clone(),
                    "".into(),
                    exec.ctx,
                    exec.graph,
                );

                let variable = NamedLoopVariable {
                    name: name,
                    loop_variable: loop_variable,
                    loop_value: loop_value,
                    value: value,
                };
                self.inherited_variables.insert(index, variable);
                index
            }
        };
        Ok(&mut self.inherited_variables[index])
    }

    pub fn iteration_value(&self) -> &Value {
        &self.iteration_value
    }
}

impl Variables for LoopVariables<'_> {
    fn add(
        &mut self,
        name: Identifier,
        value: Value,
        mutability: Mutability,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        if false && self.parent.get(name, exec).is_ok() {
            // FIXME disabled because this breaks nested scan, which shadows $
            Err(ExecutionError::DuplicateVariable(format!(
                "{}",
                name.display_with(exec.ctx),
            )))?;
        }
        self.local_variables.add(name, value, mutability, exec)
    }

    fn set(
        &mut self,
        name: Identifier,
        value: Value,
        exec: &mut VariableContext,
    ) -> Result<(), ExecutionError> {
        trace!(
            "set-loop {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        if self.local_variables.get(name, exec).is_ok() {
            self.local_variables.set(name, value, exec)
        } else {
            let iteration_values = self.iteration_values.clone();

            let value: Value = debug_assert_loop_depth!(self.loop_depth(), value).into();
            let variable = self.get_inherited(name, exec)?;
            exec.store.set(
                &variable.loop_variable,
                value.clone(),
                "".into(),
                exec.ctx,
                exec.graph,
            );
            variable.set(value);

            let final_value =
                EnterLoopValue::new(iteration_values, variable.loop_value.clone().into());
            self.parent.set(name, final_value.into(), exec)?;

            Ok(())
        }
    }

    fn get(
        &mut self,
        name: Identifier,
        exec: &mut VariableContext,
    ) -> Result<&Value, ExecutionError> {
        let value = if self.local_variables.get(name, exec).is_ok() {
            self.local_variables.get(name, exec)
        } else {
            let variable = self.get_inherited(name, exec)?;
            Ok(variable.get())
        }?;
        trace!(
            "get-loop {} = {}",
            name.display_with(exec.ctx),
            value.display_with(exec.ctx, exec.graph)
        );
        Ok(value)
    }

    fn has(&self, name: Identifier) -> bool {
        return self.local_variables.has(name) || self.parent.has(name);
    }

    fn loop_depth(&self) -> usize {
        self.parent.loop_depth() + 1
    }
}
