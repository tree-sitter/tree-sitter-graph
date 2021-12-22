// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines values for lazy DSL evaluation

use log::trace;

use std::convert::From;
use std::fmt;

use crate::execution::ExecutionError;
use crate::graph;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::GraphNodeRef;
use crate::graph::SyntaxNodeRef;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use super::store::*;
use super::DisplayWithContextAndGraph;
use super::EvaluationContext;

/// Lazy values
#[derive(Clone, Debug)]
pub enum Value {
    Value(graph::Value),
    List(List),
    Set(Set),
    Variable(Variable),
    ScopedVariable(ScopedVariable),
    Call(Call),
}

impl From<graph::Value> for Value {
    fn from(value: graph::Value) -> Self {
        Value::Value(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Value(value.into())
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Value(value.into())
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Value(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Value(value.into())
    }
}

impl From<GraphNodeRef> for Value {
    fn from(value: GraphNodeRef) -> Self {
        Value::Value(value.into())
    }
}

impl From<SyntaxNodeRef> for Value {
    fn from(value: SyntaxNodeRef) -> Self {
        Value::Value(value.into())
    }
}

impl From<Vec<graph::Value>> for Value {
    fn from(value: Vec<graph::Value>) -> Self {
        Value::Value(value.into())
    }
}

impl From<List> for Value {
    fn from(value: List) -> Self {
        Value::List(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::List(List::new(value))
    }
}

impl From<Set> for Value {
    fn from(value: Set) -> Self {
        Value::Set(value)
    }
}

impl From<Variable> for Value {
    fn from(value: Variable) -> Self {
        Value::Variable(value)
    }
}

impl From<ScopedVariable> for Value {
    fn from(value: ScopedVariable) -> Self {
        Value::ScopedVariable(value)
    }
}

impl From<Call> for Value {
    fn from(value: Call) -> Self {
        Value::Call(value)
    }
}

impl Value {
    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        trace!("eval {} {{", self.display_with(exec.ctx, exec.graph));
        let ret = match self {
            Self::Value(value) => Ok(value.clone()),
            Self::List(expr) => expr.evaluate(exec),
            Self::Set(expr) => expr.evaluate(exec),
            Self::Variable(expr) => expr.evaluate(exec),
            Self::ScopedVariable(expr) => expr.evaluate(exec),
            Self::Call(expr) => expr.evaluate(exec),
        }?;
        trace!("}} = {}", ret.display_with(exec.graph));
        Ok(ret)
    }

    pub(super) fn evaluate_as_graph_node(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<GraphNodeRef, ExecutionError> {
        let node = self.evaluate(exec)?;
        match node {
            graph::Value::GraphNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedGraphNode(format!(
                " got {}",
                node.display_with(exec.graph)
            ))),
        }
    }

    pub(super) fn evaluate_as_syntax_node(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<SyntaxNodeRef, ExecutionError> {
        let node = self.evaluate(exec)?;
        match node {
            graph::Value::SyntaxNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedSyntaxNode(format!(
                " got {}",
                node.display_with(exec.graph)
            ))),
        }
    }
}

impl DisplayWithContextAndGraph for Value {
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{}", value.display_with(graph)),
            Self::List(expr) => expr.fmt(f, ctx, graph),
            Self::Set(expr) => expr.fmt(f, ctx, graph),
            Self::Variable(expr) => expr.fmt(f, ctx, graph),
            Self::ScopedVariable(expr) => expr.fmt(f, ctx, graph),
            Self::Call(expr) => expr.fmt(f, ctx, graph),
        }
    }
}

/// Lazy scoped variable
#[derive(Clone, Debug)]
pub struct ScopedVariable {
    scope: Box<Value>,
    name: Identifier,
}

impl ScopedVariable {
    pub fn new(scope: Value, name: Identifier) -> ScopedVariable {
        ScopedVariable {
            scope: scope.into(),
            name,
        }
    }

    fn resolve<'a>(&self, exec: &'a mut EvaluationContext) -> Result<Value, ExecutionError> {
        let scope = self.scope.as_ref().evaluate_as_syntax_node(exec)?;
        let scoped_store = &exec.scoped_store;
        scoped_store.evaluate(scope, self.name, exec)
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        let value = self.resolve(exec)?;
        value.evaluate(exec)
    }
}

impl DisplayWithContextAndGraph for ScopedVariable {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "(scoped {} '{})",
            self.scope.display_with(ctx, graph),
            self.name.display_with(ctx),
        )
    }
}

/// Lazy list literal
#[derive(Clone, Debug)]
pub struct List {
    elements: Vec<Value>,
}

impl List {
    pub fn new(elements: Vec<Value>) -> List {
        List { elements }
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(graph::Value::List(elements))
    }
}

impl DisplayWithContextAndGraph for List {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(list")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                first = false;
                write!(f, "{}", elem.display_with(ctx, graph))?;
            } else {
                write!(f, " {}", elem.display_with(ctx, graph))?;
            }
        }
        write!(f, ")")
    }
}

/// Lazy set literal
#[derive(Clone, Debug)]
pub struct Set {
    elements: Vec<Value>,
}

impl Set {
    pub fn new(elements: Vec<Value>) -> Set {
        Set { elements }
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(graph::Value::Set(elements))
    }
}

impl DisplayWithContextAndGraph for Set {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(set")?;
        let mut first = true;
        for elem in &self.elements {
            if first {
                first = false;
                write!(f, "{}", elem.display_with(ctx, graph))?;
            } else {
                write!(f, " {}", elem.display_with(ctx, graph))?;
            }
        }
        write!(f, ")")
    }
}

/// Lazy function call
#[derive(Clone, Debug)]
pub struct Call {
    function: Identifier,
    arguments: Vec<Value>,
}

impl Call {
    pub fn new(function: Identifier, arguments: Vec<Value>) -> Call {
        Call {
            function,
            arguments,
        }
    }

    pub(super) fn evaluate(
        &self,
        exec: &mut EvaluationContext,
    ) -> Result<graph::Value, ExecutionError> {
        for argument in &self.arguments {
            let argument = argument.evaluate(exec)?;
            exec.function_parameters.push(argument);
        }

        exec.functions.call(
            exec.ctx,
            self.function,
            exec.graph,
            exec.source,
            &mut exec
                .function_parameters
                .drain(exec.function_parameters.len() - self.arguments.len()..),
        )
    }
}

impl DisplayWithContextAndGraph for Call {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(call '{}", self.function.display_with(ctx))?;
        for arg in &self.arguments {
            write!(f, " {}", arg.display_with(ctx, graph))?;
        }
        write!(f, ")")
    }
}
