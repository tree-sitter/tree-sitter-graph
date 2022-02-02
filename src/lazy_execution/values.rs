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
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::GraphNodeRef;
use crate::graph::SyntaxNodeRef;
use crate::graph::Value;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

use super::store::*;
use super::DisplayWithContextAndGraph;
use super::EvaluationContext;

/// Lazy values
#[derive(Clone, Debug)]
pub(super) enum LazyValue {
    Value(Value),
    List(LazyList),
    Set(LazySet),
    Variable(LazyVariable),
    ScopedVariable(LazyScopedVariable),
    Call(LazyCall),
}

impl From<Value> for LazyValue {
    fn from(value: Value) -> Self {
        LazyValue::Value(value)
    }
}

impl From<bool> for LazyValue {
    fn from(value: bool) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<u32> for LazyValue {
    fn from(value: u32) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<&str> for LazyValue {
    fn from(value: &str) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<String> for LazyValue {
    fn from(value: String) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<GraphNodeRef> for LazyValue {
    fn from(value: GraphNodeRef) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<SyntaxNodeRef> for LazyValue {
    fn from(value: SyntaxNodeRef) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<Vec<Value>> for LazyValue {
    fn from(value: Vec<Value>) -> Self {
        LazyValue::Value(value.into())
    }
}

impl From<LazyList> for LazyValue {
    fn from(value: LazyList) -> Self {
        LazyValue::List(value)
    }
}

impl From<Vec<LazyValue>> for LazyValue {
    fn from(value: Vec<LazyValue>) -> Self {
        LazyValue::List(LazyList::new(value))
    }
}

impl From<LazySet> for LazyValue {
    fn from(value: LazySet) -> Self {
        LazyValue::Set(value)
    }
}

impl From<LazyVariable> for LazyValue {
    fn from(value: LazyVariable) -> Self {
        LazyValue::Variable(value)
    }
}

impl From<LazyScopedVariable> for LazyValue {
    fn from(value: LazyScopedVariable) -> Self {
        LazyValue::ScopedVariable(value)
    }
}

impl From<LazyCall> for LazyValue {
    fn from(value: LazyCall) -> Self {
        LazyValue::Call(value)
    }
}

impl LazyValue {
    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<Value, ExecutionError> {
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
            Value::GraphNode(node) => Ok(node),
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
            Value::SyntaxNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedSyntaxNode(format!(
                " got {}",
                node.display_with(exec.graph)
            ))),
        }
    }
}

impl DisplayWithContextAndGraph for LazyValue {
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
pub(super) struct LazyScopedVariable {
    scope: Box<LazyValue>,
    name: Identifier,
}

impl LazyScopedVariable {
    pub(super) fn new(scope: LazyValue, name: Identifier) -> Self {
        Self {
            scope: scope.into(),
            name,
        }
    }

    fn resolve<'a>(&self, exec: &'a mut EvaluationContext) -> Result<LazyValue, ExecutionError> {
        let scope = self.scope.as_ref().evaluate_as_syntax_node(exec)?;
        let scoped_store = &exec.scoped_store;
        scoped_store.evaluate(scope, self.name, exec)
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<Value, ExecutionError> {
        let value = self.resolve(exec)?;
        value.evaluate(exec)
    }
}

impl DisplayWithContextAndGraph for LazyScopedVariable {
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
pub(super) struct LazyList {
    elements: Vec<LazyValue>,
}

impl LazyList {
    pub(super) fn new(elements: Vec<LazyValue>) -> Self {
        Self { elements }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::List(elements))
    }
}

impl DisplayWithContextAndGraph for LazyList {
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
pub(super) struct LazySet {
    elements: Vec<LazyValue>,
}

impl LazySet {
    pub(super) fn new(elements: Vec<LazyValue>) -> Self {
        Self { elements }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::Set(elements))
    }
}

impl DisplayWithContextAndGraph for LazySet {
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
pub(super) struct LazyCall {
    function: Identifier,
    arguments: Vec<LazyValue>,
}

impl LazyCall {
    pub(super) fn new(function: Identifier, arguments: Vec<LazyValue>) -> Self {
        Self {
            function,
            arguments,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<Value, ExecutionError> {
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

impl DisplayWithContextAndGraph for LazyCall {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "(call '{}", self.function.display_with(ctx))?;
        for arg in &self.arguments {
            write!(f, " {}", arg.display_with(ctx, graph))?;
        }
        write!(f, ")")
    }
}
