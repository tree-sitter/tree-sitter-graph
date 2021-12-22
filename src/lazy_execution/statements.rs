// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Defines graph statements for lazy DSL evaluation

use log::{debug, trace};

use std::convert::From;
use std::fmt;

use crate::execution::ExecutionError;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::Context;
use crate::DisplayWithContext as _;
use crate::Identifier;

use super::store::DebugInfo;
use super::values::*;
use super::DisplayWithContextAndGraph;
use super::EvaluationContext;
use super::GraphElementKey;

/// Lazy graph statements
#[derive(Debug)]
pub enum Statement {
    AddGraphNodeAttribute(AddGraphNodeAttribute),
    CreateEdge(CreateEdge),
    AddEdgeAttribute(AddEdgeAttribute),
}

impl Statement {
    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        debug!("eval {}", self.display_with(exec.ctx, exec.graph));
        trace!("{{");
        let result = match self {
            Statement::AddGraphNodeAttribute(stmt) => stmt.evaluate(exec),
            Statement::CreateEdge(stmt) => stmt.evaluate(exec),
            Statement::AddEdgeAttribute(stmt) => stmt.evaluate(exec),
        };
        trace!("}}");
        result
    }
}

impl From<AddEdgeAttribute> for Statement {
    fn from(stmt: AddEdgeAttribute) -> Self {
        Statement::AddEdgeAttribute(stmt)
    }
}

impl From<AddGraphNodeAttribute> for Statement {
    fn from(stmt: AddGraphNodeAttribute) -> Self {
        Statement::AddGraphNodeAttribute(stmt)
    }
}

impl From<CreateEdge> for Statement {
    fn from(stmt: CreateEdge) -> Self {
        Statement::CreateEdge(stmt)
    }
}

impl DisplayWithContextAndGraph for Statement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        match self {
            Statement::AddGraphNodeAttribute(stmt) => stmt.fmt(f, ctx, graph),
            Statement::CreateEdge(stmt) => stmt.fmt(f, ctx, graph),
            Statement::AddEdgeAttribute(stmt) => stmt.fmt(f, ctx, graph),
        }
    }
}

/// Lazy statement to add graph node attributes
#[derive(Debug)]
pub struct AddGraphNodeAttribute {
    node: Value,
    attributes: Vec<Attribute>,
    debug_info: DebugInfo,
}

impl AddGraphNodeAttribute {
    pub fn new(
        node: Value,
        attributes: Vec<Attribute>,
        debug_info: DebugInfo,
    ) -> AddGraphNodeAttribute {
        AddGraphNodeAttribute {
            node,
            attributes,
            debug_info,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate_as_graph_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            let prev_debug_info = exec.prev_element_debug_info.insert(
                GraphElementKey::NodeAttribute(node, attribute.name),
                self.debug_info,
            );
            exec.graph[node]
                .attributes
                .add(attribute.name, value)
                .map_err(|_| {
                    ExecutionError::DuplicateAttribute(format!(
                        "{} on {} at {} and {}",
                        attribute.name.display_with(exec.ctx),
                        node.display_with(exec.graph),
                        prev_debug_info.unwrap(),
                        self.debug_info,
                    ))
                })?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for AddGraphNodeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(f, "attr ({})", self.node.display_with(ctx, graph))?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx, graph))?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

/// Lazy statement to create a graph edge
#[derive(Debug)]
pub struct CreateEdge {
    source: Value,
    sink: Value,
    debug_info: DebugInfo,
}

impl CreateEdge {
    pub fn new(source: Value, sink: Value, debug_info: DebugInfo) -> CreateEdge {
        CreateEdge {
            source,
            sink,
            debug_info,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_graph_node(exec)?;
        let sink = self.sink.evaluate_as_graph_node(exec)?;
        let prev_debug_info = exec
            .prev_element_debug_info
            .insert(GraphElementKey::Edge(source, sink), self.debug_info);
        if let Err(_) = exec.graph[source].add_edge(sink) {
            Err(ExecutionError::DuplicateEdge(format!(
                "({} -> {}) at {} and {}",
                source.display_with(exec.graph),
                sink.display_with(exec.graph),
                prev_debug_info.unwrap(),
                self.debug_info,
            )))?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for CreateEdge {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "edge {} -> {} at {}",
            self.source.display_with(ctx, graph),
            self.sink.display_with(ctx, graph),
            self.debug_info,
        )
    }
}

/// Lazy statement to add graph edge attributes
#[derive(Debug)]
pub struct AddEdgeAttribute {
    source: Value,
    sink: Value,
    attributes: Vec<Attribute>,
    debug_info: DebugInfo,
}

impl AddEdgeAttribute {
    pub fn new(
        source: Value,
        sink: Value,
        attributes: Vec<Attribute>,
        debug_info: DebugInfo,
    ) -> AddEdgeAttribute {
        AddEdgeAttribute {
            source,
            sink,
            attributes,
            debug_info,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_graph_node(exec)?;
        let sink = self.sink.evaluate_as_graph_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            let edge = match exec.graph[source].get_edge_mut(sink) {
                Some(edge) => Ok(edge),
                None => Err(ExecutionError::UndefinedEdge(format!(
                    "({} -> {}) at {}",
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    self.debug_info,
                ))),
            }?;
            let prev_debug_info = exec.prev_element_debug_info.insert(
                GraphElementKey::EdgeAttribute(source, sink, attribute.name),
                self.debug_info,
            );
            edge.attributes.add(attribute.name, value).map_err(|_| {
                ExecutionError::DuplicateAttribute(format!(
                    "{} on edge ({} -> {}) at {} and {}",
                    attribute.name.display_with(exec.ctx),
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    prev_debug_info.unwrap(),
                    self.debug_info,
                ))
            })?;
        }
        Ok(())
    }
}

impl DisplayWithContextAndGraph for AddEdgeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "attr ({} -> {})",
            self.source.display_with(ctx, graph),
            self.sink.display_with(ctx, graph),
        )?;
        for attr in &self.attributes {
            write!(f, " {}", attr.display_with(ctx, graph),)?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

/// Lazy attribute
#[derive(Debug)]
pub struct Attribute {
    name: Identifier,
    value: Value,
}

impl Attribute {
    pub fn new(name: Identifier, value: Value) -> Attribute {
        Attribute { name, value }
    }
}

impl DisplayWithContextAndGraph for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        write!(
            f,
            "{} = {}",
            self.name.display_with(ctx),
            self.value.display_with(ctx, graph),
        )
    }
}
