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

use crate::execution::error::ExecutionError;
use crate::execution::error::ResultWithExecutionError;
use crate::graph::Attributes;
use crate::Identifier;

use super::store::DebugInfo;
use super::values::*;
use super::EvaluationContext;
use super::GraphElementKey;

/// Lazy graph statements
#[derive(Debug)]
pub(super) enum LazyStatement {
    AddGraphNodeAttribute(LazyAddGraphNodeAttribute),
    CreateEdge(LazyCreateEdge),
    AddEdgeAttribute(LazyAddEdgeAttribute),
    Print(LazyPrint),
}

impl LazyStatement {
    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        exec.cancellation_flag.check("evaluating statement")?;
        debug!("eval {}", self);
        trace!("{{");
        let result = match self {
            Self::AddGraphNodeAttribute(stmt) => stmt
                .evaluate(exec)
                .with_context(|| stmt.debug_info.clone().into()),
            Self::CreateEdge(stmt) => stmt
                .evaluate(exec)
                .with_context(|| stmt.debug_info.clone().into()),
            Self::AddEdgeAttribute(stmt) => stmt
                .evaluate(exec)
                .with_context(|| stmt.debug_info.clone().into()),
            Self::Print(stmt) => stmt
                .evaluate(exec)
                .with_context(|| stmt.debug_info.clone().into()),
        };
        trace!("}}");
        result
    }
}

impl From<LazyAddEdgeAttribute> for LazyStatement {
    fn from(stmt: LazyAddEdgeAttribute) -> Self {
        Self::AddEdgeAttribute(stmt)
    }
}

impl From<LazyAddGraphNodeAttribute> for LazyStatement {
    fn from(stmt: LazyAddGraphNodeAttribute) -> Self {
        Self::AddGraphNodeAttribute(stmt)
    }
}

impl From<LazyCreateEdge> for LazyStatement {
    fn from(stmt: LazyCreateEdge) -> Self {
        Self::CreateEdge(stmt)
    }
}

impl From<LazyPrint> for LazyStatement {
    fn from(stmt: LazyPrint) -> Self {
        Self::Print(stmt)
    }
}

impl fmt::Display for LazyStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AddGraphNodeAttribute(stmt) => stmt.fmt(f),
            Self::CreateEdge(stmt) => stmt.fmt(f),
            Self::AddEdgeAttribute(stmt) => stmt.fmt(f),
            Self::Print(stmt) => stmt.fmt(f),
        }
    }
}

/// Lazy statement to add graph node attributes
#[derive(Debug)]
pub(super) struct LazyAddGraphNodeAttribute {
    node: LazyValue,
    attributes: Vec<LazyAttribute>,
    debug_info: DebugInfo,
}

impl LazyAddGraphNodeAttribute {
    pub(super) fn new(
        node: LazyValue,
        attributes: Vec<LazyAttribute>,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
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
                GraphElementKey::NodeAttribute(node, attribute.name.clone()),
                self.debug_info.clone(),
            );
            if let Err(_) = exec.graph[node]
                .attributes
                .add(attribute.name.clone(), value)
            {
                return Err(ExecutionError::DuplicateAttribute(format!(
                    "{} on {}",
                    attribute.name, node,
                )))
                .with_context(|| {
                    (
                        prev_debug_info.unwrap().into(),
                        self.debug_info.clone().into(),
                    )
                        .into()
                });
            };
        }
        Ok(())
    }
}

impl fmt::Display for LazyAddGraphNodeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "attr ({})", self.node)?;
        for attr in &self.attributes {
            write!(f, " {}", attr)?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

/// Lazy statement to create a graph edge
#[derive(Debug)]
pub(super) struct LazyCreateEdge {
    source: LazyValue,
    sink: LazyValue,
    attributes: Attributes,
    debug_info: DebugInfo,
}

impl LazyCreateEdge {
    pub(super) fn new(
        source: LazyValue,
        sink: LazyValue,
        attributes: Attributes,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            source,
            sink,
            attributes,
            debug_info,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_graph_node(exec)?;
        let sink = self.sink.evaluate_as_graph_node(exec)?;
        let prev_debug_info = exec
            .prev_element_debug_info
            .insert(GraphElementKey::Edge(source, sink), self.debug_info.clone());
        let edge = match exec.graph[source].add_edge(sink) {
            Ok(edge) => edge,
            Err(_) => {
                return Err(ExecutionError::DuplicateEdge(format!(
                    "({} -> {})",
                    source, sink,
                )))
                .with_context(|| {
                    (
                        prev_debug_info.unwrap().into(),
                        self.debug_info.clone().into(),
                    )
                        .into()
                });
            }
        };
        edge.attributes = self.attributes.clone();
        Ok(())
    }
}

impl fmt::Display for LazyCreateEdge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "edge {} -> {} at {}",
            self.source, self.sink, self.debug_info,
        )
    }
}

/// Lazy statement to add graph edge attributes
#[derive(Debug)]
pub(super) struct LazyAddEdgeAttribute {
    source: LazyValue,
    sink: LazyValue,
    attributes: Vec<LazyAttribute>,
    debug_info: DebugInfo,
}

impl LazyAddEdgeAttribute {
    pub(super) fn new(
        source: LazyValue,
        sink: LazyValue,
        attributes: Vec<LazyAttribute>,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
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
                    source, sink, self.debug_info,
                ))),
            }?;
            let prev_debug_info = exec.prev_element_debug_info.insert(
                GraphElementKey::EdgeAttribute(source, sink, attribute.name.clone()),
                self.debug_info.clone(),
            );
            if let Err(_) = edge.attributes.add(attribute.name.clone(), value) {
                return Err(ExecutionError::DuplicateAttribute(format!(
                    "{} on edge ({} -> {})",
                    attribute.name, source, sink,
                )))
                .with_context(|| {
                    (
                        prev_debug_info.unwrap().into(),
                        self.debug_info.clone().into(),
                    )
                        .into()
                });
            }
        }
        Ok(())
    }
}

impl fmt::Display for LazyAddEdgeAttribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "attr ({} -> {})", self.source, self.sink,)?;
        for attr in &self.attributes {
            write!(f, " {}", attr,)?;
        }
        write!(f, " at {}", self.debug_info)
    }
}

/// Lazy statement to print values
#[derive(Debug)]
pub(super) struct LazyPrint {
    arguments: Vec<LazyPrintArgument>,
    debug_info: DebugInfo,
}

#[derive(Debug)]
pub(super) enum LazyPrintArgument {
    Text(String),
    Value(LazyValue),
}

impl LazyPrint {
    pub(super) fn new(arguments: Vec<LazyPrintArgument>, debug_info: DebugInfo) -> Self {
        Self {
            arguments,
            debug_info,
        }
    }

    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        for argument in &self.arguments {
            match argument {
                LazyPrintArgument::Text(string) => eprint!("{}", string),
                LazyPrintArgument::Value(value) => {
                    let value = value.evaluate(exec)?;
                    eprint!("{:?}", value);
                }
            }
        }
        eprintln!("");
        Ok(())
    }
}

impl fmt::Display for LazyPrint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "print")?;
        let mut first = true;
        for argument in &self.arguments {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            match argument {
                LazyPrintArgument::Text(string) => write!(f, "\"{}\"", string)?,
                LazyPrintArgument::Value(value) => write!(f, "{}", value)?,
            };
        }
        write!(f, " at {}", self.debug_info)
    }
}

/// Lazy attribute
#[derive(Debug)]
pub(super) struct LazyAttribute {
    name: Identifier,
    value: LazyValue,
}

impl LazyAttribute {
    pub(super) fn new(name: Identifier, value: LazyValue) -> Self {
        Self { name, value }
    }
}

impl fmt::Display for LazyAttribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value,)
    }
}
