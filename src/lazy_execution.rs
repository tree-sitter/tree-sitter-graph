// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

mod store;
mod values;

use std::collections::HashMap;
use std::fmt;

use tree_sitter::Query;
use tree_sitter::Tree;

use crate::ast;
use crate::execution::ExecutionError;
use crate::execution::Globals;
use crate::functions::Functions;
use crate::graph;
use crate::graph::Graph;
use crate::parser::FULL_MATCH;
use crate::Context;
use crate::Identifier;

use store::*;

impl ast::File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute_lazy<'tree>(
        &self,
        _ctx: &Context,
        tree: &'tree Tree,
        _source: &'tree str,
        _functions: &mut Functions,
        _globals: &Globals,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let graph = Graph::new();
        if tree.root_node().has_error() {
            return Err(ExecutionError::ParseTreeHasErrors);
        }
        Ok(graph)
    }
}

/// Context for evaluation, which evalautes the lazy graph to build the actual graph
pub(self) struct EvaluationContext<'a, 'tree> {
    pub ctx: &'a Context,
    pub source: &'tree str,
    pub graph: &'a mut Graph<'tree>,
    pub functions: &'a mut Functions,
    pub store: &'a Store,
    pub scoped_store: &'a ScopedVariables,
    pub function_parameters: &'a mut Vec<graph::Value>, // re-usable buffer to reduce memory allocations
    pub prev_element_debug_info: &'a mut HashMap<GraphElementKey, DebugInfo>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) enum GraphElementKey {
    NodeAttribute(graph::GraphNodeRef, Identifier),
    Edge(graph::GraphNodeRef, graph::GraphNodeRef),
    EdgeAttribute(graph::GraphNodeRef, graph::GraphNodeRef, Identifier),
}

#[allow(unused)]
fn full_match_capture_index(query: &Query) -> usize {
    query
        .capture_names()
        .iter()
        .position(|c| c == FULL_MATCH)
        .unwrap()
}

/// Trait to Display with a given Context and Graph
pub trait DisplayWithContextAndGraph
where
    Self: Sized,
{
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result;

    fn display_with<'a, 'tree>(
        &'a self,
        ctx: &'a Context,
        graph: &'a Graph<'tree>,
    ) -> Box<dyn fmt::Display + 'a> {
        struct Impl<'a, 'tree, T: DisplayWithContextAndGraph>(&'a T, &'a Context, &'a Graph<'tree>);

        impl<'a, 'tree, T: DisplayWithContextAndGraph> fmt::Display for Impl<'a, 'tree, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.fmt(f, self.1, self.2)
            }
        }

        Box::new(Impl(self, ctx, graph))
    }
}

impl<T: DisplayWithContextAndGraph> DisplayWithContextAndGraph for Box<T> {
    fn fmt<'tree>(
        &self,
        f: &mut fmt::Formatter,
        ctx: &Context,
        graph: &Graph<'tree>,
    ) -> fmt::Result {
        self.as_ref().fmt(f, ctx, graph)
    }
}
