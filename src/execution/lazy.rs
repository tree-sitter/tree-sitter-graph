// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

mod stanzas;
mod values;
mod variables;

use anyhow::Context as _;

use tree_sitter::QueryCursor;
use tree_sitter::Tree;

use crate::ast::File;
use crate::execution::ExecutionError;
use crate::execution::Variables;
use crate::functions::Functions;
use crate::graph::Graph;
use crate::Context;

use self::values::DisplayWithContextAndGraph as _;
use self::variables::ConcreteVariables;
use self::variables::LazyVariables;

impl File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute_lazy<'tree>(
        &self,
        ctx: &mut Context,
        tree: &'tree Tree,
        source: &'tree str,
        functions: &mut Functions,
        globals: &mut Variables,
        graph: &mut Graph<'tree>,
    ) -> Result<(), ExecutionError> {
        if tree.root_node().has_error() {
            return Err(ExecutionError::ParseTreeHasErrors);
        }
        let mut globals = ConcreteVariables::new(globals);
        let mut variables = LazyVariables::new_child(&mut globals);
        let mut function_arguments = Vec::new();
        let mut cursor = QueryCursor::new();
        let mut store = values::Store::new();
        let mut scoped_store = values::ScopedVariables::new();
        let mut lazy_graph = Vec::new();
        let regex_captures_identifier = ctx.add_identifier("$");
        for stanza in &self.stanzas {
            stanza.execute_lazy(
                ctx,
                tree,
                source,
                graph,
                &mut variables,
                &mut cursor,
                &mut store,
                &mut scoped_store,
                &mut lazy_graph,
                regex_captures_identifier,
            )?;
        }
        for graph_stmt in &lazy_graph {
            graph_stmt
                .evaluate(&mut values::EvaluationContext {
                    ctx,
                    source,
                    graph,
                    functions,
                    store: &mut store,
                    scoped_store: &mut scoped_store,
                    function_arguments: &mut function_arguments,
                    iteration: values::Iteration::new(),
                })
                .with_context(|| format!("Executing {}", graph_stmt.display_with(ctx, &graph)))?;
        }
        Ok(())
    }
}
