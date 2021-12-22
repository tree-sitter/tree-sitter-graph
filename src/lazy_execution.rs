// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use crate::ast;
use crate::execution::ExecutionError;
use crate::execution::Globals;
use crate::functions::Functions;
use crate::graph::Graph;
use crate::Context;
use tree_sitter::Tree;

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
