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
pub enum Statement {}

impl Statement {
    pub(super) fn evaluate(&self, exec: &mut EvaluationContext) -> Result<(), ExecutionError> {
        debug!("eval {}", self.display_with(exec.ctx, exec.graph));
        trace!("{{");
        let result = Ok(());
        trace!("}}");
        result
    }
}

impl DisplayWithContextAndGraph for Statement {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context, graph: &Graph) -> fmt::Result {
        Ok(())
    }
}
