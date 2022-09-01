// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use thiserror::Error;
use tree_sitter::CaptureQuantifier;
use tree_sitter::QueryMatch;
use tree_sitter::Tree;

use crate::ast::File;
use crate::execution::error::ExecutionError;
use crate::functions::Functions;
use crate::graph::Graph;
use crate::graph::Value;
use crate::variables::Globals;
use crate::Identifier;

pub(crate) mod error;
mod lazy;
mod strict;

impl File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute<'a, 'tree>(
        &self,
        tree: &'tree Tree,
        source: &'tree str,
        config: &mut ExecutionConfig,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let mut graph = Graph::new();
        self.execute_into(&mut graph, tree, source, config, cancellation_flag)?;
        Ok(graph)
    }

    /// Executes this graph DSL file against a source file, saving the results into an existing
    /// `Graph` instance.  You must provide the parsed syntax tree (`tree`) as well as the source
    /// text that it was parsed from (`source`).  You also provide the set of functions and global
    /// variables that are available during execution. This variant is useful when you need to
    /// “pre-seed” the graph with some predefined nodes and/or edges before executing the DSL file.
    pub fn execute_into<'a, 'tree>(
        &self,
        graph: &mut Graph<'tree>,
        tree: &'tree Tree,
        source: &'tree str,
        config: &mut ExecutionConfig,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<(), ExecutionError> {
        if config.lazy {
            self.execute_lazy_into(graph, tree, source, config, cancellation_flag)
        } else {
            self.execute_strict_into(graph, tree, source, config, cancellation_flag)
        }
    }

    pub(self) fn check_globals(&self, globals: &Globals) -> Result<(), ExecutionError> {
        for global in &self.globals {
            match globals.get(&global.name) {
                None => {
                    return Err(ExecutionError::MissingGlobalVariable(
                        global.name.as_str().to_string(),
                    ));
                }
                Some(value) => {
                    if global.quantifier == CaptureQuantifier::ZeroOrMore
                        || global.quantifier == CaptureQuantifier::OneOrMore
                    {
                        if value.as_list().is_err() {
                            return Err(ExecutionError::ExpectedList(
                                global.name.as_str().to_string(),
                            ));
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

/// Configuration for the execution of a File
pub struct ExecutionConfig<'a, 'g> {
    pub(crate) functions: &'a Functions,
    pub(crate) globals: &'a Globals<'g>,
    pub(crate) lazy: bool,
    pub(crate) location_attr: Option<Identifier>,
    pub(crate) variable_name_attr: Option<Identifier>,
}

impl<'a, 'g> ExecutionConfig<'a, 'g> {
    pub fn new(functions: &'a Functions, globals: &'a Globals<'g>) -> Self {
        Self {
            functions,
            globals,
            lazy: false,
            location_attr: None,
            variable_name_attr: None,
        }
    }

    pub fn debug_attributes(
        self,
        location_attr: Identifier,
        variable_name_attr: Identifier,
    ) -> Self {
        Self {
            functions: self.functions,
            globals: self.globals,
            lazy: self.lazy,
            location_attr: location_attr.into(),
            variable_name_attr: variable_name_attr.into(),
        }
    }

    pub fn lazy(self, lazy: bool) -> Self {
        Self {
            functions: self.functions,
            globals: self.globals,
            lazy,
            location_attr: self.location_attr,
            variable_name_attr: self.variable_name_attr,
        }
    }
}

/// Trait to signal that the execution is cancelled
pub trait CancellationFlag {
    fn check(&self, at: &'static str) -> Result<(), CancellationError>;
}

pub struct NoCancellation;
impl CancellationFlag for NoCancellation {
    fn check(&self, _at: &'static str) -> Result<(), CancellationError> {
        Ok(())
    }
}

#[derive(Debug, Error)]
#[error("Cancelled at \"{0}\"")]
pub struct CancellationError(pub &'static str);

/// Get the value for the given capture, considering the suffix
pub(self) fn query_capture_value<'tree>(
    index: usize,
    quantifier: CaptureQuantifier,
    mat: &QueryMatch<'_, 'tree>,
    graph: &mut Graph<'tree>,
) -> Value {
    let mut nodes = mat
        .captures
        .iter()
        .filter(|c| c.index as usize == index)
        .map(|c| c.node);
    match quantifier {
        CaptureQuantifier::Zero => panic!("Capture with quantifier 0 has no value"),
        CaptureQuantifier::One => {
            let syntax_node = graph.add_syntax_node(nodes.next().unwrap());
            syntax_node.into()
        }
        CaptureQuantifier::ZeroOrMore | CaptureQuantifier::OneOrMore => {
            let syntax_nodes = nodes
                .map(|n| graph.add_syntax_node(n.clone()).into())
                .collect::<Vec<Value>>();
            syntax_nodes.into()
        }
        CaptureQuantifier::ZeroOrOne => match nodes.next() {
            None => Value::Null.into(),
            Some(node) => {
                let syntax_node = graph.add_syntax_node(node);
                syntax_node.into()
            }
        },
    }
}
