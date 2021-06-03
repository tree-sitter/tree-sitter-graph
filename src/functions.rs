// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Functions that can be called by graph DSL files

use std::collections::HashMap;

use crate::execution::ExecutionError;
use crate::graph::Graph;
use crate::graph::Value;
use crate::Context;
use crate::Identifier;

/// The implementation of a function that can be called from the graph DSL.
///
/// You have access to the graph, as it has been constructed up to the point of the function call,
/// as well as the text content of the source file that's being processed.
///
/// Any other data that you need must be passed in as a parameter to the function.  You can use the
/// [`Parameters`][] trait to consume those parameters and verify that you received the correct
/// number and type of them.
pub trait Function {
    fn call(
        &mut self,
        graph: &mut Graph,
        source: &str,
        parameters: &mut dyn Parameters,
    ) -> Result<Value, ExecutionError>;
}

/// A helper trait for consuming the parameters of a function.  You will typically use it as
/// follows:
///
/// ```
/// # use tree_sitter_graph::functions::Parameters;
/// # use tree_sitter_graph::graph::Value;
/// # use tree_sitter_graph::ExecutionError;
/// # fn main() -> Result<(), ExecutionError> {
/// # let param_vec = vec![Value::String("test".to_string()), Value::Integer(42)];
/// # let mut params = param_vec.into_iter();
/// let first_param = params.param()?.into_string()?;
/// let second_param = params.param()?.into_integer()?;
/// // etc
/// params.finish()?;
/// # Ok(())
/// # }
/// ```
pub trait Parameters {
    /// Returns the next parameter, returning an error if you have exhausted all of the parameters
    /// that were passed in.
    fn param(&mut self) -> Result<Value, ExecutionError>;

    /// Ensures that there are no more parameters to consume.
    fn finish(&mut self) -> Result<(), ExecutionError>;
}

impl<I> Parameters for I
where
    I: Iterator<Item = Value>,
{
    fn param(&mut self) -> Result<Value, ExecutionError> {
        let value = self.next().ok_or(ExecutionError::InvalidParameters)?;
        Ok(value)
    }

    fn finish(&mut self) -> Result<(), ExecutionError> {
        if self.next().is_some() {
            return Err(ExecutionError::InvalidParameters);
        }
        Ok(())
    }
}

/// A library of named functions.
#[derive(Default)]
pub struct Functions {
    functions: HashMap<Identifier, Box<dyn Function>>,
}

impl Functions {
    /// Creates a new, empty library of functions.
    pub fn new() -> Functions {
        Functions::default()
    }

    /// Returns the standard library of functions, as defined in the [language
    /// reference][`crate::reference::functions`].
    pub fn stdlib(ctx: &mut Context) -> Functions {
        let mut functions = Functions::new();
        functions.add(ctx.add_identifier("child-index"), stdlib::ChildIndex);
        functions.add(ctx.add_identifier("node"), stdlib::Node);
        functions.add(ctx.add_identifier("plus"), stdlib::Plus);
        functions.add(ctx.add_identifier("replace"), stdlib::Replace);
        functions.add(ctx.add_identifier("source-text"), stdlib::SourceText);
        functions
    }

    /// Adds a new function to this library.
    pub fn add<F>(&mut self, name: Identifier, function: F)
    where
        F: Function + 'static,
    {
        self.functions.insert(name, Box::new(function));
    }

    /// Calls a named function, returning an error if there is no function with that name.
    pub fn call(
        &mut self,
        name: Identifier,
        graph: &mut Graph,
        source: &str,
        parameters: &mut dyn Parameters,
    ) -> Result<Value, ExecutionError> {
        let function = self
            .functions
            .get_mut(&name)
            .ok_or(ExecutionError::UndefinedFunction)?;
        function.call(graph, source, parameters)
    }
}

/// Implementations of the [standard library functions][`crate::reference::functions`]
pub mod stdlib {
    use anyhow::anyhow;
    use regex::Regex;

    use crate::execution::ExecutionError;
    use crate::graph::Graph;
    use crate::graph::Value;

    use super::Function;
    use super::Parameters;

    /// The implementation of the standard [`child-index`][`crate::reference::functions#child-index`]
    /// function.
    pub struct ChildIndex;

    impl Function for ChildIndex {
        fn call(
            &mut self,
            graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let node = parameters.param()?.into_syntax_node(graph)?;
            parameters.finish()?;
            let parent = match node.parent() {
                Some(parent) => parent,
                None => return Err(anyhow!("Cannot call child-index on the root node").into()),
            };
            let mut tree_cursor = parent.walk();
            let index = parent
                .named_children(&mut tree_cursor)
                .position(|child| child == *node)
                .ok_or(anyhow!("Called child-index on a non-named child"))?;
            Ok(Value::Integer(index as u32))
        }
    }

    /// The implementation of the standard [`node`][`crate::reference::functions#node`] function.
    pub struct Node;

    impl Function for Node {
        fn call(
            &mut self,
            graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            parameters.finish()?;
            let node = graph.add_graph_node();
            Ok(Value::GraphNode(node))
        }
    }

    /// The implementation of the standard [`plus`][`crate::reference::functions#plus`] function.
    pub struct Plus;

    impl Function for Plus {
        fn call(
            &mut self,
            _graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let mut result = 0;
            while let Ok(parameter) = parameters.param() {
                result += parameter.into_integer()?;
            }
            Ok(Value::Integer(result))
        }
    }

    /// The implementation of the standard [`replace`][`crate::reference::functions#replace`] function.
    pub struct Replace;

    impl Function for Replace {
        fn call(
            &mut self,
            _graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let text = parameters.param()?.into_string()?;
            let pattern = parameters.param()?.into_string()?;
            let pattern = Regex::new(&pattern).map_err(ExecutionError::other)?;
            let replacement = parameters.param()?.into_string()?;
            parameters.finish()?;
            Ok(Value::String(
                pattern.replace_all(&text, replacement).to_string(),
            ))
        }
    }

    /// The implementation of the standard [`source-text`][`crate::reference::functions#source-text`]
    /// function.
    pub struct SourceText;

    impl Function for SourceText {
        fn call(
            &mut self,
            graph: &mut Graph,
            source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let node = parameters.param()?.into_syntax_node(graph)?;
            parameters.finish()?;
            Ok(Value::String(source[node.byte_range()].to_string()))
        }
    }
}
