// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Functions that can be called by graph DSL files

use std::collections::HashMap;

use crate::execution::error::ExecutionError;
use crate::graph::Graph;
use crate::graph::Value;
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
        &self,
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
/// # use tree_sitter_graph::graph::Graph;
/// # use tree_sitter_graph::graph::Value;
/// # use tree_sitter_graph::ExecutionError;
/// # fn main() -> Result<(), ExecutionError> {
/// # let param_vec = vec![Value::String("test".to_string()), Value::Integer(42)];
/// # let mut params = param_vec.into_iter();
/// let first_param = params.param()?.into_string()?;
/// let second_param = params.param()?.as_integer()?;
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
        let value = self
            .next()
            .ok_or(ExecutionError::InvalidParameters(format!(
                "expected more parameters"
            )))?;
        Ok(value)
    }

    fn finish(&mut self) -> Result<(), ExecutionError> {
        let value = self.next();
        if value.is_some() {
            return Err(ExecutionError::InvalidParameters(format!(
                "unexpected extra parameter"
            )));
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
    pub fn stdlib() -> Functions {
        let mut functions = Functions::new();
        // general functions
        functions.add(Identifier::from("eq"), stdlib::Eq);
        functions.add(Identifier::from("is-null"), stdlib::IsNull);
        // tree functions
        functions.add(
            Identifier::from("named-child-index"),
            stdlib::syntax::NamedChildIndex,
        );
        functions.add(Identifier::from("source-text"), stdlib::syntax::SourceText);
        functions.add(Identifier::from("start-row"), stdlib::syntax::StartRow);
        functions.add(
            Identifier::from("start-column"),
            stdlib::syntax::StartColumn,
        );
        functions.add(Identifier::from("end-row"), stdlib::syntax::EndRow);
        functions.add(Identifier::from("end-column"), stdlib::syntax::EndColumn);
        functions.add(Identifier::from("node-type"), stdlib::syntax::NodeType);
        functions.add(
            Identifier::from("named-child-count"),
            stdlib::syntax::NamedChildCount,
        );
        // graph functions
        functions.add(Identifier::from("node"), stdlib::graph::Node);
        // boolean functions
        functions.add(Identifier::from("not"), stdlib::bool::Not);
        functions.add(Identifier::from("and"), stdlib::bool::And);
        functions.add(Identifier::from("or"), stdlib::bool::Or);
        // math functions
        functions.add(Identifier::from("plus"), stdlib::math::Plus);
        // string functions
        functions.add(Identifier::from("format"), stdlib::string::Format);
        functions.add(Identifier::from("replace"), stdlib::string::Replace);
        // list functions
        functions.add(Identifier::from("concat"), stdlib::list::Concat);
        functions.add(Identifier::from("is-empty"), stdlib::list::IsEmpty);
        functions.add(Identifier::from("join"), stdlib::list::Join);
        functions.add(Identifier::from("length"), stdlib::list::Length);
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
        &self,
        name: &Identifier,
        graph: &mut Graph,
        source: &str,
        parameters: &mut dyn Parameters,
    ) -> Result<Value, ExecutionError> {
        let function = self
            .functions
            .get(name)
            .ok_or(ExecutionError::UndefinedFunction(format!("{}", name)))?;
        function.call(graph, source, parameters)
    }
}

/// Implementations of the [standard library functions][`crate::reference::functions`]
pub mod stdlib {
    use regex::Regex;

    use crate::execution::error::ExecutionError;
    use crate::graph::Graph;
    use crate::graph::Value;

    use super::Function;
    use super::Parameters;

    /// The implementation of the standard [`eq`][`crate::reference::functions#eq`] function.
    pub struct Eq;

    impl Function for Eq {
        fn call(
            &self,
            _graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let left = parameters.param()?;
            let right = parameters.param()?;
            parameters.finish()?;

            match &left {
                Value::Null => match right {
                    Value::Null => return Ok(true.into()),
                    _ => return Ok(false.into()),
                },
                Value::Boolean(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::Boolean(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::Integer(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::Integer(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::String(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::String(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::List(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::List(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::Set(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::Set(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::SyntaxNode(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::SyntaxNode(right) => return Ok((left == right).into()),
                    _ => {}
                },
                Value::GraphNode(left) => match &right {
                    Value::Null => return Ok(false.into()),
                    Value::GraphNode(right) => return Ok((left == right).into()),
                    _ => {}
                },
            };
            Err(ExecutionError::FunctionFailed(
                "eq".into(),
                format!(
                    "Cannot compare values of different types: {} and {}",
                    left, right
                ),
            ))
        }
    }

    /// The implementation of the standard [`is-null`][`crate::reference::functions#is-null`] function.
    pub struct IsNull;

    impl Function for IsNull {
        fn call(
            &self,
            _graph: &mut Graph,
            _source: &str,
            parameters: &mut dyn Parameters,
        ) -> Result<Value, ExecutionError> {
            let parameter = parameters.param()?;
            parameters.finish()?;
            let result = if let Value::Null = parameter {
                true
            } else {
                false
            };
            Ok(result.into())
        }
    }

    pub mod syntax {
        use super::*;

        /// The implementation of the standard [`named-child-index`][`crate::reference::functions#named-child-index`]
        /// function.
        pub struct NamedChildIndex;

        impl Function for NamedChildIndex {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                let parent = match node.parent() {
                    Some(parent) => parent,
                    None => {
                        return Err(ExecutionError::FunctionFailed(
                            "named-child-index".into(),
                            format!("Cannot call named-child-index on the root node"),
                        ))
                    }
                };
                let mut tree_cursor = parent.walk();
                let index = parent
                    .named_children(&mut tree_cursor)
                    .position(|child| child == node)
                    .ok_or(ExecutionError::FunctionFailed(
                        "named-child-index".into(),
                        format!("Called named-child-index on a non-named child"),
                    ))?;
                Ok(Value::Integer(index as u32))
            }
        }

        /// The implementation of the standard [`source-text`][`crate::reference::functions#source-text`]
        /// function.
        pub struct SourceText;

        impl Function for SourceText {
            fn call(
                &self,
                graph: &mut Graph,
                source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::String(source[node.byte_range()].to_string()))
            }
        }

        // The implementation of the standard [`start-row`][`crate::reference::functions#start-row`]
        // function.
        pub struct StartRow;

        impl Function for StartRow {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::Integer(node.start_position().row as u32))
            }
        }

        // The implementation of the standard
        // [`start-column`][`crate::reference::functions#start-column`]
        // function.
        pub struct StartColumn;

        impl Function for StartColumn {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::Integer(node.start_position().column as u32))
            }
        }

        // The implementation of the standard [`end-row`][`crate::reference::functions#end-row`]
        // function.
        pub struct EndRow;

        impl Function for EndRow {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::Integer(node.end_position().row as u32))
            }
        }

        // The implementation of the standard [`end-column`][`crate::reference::functions#end-column`]
        // function.
        pub struct EndColumn;

        impl Function for EndColumn {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::Integer(node.end_position().column as u32))
            }
        }

        // The implementation of the standard [`node-type`][`crate::reference::functions#node-type`]
        // function.
        pub struct NodeType;

        impl Function for NodeType {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::String(node.kind().to_string()))
            }
        }

        // The implementation of the standard
        // [`named-child-count`][`crate::reference::functions#named-child-count`] function.

        pub struct NamedChildCount;

        impl Function for NamedChildCount {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let node = graph[parameters.param()?.into_syntax_node_ref()?];
                parameters.finish()?;
                Ok(Value::Integer(node.named_child_count() as u32))
            }
        }
    }

    pub mod graph {
        use super::*;

        /// The implementation of the standard [`node`][`crate::reference::functions#node`] function.
        pub struct Node;

        impl Function for Node {
            fn call(
                &self,
                graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                parameters.finish()?;
                let node = graph.add_graph_node();
                Ok(Value::GraphNode(node))
            }
        }
    }

    pub mod bool {
        use super::*;

        /// The implementation of the standard [`not`][`crate::reference::functions#not`] function.
        pub struct Not;

        impl Function for Not {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let result = !parameters.param()?.as_boolean()?;
                parameters.finish()?;
                Ok(result.into())
            }
        }

        /// The implementation of the standard [`and`][`crate::reference::functions#and`] function.
        pub struct And;

        impl Function for And {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let mut result = true;
                while let Ok(parameter) = parameters.param() {
                    result &= parameter.as_boolean()?;
                }
                Ok(result.into())
            }
        }

        /// The implementation of the standard [`or`][`crate::reference::functions#or`] function.
        pub struct Or;

        impl Function for Or {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let mut result = false;
                while let Ok(parameter) = parameters.param() {
                    result |= parameter.as_boolean()?;
                }
                Ok(result.into())
            }
        }
    }

    pub mod math {
        use super::*;

        /// The implementation of the standard [`plus`][`crate::reference::functions#plus`] function.
        pub struct Plus;

        impl Function for Plus {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let mut result = 0;
                while let Ok(parameter) = parameters.param() {
                    result += parameter.as_integer()?;
                }
                Ok(Value::Integer(result))
            }
        }
    }

    pub mod string {
        use super::*;

        /// The implementation of the standard [`format`][`crate::reference::functions#format`] function.
        pub struct Format;

        impl Function for Format {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let format = parameters.param()?.into_string()?;
                let mut result = String::new();
                let mut it = format.chars().enumerate().into_iter();
                while let Some((_, c)) = it.next() {
                    match c {
                        '{' => match it.next() {
                            Some((_, '{')) => result.push('{'),
                            Some((_, '}')) => {
                                let value = parameters.param()?;
                                result += &value.to_string();
                            },
                            Some((i, c)) => return Err(ExecutionError::FunctionFailed("format".into(), format!("Unexpected character `{}` after `{{` at position {} in format string `{}`. Expected `{{` or `}}`.", c, i + 1, format))),
                            None => return Err(ExecutionError::FunctionFailed("format".into(), format!("Unexpected end of format string `{}` after `{{`. Expected `{{` or `}}`.", format))),
                        },
                        '}' => match it.next() {
                            Some((_, '}')) => result.push('}'),
                            Some((i, c)) => return Err(ExecutionError::FunctionFailed("format".into(), format!("Unexpected character `{}` after `}}` at position {} in format string `{}`. Expected `}}`.", c, i + 1, format))),
                            None => return Err(ExecutionError::FunctionFailed("format".into(), format!("Unexpected end of format string `{}` after `{{. Expected `}}`.", format))),
                        },
                        c => result.push(c),
                    }
                }
                parameters.finish()?;
                Ok(result.into())
            }
        }

        /// The implementation of the standard [`replace`][`crate::reference::functions#replace`] function.
        pub struct Replace;

        impl Function for Replace {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let text = parameters.param()?.into_string()?;
                let pattern = parameters.param()?.into_string()?;
                let pattern = Regex::new(&pattern).map_err(|e| {
                    ExecutionError::FunctionFailed("replace".into(), format!("{}", e))
                })?;
                let replacement = parameters.param()?.into_string()?;
                parameters.finish()?;
                Ok(Value::String(
                    pattern.replace_all(&text, replacement).to_string(),
                ))
            }
        }
    }

    pub mod list {
        use super::*;

        /// The implementation of the standard [`concat`][`crate::reference::functions#concat`] function.
        pub struct Concat;

        impl Function for Concat {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let mut result = Vec::new();
                while let Ok(list) = parameters.param() {
                    result.append(&mut list.into_list()?);
                }
                Ok(result.into())
            }
        }

        /// The implementation of the standard [`is-empty`][`crate::reference::functions#is-empty`] function.
        pub struct IsEmpty;

        impl Function for IsEmpty {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let list = parameters.param()?.into_list()?;
                Ok(list.is_empty().into())
            }
        }

        /// The implementation of the standard [`join`][`crate::reference::functions#join`] function.
        pub struct Join;

        impl Function for Join {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let list = parameters.param()?.into_list()?;
                let sep = match parameters.param() {
                    Ok(sep) => sep.into_string()?,
                    Err(_) => "".to_string(),
                };
                parameters.finish()?;
                let result = list
                    .into_iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(&sep);
                Ok(result.into())
            }
        }

        /// The implementation of the standard [`length`][`crate::reference::functions#length`] function.
        pub struct Length;

        impl Function for Length {
            fn call(
                &self,
                _graph: &mut Graph,
                _source: &str,
                parameters: &mut dyn Parameters,
            ) -> Result<Value, ExecutionError> {
                let list = parameters.param()?.into_list()?;
                Ok((list.len() as u32).into())
            }
        }
    }
}
