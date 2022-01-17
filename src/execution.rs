// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

mod variables;

use anyhow::Context as ErrorContext;
use thiserror::Error;
use tree_sitter::QueryCursor;
use tree_sitter::QueryMatch;
use tree_sitter::Tree;

use crate::ast::AddEdgeAttribute;
use crate::ast::AddGraphNodeAttribute;
use crate::ast::Assign;
use crate::ast::Call;
use crate::ast::Capture;
use crate::ast::CreateEdge;
use crate::ast::CreateGraphNode;
use crate::ast::DeclareImmutable;
use crate::ast::DeclareMutable;
use crate::ast::Expression;
use crate::ast::File;
use crate::ast::IntegerConstant;
use crate::ast::ListComprehension;
use crate::ast::Print;
use crate::ast::RegexCapture;
use crate::ast::Scan;
use crate::ast::ScopedVariable;
use crate::ast::SetComprehension;
use crate::ast::Stanza;
use crate::ast::Statement;
use crate::ast::StringConstant;
use crate::ast::UnscopedVariable;
use crate::ast::Variable;
pub use crate::execution::variables::Globals;
use crate::execution::variables::ScopedVariables;
use crate::execution::variables::VariableMap;
use crate::execution::variables::Variables;
use crate::functions::Functions;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::GraphNodeRef;
use crate::graph::Value;
use crate::Context;
use crate::DisplayWithContext;

impl File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute<'tree>(
        &self,
        ctx: &Context,
        tree: &'tree Tree,
        source: &'tree str,
        functions: &mut Functions,
        globals: &Globals,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let mut graph = Graph::new();
        self.execute_into(ctx, &mut graph, tree, source, functions, globals)?;
        Ok(graph)
    }

    /// Executes this graph DSL file against a source file, saving the results into an existing
    /// `Graph` instance.  You must provide the parsed syntax tree (`tree`) as well as the source
    /// text that it was parsed from (`source`).  You also provide the set of functions and global
    /// variables that are available during execution. This variant is useful when you need to
    /// “pre-seed” the graph with some predefined nodes and/or edges before executing the DSL file.
    pub fn execute_into<'tree>(
        &self,
        ctx: &Context,
        graph: &mut Graph<'tree>,
        tree: &'tree Tree,
        source: &'tree str,
        functions: &mut Functions,
        globals: &Globals,
    ) -> Result<(), ExecutionError> {
        if tree.root_node().has_error() {
            return Err(ExecutionError::ParseTreeHasErrors);
        }
        let mut locals = VariableMap::new();
        let mut scoped = ScopedVariables::new();
        let current_regex_captures = Vec::new();
        let mut function_parameters = Vec::new();
        let mut cursor = QueryCursor::new();
        for stanza in &self.stanzas {
            stanza.execute(
                ctx,
                tree,
                source,
                graph,
                functions,
                globals,
                &mut locals,
                &mut scoped,
                &current_regex_captures,
                &mut function_parameters,
                &mut cursor,
            )?;
        }
        Ok(())
    }
}

/// An error that can occur while executing a graph DSL file
#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error("Cannot assign immutable variable {0}")]
    CannotAssignImmutableVariable(String),
    #[error("Duplicate attribute {0}")]
    DuplicateAttribute(String),
    #[error("Duplicate edge {0}")]
    DuplicateEdge(String),
    #[error("Duplicate variable {0}")]
    DuplicateVariable(String),
    #[error("Expected a graph node reference {0}")]
    ExpectedGraphNode(String),
    #[error("Expected a list {0}")]
    ExpectedList(String),
    #[error("Expected a boolean {0}")]
    ExpectedBoolean(String),
    #[error("Expected an integer {0}")]
    ExpectedInteger(String),
    #[error("Expected a string {0}")]
    ExpectedString(String),
    #[error("Expected a syntax node {0}")]
    ExpectedSyntaxNode(String),
    #[error("Invalid parameters {0}")]
    InvalidParameters(String),
    #[error("Scoped variables can only be attached to syntax nodes {0}")]
    InvalidVariableScope(String),
    #[error("Undefined capture {0}")]
    UndefinedCapture(String),
    #[error("Undefined function {0}")]
    UndefinedFunction(String),
    #[error("Undefined regex capture {0}")]
    UndefinedRegexCapture(String),
    #[error("Empty regex capture {0}")]
    EmptyRegexCapture(String),
    #[error("Undefined edge {0}")]
    UndefinedEdge(String),
    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
    #[error("Parse tree has errors")]
    ParseTreeHasErrors,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl ExecutionError {
    /// Wraps an existing [`std::error::Error`][] as an execution error
    pub fn other<E>(err: E) -> ExecutionError
    where
        E: Into<anyhow::Error>,
    {
        ExecutionError::Other(err.into())
    }
}

struct ExecutionContext<'a, 'g, 's, 'tree> {
    ctx: &'a Context,
    source: &'tree str,
    graph: &'a mut Graph<'tree>,
    functions: &'a mut Functions,
    globals: &'a Globals<'g>,
    locals: &'a mut dyn Variables,
    scoped: &'a mut ScopedVariables<'s>,
    current_regex_captures: &'a Vec<String>,
    function_parameters: &'a mut Vec<Value>,
    mat: &'a QueryMatch<'a, 'tree>,
}

impl Stanza {
    fn execute<'l, 's, 'tree>(
        &self,
        ctx: &Context,
        tree: &'tree Tree,
        source: &'tree str,
        graph: &mut Graph<'tree>,
        functions: &mut Functions,
        globals: &Globals,
        locals: &mut VariableMap<'l>,
        scoped: &mut ScopedVariables<'s>,
        current_regex_captures: &Vec<String>,
        function_parameters: &mut Vec<Value>,
        cursor: &mut QueryCursor,
    ) -> Result<(), ExecutionError> {
        let matches = cursor.matches(&self.query, tree.root_node(), source.as_bytes());
        for mat in matches {
            locals.clear();
            let mut exec = ExecutionContext {
                ctx,
                source,
                graph,
                functions,
                globals,
                locals,
                scoped,
                current_regex_captures,
                function_parameters,
                mat: &mat,
            };
            for statement in &self.statements {
                statement
                    .execute(&mut exec)
                    .with_context(|| format!("Executing {}", statement.display_with(exec.ctx)))?;
            }
        }
        Ok(())
    }
}

impl Statement {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        match self {
            Statement::DeclareImmutable(statement) => statement.execute(exec),
            Statement::DeclareMutable(statement) => statement.execute(exec),
            Statement::Assign(statement) => statement.execute(exec),
            Statement::CreateGraphNode(statement) => statement.execute(exec),
            Statement::AddGraphNodeAttribute(statement) => statement.execute(exec),
            Statement::CreateEdge(statement) => statement.execute(exec),
            Statement::AddEdgeAttribute(statement) => statement.execute(exec),
            Statement::Scan(statement) => statement.execute(exec),
            Statement::Print(statement) => statement.execute(exec),
        }
    }
}

impl DeclareImmutable {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate(exec)?;
        self.variable.add(exec, value, false)
    }
}

impl DeclareMutable {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate(exec)?;
        self.variable.add(exec, value, true)
    }
}

impl Assign {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate(exec)?;
        self.variable.set(exec, value)
    }
}

impl CreateGraphNode {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let graph_node = exec.graph.add_graph_node();
        let value = Value::GraphNode(graph_node);
        self.node.add(exec, value, false)
    }
}

impl AddGraphNodeAttribute {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate_as_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            exec.graph[node]
                .attributes
                .add(attribute.name, value)
                .map_err(|_| {
                    ExecutionError::DuplicateAttribute(format!(
                        " {} on graph node ({}) in {}",
                        attribute.name.display_with(exec.ctx),
                        node.display_with(exec.graph),
                        self.display_with(exec.ctx),
                    ))
                })?;
        }
        Ok(())
    }
}

impl CreateEdge {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_node(exec)?;
        let sink = self.sink.evaluate_as_node(exec)?;
        let ctx = exec.ctx;
        if let Err(_) = exec.graph[source].add_edge(sink) {
            Err(ExecutionError::DuplicateEdge(format!(
                "({} -> {}) in {}",
                source.display_with(exec.graph),
                sink.display_with(exec.graph),
                self.display_with(ctx)
            )))?;
        }
        Ok(())
    }
}

impl AddEdgeAttribute {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_as_node(exec)?;
        let sink = self.sink.evaluate_as_node(exec)?;
        for attribute in &self.attributes {
            let value = attribute.value.evaluate(exec)?;
            let edge = match exec.graph[source].get_edge_mut(sink) {
                Some(edge) => Ok(edge),
                None => Err(ExecutionError::UndefinedEdge(format!(
                    "({} -> {}) in {}",
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    self.display_with(exec.ctx)
                ))),
            }?;
            edge.attributes.add(attribute.name, value).map_err(|_| {
                ExecutionError::DuplicateAttribute(format!(
                    " {} on edge ({} -> {}) in {}",
                    attribute.name.display_with(exec.ctx),
                    source.display_with(exec.graph),
                    sink.display_with(exec.graph),
                    self.display_with(exec.ctx),
                ))
            })?;
        }
        Ok(())
    }
}

impl Scan {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let match_string = self.value.evaluate(exec)?.into_string(exec.graph)?;

        let mut i = 0;
        let mut matches = Vec::new();
        while i < match_string.len() {
            matches.clear();
            for (index, arm) in self.arms.iter().enumerate() {
                let captures = arm.regex.captures(&match_string[i..]);
                if let Some(captures) = captures {
                    if captures.get(0).unwrap().range().is_empty() {
                        return Err(ExecutionError::EmptyRegexCapture(format!(
                            "for regular expression /{}/",
                            arm.regex
                        )));
                    }
                    matches.push((captures, index));
                }
            }

            if matches.is_empty() {
                return Ok(());
            }

            matches.sort_by_key(|(captures, index)| {
                let range = captures.get(0).unwrap().range();
                (range.start, *index)
            });

            let (regex_captures, block_index) = &matches[0];
            let arm = &self.arms[*block_index];

            let mut current_regex_captures = Vec::new();
            for regex_capture in regex_captures.iter() {
                current_regex_captures
                    .push(regex_capture.map(|m| m.as_str()).unwrap_or("").to_string());
            }

            let mut arm_locals = VariableMap::new_child(exec.locals);
            let mut arm_exec = ExecutionContext {
                ctx: exec.ctx,
                source: exec.source,
                graph: exec.graph,
                functions: exec.functions,
                globals: exec.globals,
                locals: &mut arm_locals,
                scoped: exec.scoped,
                current_regex_captures: &current_regex_captures,
                function_parameters: exec.function_parameters,
                mat: exec.mat,
            };

            for statement in &arm.statements {
                statement
                    .execute(&mut arm_exec)
                    .with_context(|| format!("Executing {}", statement.display_with(arm_exec.ctx)))
                    .with_context(|| {
                        format!(
                            "Matching {} with arm \"{}\" {{ ... }}",
                            match_string, arm.regex,
                        )
                    })?;
            }

            i += regex_captures.get(0).unwrap().range().end;
        }

        Ok(())
    }
}

impl Print {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        for value in &self.values {
            if let Expression::StringConstant(expr) = value {
                eprint!("{}", expr.value);
            } else {
                let value = value.evaluate(exec)?;
                eprint!("{}", value.display_with(exec.graph));
            }
        }
        eprintln!();
        Ok(())
    }
}

impl Expression {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        match self {
            Expression::FalseLiteral => Ok(Value::Boolean(false)),
            Expression::NullLiteral => Ok(Value::Null),
            Expression::TrueLiteral => Ok(Value::Boolean(true)),
            Expression::IntegerConstant(expr) => expr.evaluate(exec),
            Expression::StringConstant(expr) => expr.evaluate(exec),
            Expression::List(expr) => expr.evaluate(exec),
            Expression::Set(expr) => expr.evaluate(exec),
            Expression::Capture(expr) => expr.evaluate(exec),
            Expression::Variable(expr) => expr.evaluate(exec),
            Expression::Call(expr) => expr.evaluate(exec),
            Expression::RegexCapture(expr) => expr.evaluate(exec),
        }
    }

    fn evaluate_as_node(
        &self,
        exec: &mut ExecutionContext,
    ) -> Result<GraphNodeRef, ExecutionError> {
        let node = self.evaluate(exec)?;
        match node {
            Value::GraphNode(node) => Ok(node),
            _ => Err(ExecutionError::ExpectedGraphNode(format!(
                " {}, got {}",
                self.display_with(exec.ctx),
                node.display_with(exec.graph)
            ))),
        }
    }
}

impl IntegerConstant {
    fn evaluate(&self, _exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        Ok(Value::Integer(self.value))
    }
}

impl StringConstant {
    fn evaluate(&self, _exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        Ok(Value::String(self.value.clone()))
    }
}

impl ListComprehension {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::List(elements))
    }
}

impl SetComprehension {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::Set(elements))
    }
}

impl Capture {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        for capture in exec.mat.captures {
            if capture.index as usize == self.index {
                let syntax_node = exec.graph.add_syntax_node(capture.node);
                return Ok(Value::SyntaxNode(syntax_node));
            }
        }
        Err(ExecutionError::UndefinedCapture(format!(
            "{}",
            self.display_with(exec.ctx)
        )))
    }
}

impl Call {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        for parameter in &self.parameters {
            let parameter = parameter.evaluate(exec)?;
            exec.function_parameters.push(parameter);
        }
        exec.functions.call(
            exec.ctx,
            self.function,
            exec.graph,
            exec.source,
            &mut exec
                .function_parameters
                .drain(exec.function_parameters.len() - self.parameters.len()..),
        )
    }
}

impl RegexCapture {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let capture = exec.current_regex_captures.get(self.match_index).ok_or(
            ExecutionError::UndefinedRegexCapture(format!("{}", self.display_with(exec.ctx))),
        )?;
        Ok(Value::String(capture.clone()))
    }
}

impl Variable {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let value = self.get(exec)?;
        Ok(value.clone())
    }
}

impl Variable {
    fn get<'a>(&self, exec: &'a mut ExecutionContext) -> Result<&'a Value, ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.get(exec),
            Variable::Unscoped(variable) => variable.get(exec),
        }
    }

    fn add(
        &self,
        exec: &mut ExecutionContext,
        value: Value,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.add(exec, value, mutable),
            Variable::Unscoped(variable) => variable.add(exec, value, mutable),
        }
    }

    fn set(&self, exec: &mut ExecutionContext, value: Value) -> Result<(), ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.set(exec, value),
            Variable::Unscoped(variable) => variable.set(exec, value),
        }
    }
}

impl ScopedVariable {
    fn get<'a>(&self, exec: &'a mut ExecutionContext) -> Result<&'a Value, ExecutionError> {
        let scope = self.scope.evaluate(exec)?;
        let scope = match scope {
            Value::SyntaxNode(scope) => scope,
            _ => {
                return Err(ExecutionError::InvalidVariableScope(format!(
                    " got {}",
                    scope.display_with(exec.graph)
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        if let Some(value) = variables.get(self.name) {
            Ok(value)
        } else {
            Err(ExecutionError::UndefinedVariable(format!(
                "{} on node {}",
                self.display_with(exec.ctx),
                scope.display_with(exec.graph)
            )))
        }
    }

    fn add(
        &self,
        exec: &mut ExecutionContext,
        value: Value,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        let scope = self.scope.evaluate(exec)?;
        let scope = match scope {
            Value::SyntaxNode(scope) => scope,
            _ => {
                return Err(ExecutionError::InvalidVariableScope(format!(
                    " got {}",
                    scope.display_with(exec.graph)
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        variables.add(self.name, value, mutable).map_err(|_| {
            ExecutionError::DuplicateVariable(format!(" {}", self.display_with(exec.ctx)))
        })
    }

    fn set(&self, exec: &mut ExecutionContext, value: Value) -> Result<(), ExecutionError> {
        let scope = self.scope.evaluate(exec)?;
        let scope = match scope {
            Value::SyntaxNode(scope) => scope,
            _ => {
                return Err(ExecutionError::InvalidVariableScope(format!(
                    " got {}",
                    scope.display_with(exec.graph)
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        variables.set(self.name, value).map_err(|_| {
            ExecutionError::DuplicateVariable(format!(" {}", self.display_with(exec.ctx)))
        })
    }
}

impl UnscopedVariable {
    fn get<'a>(&self, exec: &'a mut ExecutionContext) -> Result<&'a Value, ExecutionError> {
        if let Some(value) = exec.globals.get(self.name) {
            Some(value)
        } else {
            exec.locals.get(self.name)
        }
        .ok_or_else(|| {
            ExecutionError::UndefinedVariable(format!("{}", self.display_with(exec.ctx)))
        })
    }

    fn add(
        &self,
        exec: &mut ExecutionContext,
        value: Value,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        if exec.globals.get(self.name).is_some() {
            return Err(ExecutionError::DuplicateVariable(format!(
                " global {}",
                self.display_with(exec.ctx)
            )));
        }
        exec.locals.add(self.name, value, mutable).map_err(|_| {
            ExecutionError::DuplicateVariable(format!(" local {}", self.display_with(exec.ctx)))
        })
    }

    fn set(&self, exec: &mut ExecutionContext, value: Value) -> Result<(), ExecutionError> {
        if exec.globals.get(self.name).is_some() {
            return Err(ExecutionError::CannotAssignImmutableVariable(format!(
                " global {}",
                self.display_with(exec.ctx)
            )));
        }
        exec.locals.set(self.name, value).map_err(|_| {
            if exec.locals.get(self.name).is_some() {
                ExecutionError::CannotAssignImmutableVariable(format!(
                    "{}",
                    self.display_with(exec.ctx)
                ))
            } else {
                ExecutionError::UndefinedVariable(format!("{}", self.display_with(exec.ctx)))
            }
        })
    }
}
