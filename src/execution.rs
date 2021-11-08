// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::collections::HashMap;

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
use crate::ast::CaptureExists;
use crate::ast::CreateEdge;
use crate::ast::CreateGraphNode;
use crate::ast::DeclareImmutable;
use crate::ast::DeclareMutable;
use crate::ast::Expression;
use crate::ast::File;
use crate::ast::If;
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
use crate::functions::Functions;
use crate::graph::DisplayWithGraph;
use crate::graph::Graph;
use crate::graph::GraphNodeRef;
use crate::graph::SyntaxNodeRef;
use crate::graph::Value;
use crate::Context;
use crate::DisplayWithContext;
use crate::Identifier;

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
        globals: &mut Variables,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let mut graph = Graph::new();
        let mut locals = Variables::new();
        let mut scoped = ScopedVariables::new();
        let mut current_regex_matches = Vec::new();
        let mut function_parameters = Vec::new();
        let mut cursor = QueryCursor::new();
        for stanza in &self.stanzas {
            stanza.execute(
                ctx,
                tree,
                source,
                &mut graph,
                functions,
                globals,
                &mut locals,
                &mut scoped,
                &mut current_regex_matches,
                &mut function_parameters,
                &mut cursor,
            )?;
        }
        Ok(graph)
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
    #[error("Expected an integer {0}")]
    ExpectedInteger(String),
    #[error("Expected a string {0}")]
    ExpectedString(String),
    #[error("Expected a boolean {0}")]
    ExpectedBoolean(String),
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
    #[error("Undefined edge {0}")]
    UndefinedEdge(String),
    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
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

struct ExecutionContext<'a, 'tree> {
    ctx: &'a Context,
    source: &'tree str,
    graph: &'a mut Graph<'tree>,
    functions: &'a mut Functions,
    globals: &'a mut Variables,
    locals: &'a mut Variables,
    scoped: &'a mut ScopedVariables,
    current_regex_matches: &'a mut Vec<String>,
    function_parameters: &'a mut Vec<Value>,
    mat: &'a QueryMatch<'a, 'tree>,
}

/// An environment of named variables
#[derive(Default)]
pub struct Variables {
    values: Vec<NamedVariable>,
}

struct NamedVariable {
    name: Identifier,
    value: Value,
    mutable: bool,
}

impl Variables {
    /// Creates a new, empty environment of variables.
    pub fn new() -> Variables {
        Variables::default()
    }

    /// Adds a new mutable variable to an environment, returning an error if the variable already
    /// exists.
    pub fn add_mutable(&mut self, name: Identifier, value: Value) -> Result<(), ()> {
        self.add(name, value, true)
    }

    /// Adds a new immutable variable to an environment, returning an error if the variable already
    /// exists.
    pub fn add_immutable(&mut self, name: Identifier, value: Value) -> Result<(), ()> {
        self.add(name, value, false)
    }

    fn add(&mut self, name: Identifier, value: Value, mutable: bool) -> Result<(), ()> {
        match self.values.binary_search_by_key(&name, |v| v.name) {
            Ok(_) => Err(()),
            Err(index) => {
                let variable = NamedVariable {
                    name,
                    value,
                    mutable,
                };
                self.values.insert(index, variable);
                Ok(())
            }
        }
    }

    /// Returns the value of a variable, if it exists in this environment.
    pub fn get(&self, name: Identifier) -> Option<&Value> {
        self.values
            .binary_search_by_key(&name, |v| v.name)
            .ok()
            .map(|index| &self.values[index].value)
    }

    fn resolve(&mut self, name: Identifier) -> Option<&mut NamedVariable> {
        self.values
            .binary_search_by_key(&name, |v| v.name)
            .ok()
            .map(move |index| &mut self.values[index])
    }

    /// Clears this list of variables.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

#[derive(Default)]
struct ScopedVariables {
    scopes: HashMap<SyntaxNodeRef, Variables>,
}

impl ScopedVariables {
    fn new() -> ScopedVariables {
        ScopedVariables::default()
    }

    fn get(&mut self, scope: SyntaxNodeRef) -> &mut Variables {
        self.scopes.entry(scope).or_default()
    }
}

impl Stanza {
    fn execute<'tree>(
        &self,
        ctx: &Context,
        tree: &'tree Tree,
        source: &'tree str,
        graph: &mut Graph<'tree>,
        functions: &mut Functions,
        globals: &mut Variables,
        locals: &mut Variables,
        scoped: &mut ScopedVariables,
        current_regex_matches: &mut Vec<String>,
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
                current_regex_matches,
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
            Statement::If(statement) => statement.execute(exec),
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
        let variable = self.variable.resolve(exec)?;
        if !variable.mutable {
            return Err(ExecutionError::CannotAssignImmutableVariable(format!(
                " {} in {}",
                self.variable.display_with(exec.ctx),
                self.display_with(exec.ctx)
            )));
        }
        variable.value = value;
        Ok(())
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
            for statement in &arm.statements {
                exec.current_regex_matches.clear();
                for regex_capture in regex_captures.iter() {
                    exec.current_regex_matches
                        .push(regex_capture.map(|m| m.as_str()).unwrap_or("").to_string());
                }
                statement
                    .execute(exec)
                    .with_context(|| format!("Executing {}", statement.display_with(exec.ctx)))
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

impl If {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let condition = self.condition.evaluate(exec)?;
        if condition.into_boolean(exec.graph)? {
            for statement in &self.consequence {
                statement.execute(exec)?;
            }
        }
        Ok(())
    }
}

impl Print {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        for value in &self.values {
            let value = value.evaluate(exec)?;
            print!("{}", value.display_with(exec.graph));
        }
        println!("");
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
            Expression::CaptureExists(expr) => expr.evaluate(exec),
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

impl CaptureExists {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        match self.capture.evaluate(exec) {
            Err(ExecutionError::UndefinedCapture(_)) => Ok(Value::Boolean(false)),
            Ok(_) => Ok(Value::Boolean(true)),
            Err(e) => Err(e),
        }
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
        let capture = exec.current_regex_matches.get(self.match_index).ok_or(
            ExecutionError::UndefinedRegexCapture(format!("{}", self.display_with(exec.ctx))),
        )?;
        Ok(Value::String(capture.clone()))
    }
}

impl Variable {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let variable = self.resolve(exec)?;
        Ok(variable.value.clone())
    }
}

impl Variable {
    fn resolve<'a>(
        &self,
        exec: &'a mut ExecutionContext,
    ) -> Result<&'a mut NamedVariable, ExecutionError> {
        match self {
            Variable::Scoped(variable) => variable.resolve(exec),
            Variable::Unscoped(variable) => variable.resolve(exec),
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
}

impl ScopedVariable {
    fn resolve<'a>(
        &self,
        exec: &'a mut ExecutionContext,
    ) -> Result<&'a mut NamedVariable, ExecutionError> {
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
        variables
            .resolve(self.name)
            .ok_or(ExecutionError::UndefinedVariable(format!(
                "{} on node {}",
                self.display_with(exec.ctx),
                scope.display_with(exec.graph)
            )))
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
}

impl UnscopedVariable {
    fn resolve<'a>(
        &self,
        exec: &'a mut ExecutionContext,
    ) -> Result<&'a mut NamedVariable, ExecutionError> {
        if let Some(variable) = exec.globals.resolve(self.name) {
            return Ok(variable);
        }
        if let Some(variable) = exec.locals.resolve(self.name) {
            return Ok(variable);
        }
        Err(ExecutionError::UndefinedVariable(format!(
            "{}",
            self.display_with(exec.ctx)
        )))
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
}
