// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

mod statements;
mod store;
mod values;

use anyhow::Context as _;
use log::{debug, trace};

use std::collections::HashMap;
use std::fmt;

use tree_sitter::CaptureQuantifier::One;
use tree_sitter::QueryCursor;
use tree_sitter::QueryMatch;
use tree_sitter::Tree;

use crate::ast;
use crate::execution::query_capture_value;
use crate::execution::ExecutionError;
use crate::functions::Functions;
use crate::graph;
use crate::graph::DisplayWithGraph as _;
use crate::graph::Graph;
use crate::variables::Globals;
use crate::variables::VariableMap;
use crate::variables::Variables;
use crate::Context;
use crate::DisplayWithContext as _;
use crate::Identifier;

use statements::*;
use store::*;
use values::*;

impl ast::File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute_lazy<'tree>(
        &self,
        ctx: &Context,
        tree: &'tree Tree,
        source: &'tree str,
        functions: &mut Functions,
        globals: &Globals,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let mut graph = Graph::new();
        self.execute_lazy_into(ctx, &mut graph, tree, source, functions, globals)?;
        Ok(graph)
    }

    /// Executes this graph DSL file against a source file, saving the results into an existing
    /// `Graph` instance.  You must provide the parsed syntax tree (`tree`) as well as the source
    /// text that it was parsed from (`source`).  You also provide the set of functions and global
    /// variables that are available during execution. This variant is useful when you need to
    /// “pre-seed” the graph with some predefined nodes and/or edges before executing the DSL file.
    pub fn execute_lazy_into<'tree>(
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
        let mut cursor = QueryCursor::new();
        let mut store = LazyStore::new();
        let mut scoped_store = LazyScopedVariables::new();
        let mut lazy_graph = Vec::new();

        let query = &self.query.as_ref().unwrap();
        let matches = cursor.matches(query, tree.root_node(), source.as_bytes());
        for mat in matches {
            let stanza = &self.stanzas[mat.pattern_index];
            stanza.execute_lazy(
                ctx,
                &mat,
                graph,
                globals,
                &mut locals,
                &mut store,
                &mut scoped_store,
                &mut lazy_graph,
            )?;
        }

        let mut function_parameters = Vec::new();
        let mut prev_element_debug_info = HashMap::new();
        for graph_stmt in &lazy_graph {
            graph_stmt
                .evaluate(&mut EvaluationContext {
                    ctx,
                    source,
                    graph,
                    functions,
                    store: &mut store,
                    scoped_store: &mut scoped_store,
                    function_parameters: &mut function_parameters,
                    prev_element_debug_info: &mut prev_element_debug_info,
                })
                .with_context(|| format!("Executing {}", graph_stmt.display_with(ctx, &graph)))?;
        }

        Ok(())
    }
}

/// Context for execution, which executes stanzas to build the lazy graph
struct ExecutionContext<'a, 'g, 'tree> {
    ctx: &'a Context,
    graph: &'a mut Graph<'tree>,
    globals: &'a Globals<'g>,
    locals: &'a mut dyn Variables<LazyValue>,
    current_regex_captures: &'a Vec<String>,
    mat: &'a QueryMatch<'a, 'tree>,
    store: &'a mut LazyStore,
    scoped_store: &'a mut LazyScopedVariables,
    lazy_graph: &'a mut Vec<LazyStatement>,
}

/// Context for evaluation, which evalautes the lazy graph to build the actual graph
pub(self) struct EvaluationContext<'a, 'tree> {
    pub ctx: &'a Context,
    pub source: &'tree str,
    pub graph: &'a mut Graph<'tree>,
    pub functions: &'a mut Functions,
    pub store: &'a LazyStore,
    pub scoped_store: &'a LazyScopedVariables,
    pub function_parameters: &'a mut Vec<graph::Value>, // re-usable buffer to reduce memory allocations
    pub prev_element_debug_info: &'a mut HashMap<GraphElementKey, DebugInfo>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) enum GraphElementKey {
    NodeAttribute(graph::GraphNodeRef, Identifier),
    Edge(graph::GraphNodeRef, graph::GraphNodeRef),
    EdgeAttribute(graph::GraphNodeRef, graph::GraphNodeRef, Identifier),
}

impl ast::Stanza {
    fn execute_lazy<'l, 'g, 'q, 'tree>(
        &self,
        ctx: &Context,
        mat: &QueryMatch<'_, 'tree>,
        graph: &mut Graph<'tree>,
        globals: &Globals<'g>,
        locals: &mut VariableMap<'l, LazyValue>,
        store: &mut LazyStore,
        scoped_store: &mut LazyScopedVariables,
        lazy_graph: &mut Vec<LazyStatement>,
    ) -> Result<(), ExecutionError> {
        let current_regex_captures = vec![];
        locals.clear();
        let mut exec = ExecutionContext {
            ctx,
            graph,
            globals,
            locals,
            current_regex_captures: &current_regex_captures,
            mat,
            store,
            scoped_store,
            lazy_graph,
        };
        let node = query_capture_value(self.full_match_file_capture_index, One, &mat, exec.graph);
        debug!(
            "match {} at {}",
            node.display_with(exec.graph),
            self.location
        );
        trace!("{{");
        for statement in &self.statements {
            statement
                .execute_lazy(&mut exec)
                .with_context(|| format!("Executing {}", statement.display_with(exec.ctx)))?;
        }
        trace!("}}");
        Ok(())
    }
}

impl ast::Statement {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        match self {
            Self::DeclareImmutable(statement) => statement.execute_lazy(exec),
            Self::DeclareMutable(statement) => statement.execute_lazy(exec),
            Self::Assign(statement) => statement.execute_lazy(exec),
            Self::CreateGraphNode(statement) => statement.execute_lazy(exec),
            Self::AddGraphNodeAttribute(statement) => statement.execute_lazy(exec),
            Self::CreateEdge(statement) => statement.execute_lazy(exec),
            Self::AddEdgeAttribute(statement) => statement.execute_lazy(exec),
            Self::Scan(statement) => statement.execute_lazy(exec),
            Self::Print(statement) => statement.execute_lazy(exec),
            Self::If(statement) => statement.execute_lazy(exec),
            Self::ForIn(statement) => statement.execute_lazy(exec),
        }
    }
}

impl ast::DeclareImmutable {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.add_lazy(exec, value, false)
    }
}

impl ast::DeclareMutable {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.add_lazy(exec, value, true)
    }
}

impl ast::Assign {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        self.variable.set_lazy(exec, value)
    }
}

impl ast::CreateGraphNode {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let graph_node = exec.graph.add_graph_node();
        self.node.add_lazy(exec, graph_node.into(), false)
    }
}

impl ast::AddGraphNodeAttribute {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate_lazy(exec)?;
        let mut attributes = Vec::new();
        for attribute in &self.attributes {
            attributes.push(attribute.evaluate_lazy(exec)?);
        }
        let stmt = LazyAddGraphNodeAttribute::new(node, attributes, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl ast::CreateEdge {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_lazy(exec)?;
        let sink = self.sink.evaluate_lazy(exec)?;
        let stmt = LazyCreateEdge::new(source, sink, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl ast::AddEdgeAttribute {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate_lazy(exec)?;
        let sink = self.sink.evaluate_lazy(exec)?;
        let mut attributes = Vec::new();
        for attribute in &self.attributes {
            attributes.push(attribute.evaluate_lazy(exec)?);
        }
        let stmt = LazyAddEdgeAttribute::new(source, sink, attributes, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl ast::Scan {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let match_string = self.value.evaluate_strict(exec)?.into_string(exec.graph)?;

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
                graph: exec.graph,
                globals: exec.globals,
                locals: &mut arm_locals,
                current_regex_captures: &current_regex_captures,
                mat: exec.mat,
                store: exec.store,
                scoped_store: exec.scoped_store,
                lazy_graph: exec.lazy_graph,
            };

            for statement in &arm.statements {
                statement
                    .execute_lazy(&mut arm_exec)
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

impl ast::ScanExpression {
    fn evaluate_strict(&self, exec: &mut ExecutionContext) -> Result<graph::Value, ExecutionError> {
        match self {
            Self::StringConstant(expr) => expr.evaluate_strict(exec),
            Self::Capture(expr) => expr.evaluate_strict(exec),
            Self::Variable(expr) => expr.evaluate_strict(exec).map(|v| v.clone()),
            Self::RegexCapture(expr) => expr.evaluate_strict(exec),
        }
    }
}

impl ast::Print {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let mut arguments = Vec::new();
        for value in &self.values {
            let argument = if let ast::Expression::StringConstant(expr) = value {
                LazyPrintArgument::Text(expr.value.clone())
            } else {
                LazyPrintArgument::Value(value.evaluate_lazy(exec)?)
            };
            arguments.push(argument);
        }
        let stmt = LazyPrint::new(arguments, self.location.into());
        exec.lazy_graph.push(stmt.into());
        Ok(())
    }
}

impl ast::If {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        for arm in &self.arms {
            let mut result = true;
            for condition in &arm.conditions {
                result &= condition.test_strict(exec)?;
            }
            if result {
                let mut arm_locals = VariableMap::new_child(exec.locals);
                let mut arm_exec = ExecutionContext {
                    ctx: exec.ctx,
                    graph: exec.graph,
                    globals: exec.globals,
                    locals: &mut arm_locals,
                    current_regex_captures: exec.current_regex_captures,
                    mat: exec.mat,
                    store: exec.store,
                    scoped_store: exec.scoped_store,
                    lazy_graph: exec.lazy_graph,
                };
                for stmt in &arm.statements {
                    stmt.execute_lazy(&mut arm_exec)?;
                }
                break;
            }
        }
        Ok(())
    }
}

impl ast::Condition {
    fn test_strict(&self, exec: &mut ExecutionContext) -> Result<bool, ExecutionError> {
        let mut result = true;
        match self {
            Self::Some(captures) => {
                for capture in captures {
                    result &= !capture.evaluate_strict(exec)?.is_null();
                }
            }
            Self::None(captures) => {
                for capture in captures {
                    result &= capture.evaluate_strict(exec)?.is_null();
                }
            }
        }
        Ok(result)
    }
}

impl ast::ForIn {
    fn execute_lazy(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let values = self.capture.evaluate_strict(exec)?.into_list(exec.graph)?;
        let mut loop_locals = VariableMap::new_child(exec.locals);
        for value in values {
            loop_locals.clear();
            let mut loop_exec = ExecutionContext {
                ctx: exec.ctx,
                graph: exec.graph,
                globals: exec.globals,
                locals: &mut loop_locals,
                current_regex_captures: exec.current_regex_captures,
                mat: exec.mat,
                store: exec.store,
                scoped_store: exec.scoped_store,
                lazy_graph: exec.lazy_graph,
            };
            self.variable
                .add_lazy(&mut loop_exec, value.into(), false)?;
            for stmt in &self.statements {
                stmt.execute_lazy(&mut loop_exec)?;
            }
        }
        Ok(())
    }
}

impl ast::Expression {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        match self {
            Self::FalseLiteral => Ok(false.into()),
            Self::NullLiteral => Ok(graph::Value::Null.into()),
            Self::TrueLiteral => Ok(true.into()),
            Self::IntegerConstant(expr) => expr.evaluate_lazy(exec),
            Self::StringConstant(expr) => expr.evaluate_lazy(exec),
            Self::List(expr) => expr.evaluate_lazy(exec),
            Self::Set(expr) => expr.evaluate_lazy(exec),
            Self::Capture(expr) => expr.evaluate_lazy(exec),
            Self::Variable(expr) => expr.evaluate_lazy(exec),
            Self::Call(expr) => expr.evaluate_lazy(exec),
            Self::RegexCapture(expr) => expr.evaluate_lazy(exec),
        }
    }
}

impl ast::IntegerConstant {
    fn evaluate_lazy(&self, _exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        Ok(self.value.into())
    }
}

impl ast::StringConstant {
    fn evaluate_strict(
        &self,
        _exec: &mut ExecutionContext,
    ) -> Result<graph::Value, ExecutionError> {
        Ok(self.value.clone().into())
    }

    fn evaluate_lazy(&self, _exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        Ok(self.value.clone().into())
    }
}

impl ast::ListComprehension {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        let mut elements = Vec::new();
        for element in &self.elements {
            elements.push(element.evaluate_lazy(exec)?);
        }
        Ok(elements.into())
    }
}

impl ast::SetComprehension {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        let mut elements = Vec::new();
        for element in &self.elements {
            elements.push(element.evaluate_lazy(exec)?);
        }
        Ok(LazySet::new(elements).into())
    }
}

impl ast::Capture {
    fn evaluate_strict(&self, exec: &mut ExecutionContext) -> Result<graph::Value, ExecutionError> {
        Ok(query_capture_value(
            self.file_capture_index,
            self.quantifier,
            exec.mat,
            exec.graph,
        ))
    }

    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        Ok(query_capture_value(
            self.file_capture_index,
            self.quantifier,
            exec.mat,
            exec.graph,
        )
        .into())
    }
}

impl ast::Call {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        let mut parameters = Vec::new();
        for parameter in &self.parameters {
            parameters.push(parameter.evaluate_lazy(exec)?);
        }
        Ok(LazyCall::new(self.function, parameters).into())
    }
}

impl ast::RegexCapture {
    fn evaluate_strict(&self, exec: &mut ExecutionContext) -> Result<graph::Value, ExecutionError> {
        let value = exec.current_regex_captures[self.match_index].clone();
        Ok(value.into())
    }

    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        let value = exec.current_regex_captures[self.match_index].clone();
        Ok(value.into())
    }
}

impl ast::Variable {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        match self {
            Self::Scoped(variable) => variable.evaluate_lazy(exec),
            Self::Unscoped(variable) => variable.evaluate_lazy(exec),
        }
    }
}

impl ast::Variable {
    fn add_lazy(
        &self,
        exec: &mut ExecutionContext,
        value: LazyValue,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::Scoped(variable) => variable.add_lazy(exec, value, mutable),
            Self::Unscoped(variable) => variable.add_lazy(exec, value, mutable),
        }
    }

    fn set_lazy(
        &self,
        exec: &mut ExecutionContext,
        value: LazyValue,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::Scoped(variable) => variable.set_lazy(exec, value),
            Self::Unscoped(variable) => variable.set_lazy(exec, value),
        }
    }
}

impl ast::ScopedVariable {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        let scope = self.scope.evaluate_lazy(exec)?;
        let value = LazyScopedVariable::new(scope, self.name);
        Ok(value.into())
    }

    fn add_lazy(
        &self,
        exec: &mut ExecutionContext,
        value: LazyValue,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        if mutable {
            return Err(ExecutionError::CannotDefineMutableScopedVariable(format!(
                "{}",
                self.display_with(exec.ctx)
            )));
        }
        let scope = self.scope.evaluate_lazy(exec)?;
        let variable = exec
            .store
            .add(value, self.location.into(), exec.ctx, exec.graph);
        exec.scoped_store.add(
            scope,
            self.name,
            variable.into(),
            self.location.into(),
            exec.ctx,
        )
    }

    fn set_lazy(
        &self,
        exec: &mut ExecutionContext,
        _value: LazyValue,
    ) -> Result<(), ExecutionError> {
        Err(ExecutionError::CannotAssignScopedVariable(format!(
            "{}",
            self.display_with(exec.ctx)
        )))
    }
}

impl ast::UnscopedVariable {
    fn evaluate_strict(&self, exec: &mut ExecutionContext) -> Result<graph::Value, ExecutionError> {
        if let Some(value) = exec.globals.get(&self.name) {
            Ok(value.clone())
        } else {
            Err(ExecutionError::UndefinedVariable(format!(
                "global {}",
                self.display_with(exec.ctx)
            )))
        }
    }

    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyValue, ExecutionError> {
        if let Some(value) = exec.globals.get(&self.name) {
            Some(value.clone().into())
        } else {
            exec.locals.get(&self.name).map(|value| value.clone())
        }
        .ok_or_else(|| {
            ExecutionError::UndefinedVariable(format!("{}", self.display_with(exec.ctx)))
        })
    }
}

impl ast::UnscopedVariable {
    fn add_lazy(
        &self,
        exec: &mut ExecutionContext,
        value: LazyValue,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        if exec.globals.get(&self.name).is_some() {
            return Err(ExecutionError::DuplicateVariable(format!(
                " global {}",
                self.display_with(exec.ctx)
            )));
        }
        let value = exec
            .store
            .add(value, self.location.into(), exec.ctx, exec.graph);
        exec.locals
            .add(self.name, value.into(), mutable)
            .map_err(|_| {
                ExecutionError::DuplicateVariable(format!(" local {}", self.display_with(exec.ctx)))
            })
    }

    fn set_lazy(
        &self,
        exec: &mut ExecutionContext,
        value: LazyValue,
    ) -> Result<(), ExecutionError> {
        if exec.globals.get(&self.name).is_some() {
            return Err(ExecutionError::CannotAssignImmutableVariable(format!(
                " global {}",
                self.display_with(exec.ctx)
            )));
        }
        let value = exec
            .store
            .add(value, self.location.into(), exec.ctx, exec.graph);
        exec.locals.set(self.name, value.into()).map_err(|_| {
            if exec.locals.get(&self.name).is_some() {
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

impl ast::Attribute {
    fn evaluate_lazy(&self, exec: &mut ExecutionContext) -> Result<LazyAttribute, ExecutionError> {
        let value = self.value.evaluate_lazy(exec)?;
        let attribute = LazyAttribute::new(self.name, value);
        Ok(attribute)
    }
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
