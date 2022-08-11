// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use anyhow::Context as ErrorContext;
use std::collections::BTreeSet;
use std::collections::HashMap;
use thiserror::Error;
use tree_sitter::CaptureQuantifier;
use tree_sitter::QueryCursor;
use tree_sitter::QueryMatch;
use tree_sitter::Tree;

use crate::ast::AddEdgeAttribute;
use crate::ast::AddGraphNodeAttribute;
use crate::ast::Assign;
use crate::ast::Attribute;
use crate::ast::AttributeShorthand;
use crate::ast::AttributeShorthands;
use crate::ast::Call;
use crate::ast::Capture;
use crate::ast::Condition;
use crate::ast::CreateEdge;
use crate::ast::CreateGraphNode;
use crate::ast::DeclareImmutable;
use crate::ast::DeclareMutable;
use crate::ast::Expression;
use crate::ast::File;
use crate::ast::ForIn;
use crate::ast::If;
use crate::ast::IntegerConstant;
use crate::ast::ListComprehension;
use crate::ast::ListLiteral;
use crate::ast::Print;
use crate::ast::RegexCapture;
use crate::ast::Scan;
use crate::ast::ScopedVariable;
use crate::ast::SetComprehension;
use crate::ast::SetLiteral;
use crate::ast::Stanza;
use crate::ast::Statement;
use crate::ast::StringConstant;
use crate::ast::UnscopedVariable;
use crate::ast::Variable;
use crate::functions::Functions;
use crate::graph::Attributes;
use crate::graph::Graph;
use crate::graph::SyntaxNodeRef;
use crate::graph::Value;
use crate::variables::Globals;
use crate::variables::VariableMap;
use crate::variables::Variables;
use crate::Identifier;
use crate::Location;

impl File {
    /// Executes this graph DSL file against a source file.  You must provide the parsed syntax
    /// tree (`tree`) as well as the source text that it was parsed from (`source`).  You also
    /// provide the set of functions and global variables that are available during execution.
    pub fn execute<'a, 'tree>(
        &self,
        tree: &'tree Tree,
        source: &'tree str,
        tsg_source: &'a str,
        config: &mut ExecutionConfig,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<Graph<'tree>, ExecutionError> {
        let mut graph = Graph::new();
        self.execute_into(
            &mut graph,
            tree,
            source,
            tsg_source,
            config,
            cancellation_flag,
        )?;
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
        tsg_source: &'a str,
        config: &mut ExecutionConfig,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<(), ExecutionError> {
        if config.lazy {
            self.execute_lazy_into(graph, tree, source, tsg_source, config, cancellation_flag)
        } else {
            self.execute_strict_into(graph, tree, source, tsg_source, config, cancellation_flag)
        }
    }
}

impl File {
    /// Executes this graph DSL file against a source file, saving the results into an existing
    /// `Graph` instance.  You must provide the parsed syntax tree (`tree`) as well as the source
    /// text that it was parsed from (`source`).  You also provide the set of functions and global
    /// variables that are available during execution. This variant is useful when you need to
    /// “pre-seed” the graph with some predefined nodes and/or edges before executing the DSL file.
    fn execute_strict_into<'a, 'tree>(
        &self,
        graph: &mut Graph<'tree>,
        tree: &'tree Tree,
        source: &'tree str,
        tsg_source: &'a str,
        config: &mut ExecutionConfig,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<(), ExecutionError> {
        self.check_globals(config.globals)?;
        let mut locals = VariableMap::new();
        let mut scoped = ScopedVariables::new();
        let current_regex_captures = Vec::new();
        let mut function_parameters = Vec::new();
        let mut cursor = QueryCursor::new();
        for stanza in &self.stanzas {
            cancellation_flag.check("executing stanza")?;
            stanza.execute(
                tree,
                source,
                tsg_source,
                graph,
                config,
                &mut locals,
                &mut scoped,
                &current_regex_captures,
                &mut function_parameters,
                &mut cursor,
                &self.shorthands,
                cancellation_flag,
            )?;
        }
        Ok(())
    }

    pub(crate) fn check_globals(&self, globals: &Globals) -> Result<(), ExecutionError> {
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

/// An error that can occur while executing a graph DSL file
#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error(transparent)]
    Cancelled(#[from] CancellationError),
    #[error("Cannot assign immutable variable {0}")]
    CannotAssignImmutableVariable(String),
    #[error("Cannot assign scoped variable {0}")]
    CannotAssignScopedVariable(String),
    #[error("Cannot define mutable scoped variable {0}")]
    CannotDefineMutableScopedVariable(String),
    #[error("Duplicate attribute {0}")]
    DuplicateAttribute(String),
    #[error("Duplicate edge {0}")]
    DuplicateEdge(String),
    #[error("Duplicate variable {0} at {1}")]
    DuplicateVariable(String, Location),
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
    #[error("Missing global variable {0}")]
    MissingGlobalVariable(String),
    #[error("Recursively defined scoped variable {0}")]
    RecursivelyDefinedScopedVariable(String),
    #[error("Recursively defined variable {0}")]
    RecursivelyDefinedVariable(String),
    #[error("Undefined capture {0}")]
    UndefinedCapture(String),
    #[error("Undefined function {0}")]
    UndefinedFunction(String),
    #[error("Undefined regex capture {0}")]
    UndefinedRegexCapture(String),
    #[error("Undefined scoped variable {0}")]
    UndefinedScopedVariable(String),
    #[error("Empty regex capture {0}")]
    EmptyRegexCapture(String),
    #[error("Undefined edge {0}")]
    UndefinedEdge(String),
    #[error("Undefined variable {0} at {1}")]
    UndefinedVariable(String, Location),
    #[error("Cannot add scoped variable after being forced {0}")]
    VariableScopesAlreadyForced(String),
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

/// State that is threaded through the execution
struct ExecutionContext<'a, 'c, 'g, 's, 'tree> {
    source: &'tree str,
    tsg_source: &'a str,
    graph: &'a mut Graph<'tree>,
    config: &'a mut ExecutionConfig<'c, 'g>,
    locals: &'a mut dyn Variables<Value>,
    scoped: &'a mut ScopedVariables<'s>,
    current_regex_captures: &'a Vec<String>,
    function_parameters: &'a mut Vec<Value>,
    mat: &'a QueryMatch<'a, 'tree>,
    shorthands: &'a AttributeShorthands,
    cancellation_flag: &'a dyn CancellationFlag,
}

struct ScopedVariables<'a> {
    scopes: HashMap<SyntaxNodeRef, VariableMap<'a, Value>>,
}

impl<'a> ScopedVariables<'a> {
    fn new() -> Self {
        Self {
            scopes: HashMap::new(),
        }
    }

    fn get(&mut self, scope: SyntaxNodeRef) -> &mut VariableMap<'a, Value> {
        self.scopes.entry(scope).or_insert(VariableMap::new())
    }
}

/// Excerpts of source from either the target language file or the tsg rules file.
struct Excerpt<'a> {
    source: Option<&'a str>,
    location: Location,
}

impl<'a> Excerpt<'a> {
    fn from_source(source: &'a str, location: Location) -> Excerpt {
        Excerpt {
            source: source.lines().nth(location.row),
            location: location,
        }
    }
}

impl<'a> std::fmt::Display for Excerpt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} | {}",
            self.location.row + 1,
            self.source.unwrap_or("<no source found>")
        )
    }
}

impl Stanza {
    fn execute<'a, 'g, 'l, 's, 'tree>(
        &self,
        tree: &'tree Tree,
        source: &'tree str,
        tsg_source: &'a str,
        graph: &mut Graph<'tree>,
        config: &mut ExecutionConfig<'_, 'g>,
        locals: &mut VariableMap<'l, Value>,
        scoped: &mut ScopedVariables<'s>,
        current_regex_captures: &Vec<String>,
        function_parameters: &mut Vec<Value>,
        cursor: &mut QueryCursor,
        shorthands: &AttributeShorthands,
        cancellation_flag: &dyn CancellationFlag,
    ) -> Result<(), ExecutionError> {
        let matches = cursor.matches(&self.query, tree.root_node(), source.as_bytes());
        for mat in matches {
            cancellation_flag.check("processing matches")?;
            locals.clear();
            let mut exec = ExecutionContext {
                source,
                tsg_source,
                graph,
                config,
                locals,
                scoped,
                current_regex_captures,
                function_parameters,
                mat: &mat,
                shorthands: shorthands,
                cancellation_flag,
            };
            for statement in &self.statements {
                statement.execute(&mut exec).or_else(|e| {
                    Err(e).with_context(|| {
                        format!(
                            "Executing {}\n{}",
                            statement,
                            Excerpt::from_source(tsg_source, statement.location()) // FIXME: add the statement excerpt to the leaf error instead
                        )
                    })
                })?;
            }
        }
        Ok(())
    }
}

impl Statement {
    fn location(&self) -> Location {
        match self {
            Statement::DeclareImmutable(s) => s.location,
            Statement::DeclareMutable(s) => s.location,
            Statement::Assign(s) => s.location,
            Statement::CreateGraphNode(s) => s.location,
            Statement::AddGraphNodeAttribute(s) => s.location,
            Statement::CreateEdge(s) => s.location,
            Statement::AddEdgeAttribute(s) => s.location,
            Statement::Scan(s) => s.location,
            Statement::Print(s) => s.location,
            Statement::If(s) => s.location,
            Statement::ForIn(s) => s.location,
        }
    }

    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        exec.cancellation_flag.check("executing statement")?;
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
            Statement::If(statement) => statement.execute(exec),
            Statement::ForIn(statement) => statement.execute(exec),
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
        self.node
            .add_debug_attrs(&mut exec.graph[graph_node].attributes, exec.config)?;
        let value = Value::GraphNode(graph_node);
        self.node.add(exec, value, false)
    }
}

impl AddGraphNodeAttribute {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let node = self.node.evaluate(exec)?.into_graph_node_ref()?;
        let add_attribute = |exec: &mut ExecutionContext, name: Identifier, value: Value| {
            exec.graph[node]
                .attributes
                .add(name.clone(), value)
                .map_err(|_| {
                    ExecutionError::DuplicateAttribute(format!(
                        " {} on graph node ({}) in {}",
                        name, node, self,
                    ))
                })
        };
        for attribute in &self.attributes {
            attribute.execute(exec, &add_attribute)?;
        }
        Ok(())
    }
}

impl CreateEdge {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate(exec)?.into_graph_node_ref()?;
        let sink = self.sink.evaluate(exec)?.into_graph_node_ref()?;
        if let Err(_) = exec.graph[source].add_edge(sink) {
            Err(ExecutionError::DuplicateEdge(format!(
                "({} -> {}) in {}",
                source, sink, self,
            )))?;
        }
        Ok(())
    }
}

impl AddEdgeAttribute {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let source = self.source.evaluate(exec)?.into_graph_node_ref()?;
        let sink = self.sink.evaluate(exec)?.into_graph_node_ref()?;
        let add_attribute = |exec: &mut ExecutionContext, name: Identifier, value: Value| {
            let edge = match exec.graph[source].get_edge_mut(sink) {
                Some(edge) => Ok(edge),
                None => Err(ExecutionError::UndefinedEdge(format!(
                    "({} -> {}) in {}",
                    source, sink, self,
                ))),
            }?;
            edge.attributes.add(name.clone(), value).map_err(|_| {
                ExecutionError::DuplicateAttribute(format!(
                    " {} on edge ({} -> {}) in {}",
                    name, source, sink, self,
                ))
            })
        };
        for attribute in &self.attributes {
            attribute.execute(exec, &add_attribute)?;
        }
        Ok(())
    }
}

impl Scan {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let match_string = self.value.evaluate(exec)?.into_string()?;

        let mut i = 0;
        let mut matches = Vec::new();
        while i < match_string.len() {
            exec.cancellation_flag.check("processing scan matches")?;
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

            let mut arm_locals = VariableMap::nested(exec.locals);
            let mut arm_exec = ExecutionContext {
                source: exec.source,
                tsg_source: exec.tsg_source,
                graph: exec.graph,
                config: exec.config,
                locals: &mut arm_locals,
                scoped: exec.scoped,
                current_regex_captures: &current_regex_captures,
                function_parameters: exec.function_parameters,
                mat: exec.mat,
                shorthands: exec.shorthands,
                cancellation_flag: exec.cancellation_flag,
            };

            for statement in &arm.statements {
                statement
                    .execute(&mut arm_exec)
                    .with_context(|| format!("Executing {}", statement))
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
                eprint!("{}", value);
            }
        }
        eprintln!();
        Ok(())
    }
}

impl If {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        for arm in &self.arms {
            let mut result = true;
            for condition in &arm.conditions {
                result &= condition.test(exec)?;
            }
            if result {
                let mut arm_locals = VariableMap::nested(exec.locals);
                let mut arm_exec = ExecutionContext {
                    source: exec.source,
                    tsg_source: exec.tsg_source,
                    graph: exec.graph,
                    config: exec.config,
                    locals: &mut arm_locals,
                    scoped: exec.scoped,
                    current_regex_captures: exec.current_regex_captures,
                    function_parameters: exec.function_parameters,
                    mat: exec.mat,
                    shorthands: exec.shorthands,
                    cancellation_flag: exec.cancellation_flag,
                };
                for stmt in &arm.statements {
                    stmt.execute(&mut arm_exec)?;
                }
                break;
            }
        }
        Ok(())
    }
}

impl Condition {
    fn test(&self, exec: &mut ExecutionContext) -> Result<bool, ExecutionError> {
        match self {
            Condition::Some { value, .. } => Ok(!value.evaluate(exec)?.is_null()),
            Condition::None { value, .. } => Ok(value.evaluate(exec)?.is_null()),
            Condition::Bool { value, .. } => Ok(value.evaluate(exec)?.into_boolean()?),
        }
    }
}

impl ForIn {
    fn execute(&self, exec: &mut ExecutionContext) -> Result<(), ExecutionError> {
        let values = self.value.evaluate(exec)?.into_list()?;
        let mut loop_locals = VariableMap::nested(exec.locals);
        for value in values {
            loop_locals.clear();
            let mut loop_exec = ExecutionContext {
                source: exec.source,
                tsg_source: exec.tsg_source,
                graph: exec.graph,
                config: exec.config,
                locals: &mut loop_locals,
                scoped: exec.scoped,
                current_regex_captures: exec.current_regex_captures,
                function_parameters: exec.function_parameters,
                mat: exec.mat,
                shorthands: exec.shorthands,
                cancellation_flag: exec.cancellation_flag,
            };
            self.variable.add(&mut loop_exec, value, false)?;
            for stmt in &self.statements {
                stmt.execute(&mut loop_exec)?;
            }
        }
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
            Expression::ListLiteral(expr) => expr.evaluate(exec),
            Expression::SetLiteral(expr) => expr.evaluate(exec),
            Expression::ListComprehension(expr) => expr.evaluate(exec),
            Expression::SetComprehension(expr) => expr.evaluate(exec),
            Expression::Capture(expr) => expr.evaluate(exec),
            Expression::Variable(expr) => expr.evaluate(exec),
            Expression::Call(expr) => expr.evaluate(exec),
            Expression::RegexCapture(expr) => expr.evaluate(exec),
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

impl ListLiteral {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::List(elements))
    }
}

impl ListComprehension {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let values = self.value.evaluate(exec)?.into_list()?;
        let mut elements = Vec::new();
        let mut loop_locals = VariableMap::nested(exec.locals);
        for value in values {
            loop_locals.clear();
            let mut loop_exec = ExecutionContext {
                source: exec.source,
                tsg_source: exec.tsg_source,
                graph: exec.graph,
                config: exec.config,
                locals: &mut loop_locals,
                scoped: exec.scoped,
                current_regex_captures: exec.current_regex_captures,
                function_parameters: exec.function_parameters,
                mat: exec.mat,
                shorthands: exec.shorthands,
                cancellation_flag: exec.cancellation_flag,
            };
            self.variable.add(&mut loop_exec, value, false)?;
            let element = self.element.evaluate(&mut loop_exec)?;
            elements.push(element);
        }
        Ok(Value::List(elements))
    }
}

impl SetLiteral {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.evaluate(exec))
            .collect::<Result<_, _>>()?;
        Ok(Value::Set(elements))
    }
}

impl SetComprehension {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        let values = self.value.evaluate(exec)?.into_list()?;
        let mut elements = BTreeSet::new();
        let mut loop_locals = VariableMap::nested(exec.locals);
        for value in values {
            loop_locals.clear();
            let mut loop_exec = ExecutionContext {
                source: exec.source,
                tsg_source: exec.tsg_source,
                graph: exec.graph,
                config: exec.config,
                locals: &mut loop_locals,
                scoped: exec.scoped,
                current_regex_captures: exec.current_regex_captures,
                function_parameters: exec.function_parameters,
                mat: exec.mat,
                shorthands: exec.shorthands,
                cancellation_flag: exec.cancellation_flag,
            };
            self.variable.add(&mut loop_exec, value, false)?;
            let element = self.element.evaluate(&mut loop_exec)?;
            elements.insert(element);
        }
        Ok(Value::Set(elements))
    }
}

impl Capture {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        Ok(query_capture_value(
            self.stanza_capture_index,
            self.quantifier,
            exec.mat,
            exec.graph,
        ))
    }
}

impl Call {
    fn evaluate(&self, exec: &mut ExecutionContext) -> Result<Value, ExecutionError> {
        for parameter in &self.parameters {
            let parameter = parameter.evaluate(exec)?;
            exec.function_parameters.push(parameter);
        }
        exec.config.functions.call(
            &self.function,
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
        let capture = exec
            .current_regex_captures
            .get(self.match_index)
            .ok_or(ExecutionError::UndefinedRegexCapture(format!("{}", self)))?;
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
                    "got {}",
                    scope
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        if let Some(value) = variables.get(&self.name) {
            Ok(value)
        } else {
            Err(ExecutionError::UndefinedVariable(
                format!("{} on node {}", self, scope,),
                scope.location(),
            ))
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
                    "got {}",
                    scope
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        variables
            .add(self.name.clone(), value, mutable)
            .map_err(|_| ExecutionError::DuplicateVariable(format!("{}", self), self.location))
    }

    fn set(&self, exec: &mut ExecutionContext, value: Value) -> Result<(), ExecutionError> {
        let scope = self.scope.evaluate(exec)?;
        let scope = match scope {
            Value::SyntaxNode(scope) => scope,
            _ => {
                return Err(ExecutionError::InvalidVariableScope(format!(
                    "got {}",
                    scope,
                )))
            }
        };
        let variables = exec.scoped.get(scope);
        variables
            .set(self.name.clone(), value)
            .map_err(|_| ExecutionError::DuplicateVariable(format!("{}", self), self.location))
    }
}

impl UnscopedVariable {
    fn get<'a>(&self, exec: &'a mut ExecutionContext) -> Result<&'a Value, ExecutionError> {
        if let Some(value) = exec.config.globals.get(&self.name) {
            Some(value)
        } else {
            exec.locals.get(&self.name)
        }
        .ok_or_else(|| ExecutionError::UndefinedVariable(format!("{}", self), self.location))
    }

    fn add(
        &self,
        exec: &mut ExecutionContext,
        value: Value,
        mutable: bool,
    ) -> Result<(), ExecutionError> {
        if exec.config.globals.get(&self.name).is_some() {
            return Err(ExecutionError::DuplicateVariable(
                format!(" global {}", self,),
                self.location,
            ));
        }
        exec.locals
            .add(self.name.clone(), value, mutable)
            .map_err(|_| {
                ExecutionError::DuplicateVariable(format!(" local {}", self), self.location)
            })
    }

    fn set(&self, exec: &mut ExecutionContext, value: Value) -> Result<(), ExecutionError> {
        if exec.config.globals.get(&self.name).is_some() {
            return Err(ExecutionError::CannotAssignImmutableVariable(format!(
                " global {}",
                self,
            )));
        }
        exec.locals.set(self.name.clone(), value).map_err(|_| {
            if exec.locals.get(&self.name).is_some() {
                ExecutionError::CannotAssignImmutableVariable(format!("{}", self))
            } else {
                ExecutionError::UndefinedVariable(format!("{}", self), self.location)
            }
        })
    }
}

impl Variable {
    pub(crate) fn add_debug_attrs(
        &self,
        attributes: &mut Attributes,
        config: &ExecutionConfig,
    ) -> Result<(), ExecutionError> {
        if let Some(variable_name_attr) = &config.variable_name_attr {
            attributes
                .add(variable_name_attr.clone(), format!("{}", self))
                .map_err(|_| {
                    ExecutionError::DuplicateAttribute(variable_name_attr.as_str().into())
                })?;
        }
        if let Some(location_attr) = &config.location_attr {
            let location = match &self {
                Variable::Scoped(v) => v.location,
                Variable::Unscoped(v) => v.location,
            };
            attributes
                .add(location_attr.clone(), format!("{}", location))
                .map_err(|_| ExecutionError::DuplicateAttribute(location_attr.as_str().into()))?;
        }
        Ok(())
    }
}

impl Attribute {
    fn execute<F>(
        &self,
        exec: &mut ExecutionContext,
        add_attribute: &F,
    ) -> Result<(), ExecutionError>
    where
        F: Fn(&mut ExecutionContext, Identifier, Value) -> Result<(), ExecutionError>,
    {
        exec.cancellation_flag.check("executing attribute")?;
        let value = self.value.evaluate(exec)?;
        if let Some(shorthand) = exec.shorthands.get(&self.name) {
            shorthand.execute(exec, add_attribute, value)
        } else {
            add_attribute(exec, self.name.clone(), value)
        }
    }
}

impl AttributeShorthand {
    fn execute<F>(
        &self,
        exec: &mut ExecutionContext,
        add_attribute: &F,
        value: Value,
    ) -> Result<(), ExecutionError>
    where
        F: Fn(&mut ExecutionContext, Identifier, Value) -> Result<(), ExecutionError>,
    {
        let mut shorthand_locals = VariableMap::new();
        let mut shorthand_exec = ExecutionContext {
            source: exec.source,
            tsg_source: exec.tsg_source,
            graph: exec.graph,
            config: exec.config,
            locals: &mut shorthand_locals,
            scoped: exec.scoped,
            current_regex_captures: exec.current_regex_captures,
            function_parameters: exec.function_parameters,
            mat: exec.mat,
            shorthands: exec.shorthands,
            cancellation_flag: exec.cancellation_flag,
        };
        self.variable.add(&mut shorthand_exec, value, false)?;
        for attr in &self.attributes {
            attr.execute(&mut shorthand_exec, add_attribute)?;
        }
        Ok(())
    }
}

/// Get the value for the given capture, considering the suffix
pub(crate) fn query_capture_value<'tree>(
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
