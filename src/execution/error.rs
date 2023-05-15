// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::path::Path;
use thiserror::Error;

use crate::ast::Stanza;
use crate::ast::Statement;
use crate::execution::CancellationError;
use crate::parse_error::Excerpt;
use crate::Location;

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
    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
    #[error("Cannot add scoped variable after being forced {0}")]
    VariableScopesAlreadyForced(String),
    #[error("Function {0} failed: {1}")]
    FunctionFailed(String, String),
    #[error("{0}. Caused by: {1}")]
    InContext(Context, Box<ExecutionError>),
}

#[derive(Clone, Debug)]
pub enum Context {
    Statement(Vec<StatementContext>),
    Other(String),
}

#[derive(Clone, Debug)]
pub struct StatementContext {
    pub statement: String,
    pub statement_location: Location,
    pub stanza_location: Location,
    pub source_location: Location,
    pub node_kind: String,
}

impl StatementContext {
    pub(crate) fn new(stmt: &Statement, stanza: &Stanza, source_node: &tree_sitter::Node) -> Self {
        Self {
            statement: format!("{}", stmt),
            statement_location: stmt.location(),
            stanza_location: stanza.range.start,
            source_location: Location::from(source_node.range().start_point),
            node_kind: source_node.kind().to_string(),
        }
    }

    pub(crate) fn update_statement(&mut self, stmt: &Statement) {
        self.statement = format!("{}", stmt);
        self.statement_location = stmt.location();
    }
}

impl From<StatementContext> for Context {
    fn from(value: StatementContext) -> Self {
        Self::Statement(vec![value])
    }
}

impl From<(StatementContext, StatementContext)> for Context {
    fn from((left, right): (StatementContext, StatementContext)) -> Self {
        Self::Statement(vec![left, right])
    }
}

impl From<String> for Context {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Statement(stmts) => {
                let mut first = true;
                for stmt in stmts {
                    stmt.fmt(f, first)?;
                    first = false;
                }
            }
            Self::Other(msg) => write!(f, "{}", msg)?,
        }
        Ok(())
    }
}

impl StatementContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, first: bool) -> std::fmt::Result {
        if first {
            write!(f, "Error executing",)?;
        } else {
            write!(f, " and executing",)?;
        }
        write!(
            f,
            " {} in stanza at {} matching ({}) node at {}",
            self.statement, self.stanza_location, self.node_kind, self.source_location
        )?;
        Ok(())
    }
}

pub(super) trait ResultWithExecutionError<R> {
    fn with_context<F>(self, with_context: F) -> Result<R, ExecutionError>
    where
        F: FnOnce() -> Context;
}

impl<R> ResultWithExecutionError<R> for Result<R, ExecutionError> {
    fn with_context<F>(self, with_context: F) -> Result<R, ExecutionError>
    where
        F: FnOnce() -> Context,
    {
        self.map_err(|e| match e {
            cancelled @ ExecutionError::Cancelled(_) => cancelled,
            in_other_context @ ExecutionError::InContext(Context::Other(_), _) => {
                ExecutionError::InContext(with_context(), Box::new(in_other_context))
            }
            in_stmt_context @ ExecutionError::InContext(_, _) => in_stmt_context,
            _ => ExecutionError::InContext(with_context(), Box::new(e)),
        })
    }
}

impl ExecutionError {
    pub fn display_pretty<'a>(
        &'a self,
        source_path: &'a Path,
        source: &'a str,
        tsg_path: &'a Path,
        tsg: &'a str,
    ) -> impl std::fmt::Display + 'a {
        DisplayExecutionErrorPretty {
            error: self,
            source_path,
            source,
            tsg_path,
            tsg,
        }
    }
}

struct DisplayExecutionErrorPretty<'a> {
    error: &'a ExecutionError,
    source_path: &'a Path,
    source: &'a str,
    tsg_path: &'a Path,
    tsg: &'a str,
}

impl std::fmt::Display for DisplayExecutionErrorPretty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_entry(f, 0, self.error)
    }
}

impl DisplayExecutionErrorPretty<'_> {
    fn fmt_entry(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        index: usize,
        error: &ExecutionError,
    ) -> std::fmt::Result {
        match error {
            ExecutionError::InContext(context, cause) => {
                match context {
                    Context::Statement(stmts) => {
                        let mut first = true;
                        for stmt in stmts {
                            stmt.fmt_pretty(
                                f,
                                self.source_path,
                                self.source,
                                self.tsg_path,
                                self.tsg,
                                index,
                                first,
                            )?;
                            first = false;
                        }
                    }
                    Context::Other(msg) => writeln!(f, "{:>5}: {}", index, msg)?,
                };
                self.fmt_entry(f, index + 1, cause)?;
                Ok(())
            }
            other => writeln!(f, "{:>5}: {}", index, other),
        }
    }
}

impl StatementContext {
    fn fmt_pretty(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source_path: &Path,
        source: &str,
        tsg_path: &Path,
        tsg: &str,
        index: usize,
        first: bool,
    ) -> std::fmt::Result {
        if first {
            writeln!(
                f,
                "{:>5}: Error executing statement {}",
                index, self.statement
            )?;
        } else {
            writeln!(f, "     > and executing statement {}", self.statement)?;
        }
        write!(
            f,
            "{}",
            Excerpt::from_source(
                tsg_path,
                tsg,
                self.statement_location.row,
                self.statement_location.to_column_range(),
                7
            )
        )?;
        writeln!(f, "{}in stanza", " ".repeat(7))?;
        write!(
            f,
            "{}",
            Excerpt::from_source(
                tsg_path,
                tsg,
                self.stanza_location.row,
                self.stanza_location.to_column_range(),
                7
            )
        )?;
        writeln!(f, "{}matching ({}) node", " ".repeat(7), self.node_kind)?;
        write!(
            f,
            "{}",
            Excerpt::from_source(
                source_path,
                source,
                self.source_location.row,
                self.source_location.to_column_range(),
                7
            )
        )?;
        Ok(())
    }
}
