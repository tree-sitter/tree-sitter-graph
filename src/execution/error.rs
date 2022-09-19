// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "term-colors")]
use colored::Colorize;
use std::path::Path;
use thiserror::Error;

use crate::execution::CancellationError;
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

#[derive(Debug)]
pub enum Context {
    Statement {
        statement: String,
        statement_location: Location,
        stanza_location: Location,
        source_location: Location,
        node_kind: String,
    },
    Other(String),
}

impl From<String> for Context {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Statement {
                statement,
                stanza_location,
                source_location,
                node_kind,
                ..
            } => write!(
                f,
                "Executing {} in stanza at {} matching ({}) node at {}",
                statement, stanza_location, node_kind, source_location
            ),
            Self::Other(msg) => write!(f, "{}", msg),
        }
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
                    Context::Statement {
                        statement,
                        statement_location,
                        stanza_location,
                        source_location,
                        node_kind,
                    } => {
                        writeln!(f, "{:>5}: Error executing statement {}", index, statement)?;
                        write!(
                            f,
                            "{}",
                            Excerpt::from_source(self.tsg_path, self.tsg, statement_location, 7)
                        )?;
                        writeln!(f, "{}in stanza", " ".repeat(7))?;
                        write!(
                            f,
                            "{}",
                            Excerpt::from_source(self.tsg_path, self.tsg, stanza_location, 7)
                        )?;
                        writeln!(f, "{}matching ({}) node", " ".repeat(7), node_kind)?;
                        write!(
                            f,
                            "{}",
                            Excerpt::from_source(self.source_path, self.source, source_location, 7)
                        )?;
                        Ok(())
                    }
                    Context::Other(msg) => writeln!(f, "{:>5}: {}", index, msg),
                }?;
                self.fmt_entry(f, index + 1, cause)
            }
            other => writeln!(f, "{:>5}: {}", index, other),
        }
    }
}

/// Excerpts of source from either the target language file or the tsg rules file.
struct Excerpt<'a> {
    path: &'a Path,
    source: Option<&'a str>,
    location: &'a Location,
    indent: usize,
}

impl<'a> Excerpt<'a> {
    pub fn from_source(
        path: &'a Path,
        source: &'a str,
        location: &'a Location,
        indent: usize,
    ) -> Excerpt<'a> {
        Excerpt {
            path,
            source: source.lines().nth(location.row),
            location,
            indent,
        }
    }

    fn gutter_width(&self) -> usize {
        ((self.location.row + 1) as f64).log10() as usize + 1
    }
}

impl<'a> std::fmt::Display for Excerpt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn blue<'a>(str: &'a str) -> impl std::fmt::Display {
            #[cfg(feature = "term-colors")]
            {
                str.blue().to_string()
            }
            #[cfg(not(feature = "term-colors"))]
            {
                str.to_string()
            }
        }
        fn green_bold<'a>(str: &'a str) -> impl std::fmt::Display {
            #[cfg(feature = "term-colors")]
            {
                str.green().bold().to_string()
            }
            #[cfg(not(feature = "term-colors"))]
            {
                str.to_string()
            }
        }
        fn white_bold<'a>(str: &'a str) -> impl std::fmt::Display {
            #[cfg(feature = "term-colors")]
            {
                str.white().bold()
            }
            #[cfg(not(feature = "term-colors"))]
            {
                str.to_string()
            }
        }

        // path and line/col
        writeln!(
            f,
            "{}{}:{}:{}:",
            " ".repeat(self.indent),
            white_bold(&self.path.to_str().unwrap_or("<unknown file>")),
            white_bold(&format!("{}", self.location.row + 1)),
            white_bold(&format!("{}", self.location.column + 1)),
        )?;
        // first line: line number & source
        writeln!(
            f,
            "{}{}{}{}",
            " ".repeat(self.indent),
            blue(&format!("{}", self.location.row + 1)),
            blue(" | "),
            self.source.unwrap_or("<no source found>"),
        )?;
        // second line: caret
        writeln!(
            f,
            "{}{}{}{}{}",
            " ".repeat(self.indent),
            " ".repeat(self.gutter_width()),
            blue(" | "),
            " ".repeat(self.location.column),
            green_bold("^")
        )?;
        Ok(())
    }
}
