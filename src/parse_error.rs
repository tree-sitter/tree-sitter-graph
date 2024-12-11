// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! Data types and functions for finding and displaying tree-sitter parse errors.

#[cfg(feature = "term-colors")]
use colored::Colorize;
use std::ops::Range;
use std::path::Path;
use tree_sitter::Node;
use tree_sitter::Tree;

/// Parse error for tree-sitter tree
#[derive(Debug)]
pub enum ParseError<'tree> {
    /// Error representing missing syntax
    Missing(Node<'tree>),
    /// Error representing unexpected syntax
    Unexpected(Node<'tree>),
}

impl<'tree> ParseError<'tree> {
    /// Return the first parse error in the given tree, if it exists.
    pub fn first(tree: &Tree) -> Option<ParseError> {
        let mut errors = Vec::new();
        find_errors(tree, &mut errors, true);
        errors.into_iter().next()
    }

    /// Return the tree and the first parse error in the given tree, if it exists.
    /// This returns a type, combining the tree and the error, that can be moved safely,
    /// which is not possible with a seperate tree and error.
    pub fn into_first(tree: Tree) -> TreeWithParseErrorOption {
        TreeWithParseErrorOption::into_first(tree)
    }

    /// Return all parse errors in the given tree.
    pub fn all(tree: &'tree Tree) -> Vec<ParseError<'tree>> {
        let mut errors = Vec::new();
        find_errors(tree, &mut errors, false);
        errors
    }

    /// Return the tree and all parse errors in the given tree.
    /// This returns a type, combining the tree and the errors, that can be moved safely,
    /// which is not possible with a seperate tree and errors.
    pub fn into_all(tree: Tree) -> TreeWithParseErrorVec {
        TreeWithParseErrorVec::into_all(tree)
    }
}

impl<'tree> ParseError<'tree> {
    pub fn node(&self) -> &Node<'tree> {
        match self {
            Self::Missing(node) => node,
            Self::Unexpected(node) => node,
        }
    }

    pub fn display(
        &'tree self,
        path: &'tree Path,
        source: &'tree str,
    ) -> impl std::fmt::Display + 'tree {
        ParseErrorDisplay {
            error: self,
            path,
            source,
        }
    }

    pub fn display_pretty(
        &'tree self,
        path: &'tree Path,
        source: &'tree str,
    ) -> impl std::fmt::Display + 'tree {
        ParseErrorDisplayPretty {
            error: self,
            path,
            source,
        }
    }
}

struct ParseErrorDisplay<'tree> {
    error: &'tree ParseError<'tree>,
    path: &'tree Path,
    source: &'tree str,
}

impl std::fmt::Display for ParseErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let node = self.error.node();
        write!(
            f,
            "{}:{}:{}: ",
            self.path.display(),
            node.start_position().row + 1,
            node.start_position().column + 1
        )?;
        let node = match self.error {
            ParseError::Missing(node) => {
                write!(f, "missing syntax")?;
                node
            }
            ParseError::Unexpected(node) => {
                write!(f, "unexpected syntax")?;
                node
            }
        };
        if node.byte_range().is_empty() {
            writeln!(f, "")?;
        } else {
            let end_byte = self.source[node.byte_range()]
                .chars()
                .take_while(|c| *c != '\n')
                .map(|c| c.len_utf8())
                .sum();
            let text = &self.source[node.start_byte()..end_byte];
            write!(f, ": {}", text)?;
        }
        Ok(())
    }
}

struct ParseErrorDisplayPretty<'tree> {
    error: &'tree ParseError<'tree>,
    path: &'tree Path,
    source: &'tree str,
}

impl std::fmt::Display for ParseErrorDisplayPretty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let node = match self.error {
            ParseError::Missing(node) => {
                writeln!(f, "missing syntax")?;
                node
            }
            ParseError::Unexpected(node) => {
                writeln!(f, "unexpected syntax")?;
                node
            }
        };
        if node.byte_range().is_empty() {
            writeln!(f, "")?;
        } else {
            let start_column = node.start_position().column;
            let end_column = node.start_position().column
                + self.source[node.byte_range()]
                    .chars()
                    .take_while(|c| *c != '\n')
                    .count();
            write!(
                f,
                "{}",
                Excerpt::from_source(
                    self.path,
                    self.source,
                    node.start_position().row,
                    start_column..end_column,
                    0,
                ),
            )?;
        }
        Ok(())
    }
}

/// Find errors in the given tree and add those to the given errors vector
fn find_errors<'tree>(tree: &'tree Tree, errors: &mut Vec<ParseError<'tree>>, first_only: bool) {
    // do not walk the tree unless there actually are errors
    if !tree.root_node().has_error() {
        return;
    }

    let mut cursor = tree.walk();
    let mut did_visit_children = false;
    loop {
        let node = cursor.node();
        if node.is_error() {
            errors.push(ParseError::Unexpected(node));
            if first_only {
                break;
            }
            did_visit_children = true;
        } else if node.is_missing() {
            errors.push(ParseError::Missing(node));
            if first_only {
                break;
            }
            did_visit_children = true;
        }
        if did_visit_children {
            if cursor.goto_next_sibling() {
                did_visit_children = false;
            } else if cursor.goto_parent() {
                did_visit_children = true;
            } else {
                break;
            }
        } else {
            if cursor.goto_first_child() {
                did_visit_children = false;
            } else {
                did_visit_children = true;
            }
        }
    }
    cursor.reset(tree.root_node());
}

// ------------------------------------------------------------------------------------------------
// Types to package a tree and parse errors for that tree
//
// Parse errors contain `Node` values, that are parametrized by the lifetime `tree of the tree they
// are part of. It is normally not possible to combine a value and references to that value in a single
// data type. However, in the case of tree-sitter trees and nodes, the nodes do not point to memory in the
// tree value, but both point to heap allocated memory. Therefore, moving the tree does not invalidate the
// node. We use this fact to implement the TreeWithParseError* types.
//
// To be able to use these types in errors, we implement Send and Sync. These traits are implemented for
// Tree, but not for Node. However, since the TreeWithParseError* types contain the tree as well as the nodes,
// it is okay to implement Send and Sync.

/// A type containing a tree and a parse error
pub struct TreeWithParseError {
    tree: Tree,
    // the 'static lifetime is okay because we own `tree`
    error: ParseError<'static>,
}

impl TreeWithParseError {
    pub fn tree(&self) -> &Tree {
        &self.tree
    }

    pub fn into_tree(self) -> Tree {
        self.tree
    }

    pub fn error(&self) -> &ParseError {
        &self.error
    }
}

impl std::fmt::Debug for TreeWithParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.error)
    }
}

// Send and Sync must be implemented for ParseError -> Node -> ffi::TSTree
// This is okay because Send and Sync _are_ implemented for Tree, which also holds ffi::TSTree
unsafe impl Send for TreeWithParseError {}
unsafe impl Sync for TreeWithParseError {}

/// A type containing a tree and an optional parse error
pub struct TreeWithParseErrorOption {
    tree: Tree,
    // the 'static lifetime is okay because we own `tree`
    error: Option<ParseError<'static>>,
}

impl TreeWithParseErrorOption {
    fn into_first(tree: Tree) -> TreeWithParseErrorOption {
        let mut errors = Vec::new();
        find_errors(&tree, &mut errors, true);
        Self {
            error: unsafe { std::mem::transmute(errors.into_iter().next()) },
            tree: tree,
        }
    }
}

impl TreeWithParseErrorOption {
    pub fn tree(&self) -> &Tree {
        &self.tree
    }

    pub fn into_tree(self) -> Tree {
        self.tree
    }

    pub fn error(&self) -> &Option<ParseError> {
        &self.error
    }

    pub fn into_option(self) -> Option<TreeWithParseError> {
        match self.error {
            None => None,
            Some(error) => Some(TreeWithParseError {
                tree: self.tree,
                error,
            }),
        }
    }
}

impl std::fmt::Debug for TreeWithParseErrorOption {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.error)
    }
}

// Send and Sync must be implemented for ParseError -> Node -> ffi::TSTree
// This is okay because Send and Sync _are_ implemented for Tree, which also holds ffi::TSTree
unsafe impl Send for TreeWithParseErrorOption {}
unsafe impl Sync for TreeWithParseErrorOption {}

/// A type containing a tree and parse errors
pub struct TreeWithParseErrorVec {
    tree: Tree,
    // the 'static lifetime is okay because we own `tree`
    errors: Vec<ParseError<'static>>,
}

impl TreeWithParseErrorVec {
    fn into_all(tree: Tree) -> TreeWithParseErrorVec {
        let mut errors = Vec::new();
        find_errors(&tree, &mut errors, false);
        TreeWithParseErrorVec {
            errors: unsafe { std::mem::transmute(errors) },
            tree: tree,
        }
    }
}

impl TreeWithParseErrorVec {
    pub fn tree(&self) -> &Tree {
        &self.tree
    }

    pub fn into_tree(self) -> Tree {
        self.tree
    }

    pub fn errors(&self) -> &Vec<ParseError> {
        &self.errors
    }
}

impl std::fmt::Debug for TreeWithParseErrorVec {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.errors)
    }
}

// Send and Sync must be implemented for ParseError -> Node -> ffi::TSTree
// This is okay because Send and Sync _are_ implemented for Tree, which also holds ffi::TSTree
unsafe impl Send for TreeWithParseErrorVec {}
unsafe impl Sync for TreeWithParseErrorVec {}

//-----------------------------------------------------------------------------

/// Excerpts of source from either the target language file or the tsg rules file.
pub struct Excerpt<'a> {
    path: &'a Path,
    source: Option<&'a str>,
    row: usize,
    columns: Range<usize>,
    indent: usize,
}

impl<'a> Excerpt<'a> {
    pub fn from_source(
        path: &'a Path,
        source: &'a str,
        row: usize,
        mut columns: Range<usize>,
        indent: usize,
    ) -> Excerpt<'a> {
        let source = source.lines().nth(row);
        columns.end = std::cmp::min(columns.end, source.map(|s| s.len()).unwrap_or_default());
        Excerpt {
            path,
            source,
            row,
            columns,
            indent,
        }
    }

    fn gutter_width(&self) -> usize {
        ((self.row + 1) as f64).log10() as usize + 1
    }
}

impl<'a> std::fmt::Display for Excerpt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // path and line/col
        writeln!(
            f,
            "{}{}:{}:{}:",
            " ".repeat(self.indent),
            header_style(&self.path.to_string_lossy()),
            header_style(&format!("{}", self.row + 1)),
            header_style(&format!("{}", self.columns.start + 1)),
        )?;
        if let Some(source) = self.source {
            // first line: line number & source
            writeln!(
                f,
                "{}{}{}{}",
                " ".repeat(self.indent),
                prefix_style(&format!("{}", self.row + 1)),
                prefix_style(" | "),
                source,
            )?;
            // second line: caret
            writeln!(
                f,
                "{}{}{}{}{}",
                " ".repeat(self.indent),
                " ".repeat(self.gutter_width()),
                prefix_style(" | "),
                " ".repeat(self.columns.start),
                underline_style(&"^".repeat(self.columns.len())),
            )?;
        } else {
            writeln!(f, "{}{}", " ".repeat(self.indent), "<missing source>",)?;
        }
        Ok(())
    }
}

// coloring functions

#[cfg(feature = "term-colors")]
fn header_style(str: &str) -> impl std::fmt::Display {
    str.bold()
}
#[cfg(not(feature = "term-colors"))]
fn header_style<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}

#[cfg(feature = "term-colors")]
fn prefix_style(str: &str) -> impl std::fmt::Display {
    str.dimmed()
}
#[cfg(not(feature = "term-colors"))]
fn prefix_style<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}

#[cfg(feature = "term-colors")]
fn underline_style(str: &str) -> impl std::fmt::Display {
    str.green()
}
#[cfg(not(feature = "term-colors"))]
fn underline_style<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}
