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
    pub fn all(tree: &'tree Tree) -> Vec<ParseError> {
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

    pub fn display<'a: 'tree>(
        &'a self,
        source: &'tree str,
        verbose: bool,
    ) -> impl std::fmt::Display + 'a + 'tree {
        ParseErrorDisplay {
            error: self,
            source,
            verbose,
        }
    }
}

struct ParseErrorDisplay<'tree> {
    error: &'tree ParseError<'tree>,
    source: &'tree str,
    verbose: bool,
}

impl std::fmt::Display for ParseErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let node = match self.error {
            ParseError::Missing(node) => {
                write!(f, "Missing syntax")?;
                node
            }
            ParseError::Unexpected(node) => {
                write!(f, "Unexpected syntax")?;
                node
            }
        };
        let line = node.start_position().row;
        let start_column = node.start_position().column;
        if node.byte_range().is_empty() {
            writeln!(f, "")?;
        } else {
            let (end_column, end_byte) = self.source[node.byte_range()]
                .chars()
                .take_while(|c| *c != '\n')
                .fold(
                    (node.start_position().column, node.start_byte()),
                    |(column, byte), c| (column + 1, byte + c.len_utf8()),
                );
            if !self.verbose {
                let text = &self.source[node.start_byte()..end_byte];
                write!(f, ": {}", text)?;
            } else {
                let text = self
                    .source
                    .lines()
                    .nth(line)
                    .expect("parse error has invalid row");
                writeln!(f, ":")?;
                writeln!(f, "")?;
                writeln!(f, "| {}", text)?;
                write!(
                    f,
                    "  {}{}",
                    " ".repeat(start_column),
                    "^".repeat(end_column - start_column)
                )?;
                if node.end_position().row == line {
                    writeln!(f, "")?;
                } else {
                    writeln!(f, "...")?;
                }
            }
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
pub(crate) struct Excerpt<'a> {
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
        columns: Range<usize>,
        indent: usize,
    ) -> Excerpt<'a> {
        Excerpt {
            path,
            source: source.lines().nth(row),
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
            white_bold(&self.path.to_str().unwrap_or("<unknown file>")),
            white_bold(&format!("{}", self.row + 1)),
            white_bold(&format!("{}", self.columns.start + 1)),
        )?;
        if let Some(source) = self.source {
            // first line: line number & source
            writeln!(
                f,
                "{}{}{}{}",
                " ".repeat(self.indent),
                blue(&format!("{}", self.row + 1)),
                blue(" | "),
                source,
            )?;
            // second line: caret
            writeln!(
                f,
                "{}{}{}{}{}",
                " ".repeat(self.indent),
                " ".repeat(self.gutter_width()),
                blue(" | "),
                " ".repeat(self.columns.start),
                green_bold(&"^".repeat(self.columns.len()))
            )?;
        } else {
            writeln!(f, "{}{}", " ".repeat(self.indent), "<missing source>",)?;
        }
        Ok(())
    }
}

// coloring functions

#[cfg(feature = "term-colors")]
fn blue(str: &str) -> impl std::fmt::Display {
    str.blue()
}
#[cfg(not(feature = "term-colors"))]
fn blue<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}

#[cfg(feature = "term-colors")]
fn green_bold(str: &str) -> impl std::fmt::Display {
    str.green().bold()
}
#[cfg(not(feature = "term-colors"))]
fn green_bold<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}

#[cfg(feature = "term-colors")]
fn white_bold(str: &str) -> impl std::fmt::Display {
    str.white().bold()
}
#[cfg(not(feature = "term-colors"))]
fn white_bold<'a>(str: &'a str) -> impl std::fmt::Display + 'a {
    str
}
