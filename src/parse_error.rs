// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use tree_sitter::Node;
use tree_sitter::Tree;

/// Parse errors for tree-sitter parse tree
#[derive(Debug)]
pub enum ParseError<'tree> {
    /// Error representing missing syntax
    Missing(Node<'tree>),
    /// Error representing unexpected syntax
    Unexpected(Node<'tree>),
}

impl<'tree> ParseError<'tree> {
    pub fn find_first(tree: &'tree Tree) -> Option<ParseError> {
        let mut errors = Vec::new();
        Self::find_into(tree, &mut errors, true);
        errors.into_iter().next()
    }

    pub fn find_all(tree: &'tree Tree) -> Vec<ParseError> {
        let mut errors = Vec::new();
        Self::find_into(tree, &mut errors, true);
        errors
    }

    fn find_into(tree: &'tree Tree, errors: &mut Vec<ParseError<'tree>>, first_only: bool) {
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
}

impl<'tree> ParseError<'tree> {
    pub fn display(&'tree self, source: &'tree str, verbose: bool) -> ParseErrorDisplay<'tree> {
        ParseErrorDisplay {
            error: self,
            source,
            verbose,
        }
    }
}

pub struct ParseErrorDisplay<'tree> {
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
        write!(f, " on line {} column {}", line + 1, start_column + 1)?;
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
                let text = self.source.lines().nth(line).unwrap();
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
