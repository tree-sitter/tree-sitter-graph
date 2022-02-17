// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use tree_sitter::Tree;

#[derive(Debug, Default)]
pub(crate) struct ParseErrors {
    errors: Vec<(usize, usize, Option<String>)>,
}

impl ParseErrors {
    pub(crate) fn from_tree(tree: &Tree, source: &str) -> Self {
        let mut errors = ParseErrors { errors: Vec::new() };

        let mut cursor = tree.walk();
        let mut did_visit_children = false;
        loop {
            let node = cursor.node();
            if node.is_error() || node.is_missing() {
                errors.add(node, source);
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

        errors
    }

    fn add(&mut self, node: tree_sitter::Node, source: &str) {
        let start = node.start_position();
        let node_source = if !node.byte_range().is_empty() {
            Some(source[node.byte_range()].to_string())
        } else {
            None
        };
        self.errors.push((start.row, start.column, node_source));
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            write!(
                f,
                "Parse error on line {} column {} of input file",
                error.0 + 1,
                error.1 + 1
            )?;
            if let Some(source) = &error.2 {
                write!(f, ":\n|\n| {}\n|", source)?;
            } else {
                write!(f, " (node has no source)")?;
            }
        }
        Ok(())
    }
}
