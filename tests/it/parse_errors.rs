// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use indoc::indoc;
use tree_sitter::Parser;
use tree_sitter::Point;
use tree_sitter::Tree;
use tree_sitter_graph::parse_error::ParseError;

fn init_log() {
    let _ = env_logger::builder()
        .is_test(true)
        .format_level(false)
        .format_target(false)
        .format_timestamp(None)
        .try_init(); // try, because earlier test may have already initialized it
}

fn parse(python_source: &str) -> Tree {
    init_log();
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_python::language()).unwrap();
    parser.parse(python_source, None).unwrap()
}

#[test]
fn can_find_error() {
    let tree = parse(indoc! {r#"
        def f():
            a 42
        def g():
            pass
    "#});
    let parse_error = ParseError::first(&tree);
    let position = parse_error.map(|e| e.node().start_position());
    assert_eq!(position, Some(Point::new(1, 4)));
}

#[test]
fn can_find_errors() {
    let tree = parse(indoc! {r#"
        def f():
            a 42
        def g():
            b 11
    "#});
    let parse_errors = ParseError::all(&tree);
    let positions = parse_errors
        .into_iter()
        .map(|e| e.node().start_position())
        .collect::<Vec<_>>();
    assert_eq!(positions, vec![Point::new(1, 4), Point::new(3, 4)]);
}

#[test]
fn can_move_tree_with_error() {
    let tree = parse(indoc! {r#"
        def f():
            a 42
        def g():
            pass
    "#});
    let parse_error = ParseError::into_first(tree);
    let moved_parse_error = parse_error;
    let position = moved_parse_error
        .error()
        .as_ref()
        .map(|e| e.node().start_position());
    assert_eq!(position, Some(Point::new(1, 4)));
    let _recovered_tree = moved_parse_error.into_tree();
}

#[test]
fn can_move_tree_with_errors() {
    let tree = parse(indoc! {r#"
        def f():
            a 42
        def g():
            b 11
    "#});
    let parse_errors = ParseError::into_all(tree);
    let moved_parse_errors = parse_errors;
    let positions = moved_parse_errors
        .errors()
        .iter()
        .map(|e| e.node().start_position())
        .collect::<Vec<_>>();
    assert_eq!(positions, vec![Point::new(1, 4), Point::new(3, 4)]);
    let _recovered_tree = moved_parse_errors.into_tree();
}
