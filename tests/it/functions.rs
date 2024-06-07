// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use indoc::indoc;
use tree_sitter::Parser;
use tree_sitter_graph::ast::File;
use tree_sitter_graph::functions::Functions;
use tree_sitter_graph::ExecutionConfig;
use tree_sitter_graph::ExecutionError;
use tree_sitter_graph::Identifier;
use tree_sitter_graph::NoCancellation;
use tree_sitter_graph::Variables;

fn init_log() {
    let _ = env_logger::builder()
        .is_test(true)
        .format_level(false)
        .format_target(false)
        .format_timestamp(None)
        .try_init(); // try, because earlier test may have already initialized it
}

fn execute(python_source: &str, dsl_source: &str) -> Result<String, ExecutionError> {
    init_log();
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_python::language()).unwrap();
    let tree = parser.parse(python_source, None).unwrap();
    let file =
        File::from_str(tree_sitter_python::language(), dsl_source).expect("Cannot parse file");
    let functions = Functions::stdlib();
    let mut globals = Variables::new();
    globals
        .add(Identifier::from("filename"), "test.py".into())
        .map_err(|_| ExecutionError::DuplicateVariable("filename".into()))?;
    let mut config = ExecutionConfig::new(&functions, &globals);
    let graph = file.execute(&tree, python_source, &mut config, &NoCancellation)?;
    let result = graph.pretty_print().to_string();
    Ok(result)
}

fn check_execution(python_source: &str, dsl_source: &str, expected_graph: &str) {
    match execute(python_source, dsl_source) {
        Ok(actual_graph) => assert_eq!(actual_graph, expected_graph),
        Err(e) => panic!("Could not execute file: {}", e),
    }
}

fn fail_execution(python_source: &str, dsl_source: &str) {
    if let Ok(_) = execute(python_source, dsl_source) {
        panic!("Execution succeeded unexpectedly");
    }
}

#[test]
fn can_eq_equal_bools() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) eq = (eq #true #true)
          }
        "#},
        indoc! {r#"
          node 0
            eq: #true
        "#},
    );
}

#[test]
fn can_eq_nonequal_bools() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) eq = (eq #true #false)
          }
        "#},
        indoc! {r#"
          node 0
            eq: #false
        "#},
    );
}

#[test]
fn cannot_eq_bool_and_string() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) eq = (eq #true "false")
          }
        "#},
    );
}

#[test]
fn can_format_string_null_and_escaped_braces() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (format "{} : {{ {} }}" "foo" #null)
          }
        "#},
        indoc! {r#"
          node 0
            str: "foo : { #null }"
        "#},
    );
}

#[test]
fn cannot_format_with_missing_parameter() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (format "{} : {{ {} }}" "foo")
          }
        "#},
    );
}

#[test]
fn cannot_format_with_extra_parameter() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (format "{} : {{ {} }}" "foo" #null 42)
          }
        "#},
    );
}

#[test]
fn cannot_format_with_unexpected_opening_brace() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (format "{} : { {} }}" "foo" #null)
          }
        "#},
    );
}

#[test]
fn cannot_format_with_unexpected_closing_brace() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (format "{} : {{ {} }" "foo" #null)
          }
        "#},
    );
}

#[test]
fn can_concat_lists() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) xs = (concat [1, 2] [] [3, 4, 5])
          }
        "#},
        indoc! {r#"
          node 0
            xs: [1, 2, 3, 4, 5]
        "#},
    );
}

#[test]
fn can_join_list_with_separator() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (join [1, 2, 3] ".")
          }
        "#},
        indoc! {r#"
          node 0
            str: "1.2.3"
        "#},
    );
}

#[test]
fn can_join_list_without_separator() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            node n
            attr (n) str = (join [1, 2, 3])
          }
        "#},
        indoc! {r#"
          node 0
            str: "123"
        "#},
    );
}
