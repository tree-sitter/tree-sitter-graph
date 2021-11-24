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
use tree_sitter_graph::Context;
use tree_sitter_graph::ExecutionError;
use tree_sitter_graph::Variables;

fn execute(python_source: &str, dsl_source: &str) -> Result<String, ExecutionError> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_python::language()).unwrap();
    let tree = parser.parse(python_source, None).unwrap();
    let mut ctx = Context::new();
    let mut file = File::new(tree_sitter_python::language());
    file.parse(&mut ctx, dsl_source).expect("Cannot parse file");
    let mut functions = Functions::stdlib(&mut ctx);
    let mut globals = Variables::new();
    let graph = file.execute(&ctx, &tree, python_source, &mut functions, &mut globals)?;
    let result = graph.display_with(&ctx).to_string();
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
fn can_build_simple_graph() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            attr (node0) name = "node0", source = @root
            var node1 = (node)
            attr (node1) name = "node1"
            edge node0 -> node1
            attr (node0 -> node1) precedence = 14
            node node2
            attr (node2) name = "node2", parent = node1
          }
        "#},
        indoc! {r#"
          node 0
            name: "node0"
            source: [syntax node module (1, 1)]
          edge 0 -> 1
            precedence: 14
          node 1
            name: "node1"
          node 2
            name: "node2"
            parent: [graph node 1]
        "#},
    );
}

#[test]
fn can_scan_strings() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var new_node = #null
            var current_node = (node)

            scan "alpha/beta/gamma/delta.py" {
               "([^/]+)/"
               {
                 set new_node = (node)
                 attr (new_node) name = $1
                 edge current_node -> new_node
                 set current_node = new_node
               }

               "([^/]+)\\.py$"
               {
                 set new_node = (node)
                 attr (new_node) name = $1
                 edge current_node -> new_node
               }
            }
          }
        "#},
        indoc! {r#"
          node 0
          edge 0 -> 1
          node 1
            name: "alpha"
          edge 1 -> 2
          node 2
            name: "beta"
          edge 2 -> 3
          node 3
            name: "gamma"
          edge 3 -> 4
          node 4
            name: "delta"
        "#},
    );
}

#[test]
fn scoped_variables_carry_across_stanzas() {
    check_execution(
        indoc! {r#"
          import a
          from b import c
          print(a.d.f)
        "#},
        indoc! {r#"
          (identifier) @id
          {
            let @id.node = (node)
          }

          (identifier) @id
          {
            attr (@id.node) name = (source-text @id)
          }
        "#},
        indoc! {r#"
          node 0
            name: "a"
          node 1
            name: "b"
          node 2
            name: "c"
          node 3
            name: "print"
          node 4
            name: "a"
          node 5
            name: "d"
          node 6
            name: "f"
        "#},
    );
}

#[test]
fn can_match_stanza_multiple_times() {
    check_execution(
        indoc! {r#"
          import a
          from b import c
          print(a.d.f)
        "#},
        indoc! {r#"
          (identifier) @id
          {
            node new_node
            attr (new_node) name = (source-text @id)
          }
        "#},
        indoc! {r#"
          node 0
            name: "a"
          node 1
            name: "b"
          node 2
            name: "c"
          node 3
            name: "print"
          node 4
            name: "a"
          node 5
            name: "d"
          node 6
            name: "f"
        "#},
    );
}

#[test]
fn can_nest_function_calls() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            attr (node0) val = (replace "accacc" (replace "abc" "b" "c") (replace "abc" "a" "b"))
          }
        "#},
        indoc! {r#"
          node 0
            val: "bbcbbc"
        "#},
    );
}

#[test]
fn cannot_use_nullable_regex() {
    fail_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            scan "abc" {
              "^\\b" {
              }
            }
            node n
          }
        "#},
    );
}

#[test]
fn can_create_present_optional_capture() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (_)? @stmts)
          {
            node n
            attr (n) stmts = @stmts
          }
        "#},
        indoc! {r#"
          node 0
            stmts: [syntax node pass_statement (1, 1)]
        "#},
    );
}

#[test]
fn can_create_missing_optional_capture() {
    check_execution(
        indoc! {r#"
        "#},
        indoc! {r#"
          (module (_)? @stmts)
          {
            node n
            attr (n) stmts = @stmts
          }
        "#},
        indoc! {r#"
          node 0
            stmts: #null
        "#},
    );
}

#[test]
fn can_create_empty_list_capture() {
    check_execution(
        indoc! {r#"
        "#},
        indoc! {r#"
          (module (_)* @stmts)
          {
            node n
            attr (n) stmts = @stmts
          }
        "#},
        indoc! {r#"
          node 0
            stmts: []
        "#},
    );
}

#[test]
fn can_create_nonempty_list_capture() {
    check_execution(
        indoc! {r#"
          pass
          pass
        "#},
        indoc! {r#"
          (module (_)+ @stmts)
          {
            node n
            attr (n) stmts = @stmts
          }
        "#},
        indoc! {r#"
          node 0
            stmts: [[syntax node pass_statement (1, 1)], [syntax node pass_statement (2, 1)]]
        "#},
    );
}
