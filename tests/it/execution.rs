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
    parser
        .set_language(&tree_sitter_python::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(python_source, None).unwrap();
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), dsl_source).expect("Cannot parse file");
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
          (module)
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
fn variables_in_scan_arms_are_local() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            var current_node = (node)

            scan "alpha/beta/gamma/delta.py" {
               "([^/]+)/"
               {
                 let new_node = (node)
                 attr (new_node) name = $1
                 edge current_node -> new_node
                 set current_node = new_node
               }

               "([^/]+)\\.py$"
               {
                 let new_node = (node)
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
fn can_use_global_variable() {
    check_execution(
        "pass",
        indoc! {r#"
          global filename

          (module)
          {
            node n
            attr (n) filename = filename
          }
        "#},
        indoc! {r#"
          node 0
            filename: "test.py"
    "#},
    );
}

#[test]
fn can_omit_global_variable_with_default() {
    check_execution(
        "pass",
        indoc! {r#"
          global pkgname = ""

          (module)
          {
            node n
            attr (n) pkgname = pkgname
          }
        "#},
        indoc! {r#"
          node 0
            pkgname: ""
    "#},
    );
}

#[test]
fn cannot_omit_global_variable() {
    fail_execution(
        "pass",
        indoc! {r#"
          global root

          (identifier) {
            node n
            edge n -> root
          }
        "#},
    );
}

#[test]
fn cannot_pass_string_to_global_list_variable() {
    fail_execution(
        "pass",
        indoc! {r#"
          global filename*
        "#},
    );
}

#[test]
fn can_use_variable_multiple_times() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
          {
            let x = (node)
            let y = x
            let z = x
          }
        "#},
        indoc! {r#"
          node 0
        "#},
    );
}

#[test]
fn can_nest_function_calls() {
    check_execution(
        "pass",
        indoc! {r#"
          (module)
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
          (module)
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

#[test]
fn can_execute_if_some() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (pass_statement)? @x)
          {
            node node0
            if some @x {
                attr (node0) val = 0
            } else {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 0
        "#},
    );
}

#[test]
fn can_execute_if_none() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)? @x)
          {
            node node0
            if none @x {
                attr (node0) val = 0
            } else {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 0
        "#},
    );
}

#[test]
fn can_execute_if_some_and_none() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)? @x (pass_statement)? @y)
          {
            node node0
            if none @x, some @y {
              attr (node0) val = 1
            } elif some @y {
              attr (node0) val = 0
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn can_execute_elif() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)? @x (pass_statement)? @y)
          {
            node node0
            if some @x {
              attr (node0) val = 0
            } elif some @y {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn can_execute_else() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)? @x)
          {
            node node0
            if some @x {
              attr (node0) val = 0
            } else {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn can_execute_if_literal() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)?)
          {
            node node0
            if #true {
              attr (node0) val = 0
            } else {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
            val: 0
        "#},
    );
}

#[test]
fn skip_if_without_true_conditions() {
    check_execution(
        "pass",
        indoc! {r#"
          (module (import_statement)? @x (import_statement)? @y)
          {
            node node0
            if some @x {
              attr (node0) val = 0
            } elif some @y {
              attr (node0) val = 1
            }
          }
        "#},
        indoc! {r#"
          node 0
        "#},
    );
}

#[test]
fn variables_are_local_in_if_body() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)? @x)
          {
            let n = 1
            if some @x {
              let n = 2
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn variables_do_not_escape_if_body() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)? @x)
          {
            var n = 1
            if some @x {
              var n = 2
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn variables_are_inherited_in_if_body() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)? @x)
          {
            var n = 1
            if some @x {
              set n = (plus n 1)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 2
        "#},
    );
}

#[test]
fn can_execute_for_in_nonempty_list_capture() {
    check_execution(
        r#"
          pass
          pass
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)* @xs)
          {
            var n = 0
            for x in @xs {
              set n = (plus n 1)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 3
        "#},
    );
}

#[test]
fn can_execute_for_in_empty_list_capture() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (import_statement)* @xs)
          {
            var n = 0
            for x in @xs {
              set n = (plus n 1)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 0
        "#},
    );
}

#[test]
fn can_execute_for_in_list_literal() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module)
          {
            var n = 0
            for x in [#null, #null, #null] {
              set n = (plus n 1)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 3
        "#},
    );
}

#[test]
fn variables_are_local_in_for_in_body() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)* @xs)
          {
            let n = 1
            for x in @xs {
              let n = 2
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn variables_do_not_escape_for_in_body() {
    check_execution(
        r#"
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)* @xs)
          {
            var n = 1
            for x in @xs {
              var n = 2
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 1
        "#},
    );
}

#[test]
fn variables_are_inherited_in_for_in_body() {
    check_execution(
        r#"
          pass
          pass
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)+ @xs)
          {
            var n = 0
            for x in @xs {
              set n = (plus n 1)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 3
        "#},
    );
}

#[test]
fn can_execute_list_comprehension() {
    check_execution(
        r#"
          pass
          pass
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)* @xs)
          {
            node node0
            attr (node0) val = [ (named-child-index x) for x in @xs ]
          }
        "#},
        indoc! {r#"
          node 0
            val: [0, 1, 2]
        "#},
    );
}

#[test]
fn can_execute_set_comprehension() {
    check_execution(
        r#"
          pass
          pass
          pass
        "#,
        indoc! {r#"
          (module (pass_statement)* @xs)
          {
            node node0
            attr (node0) val = { (source-text x) for x in @xs }
          }
        "#},
        indoc! {r#"
          node 0
            val: {"pass"}
        "#},
    );
}

#[test]
fn can_execute_scan_of_local_call_expression() {
    check_execution(
        r#"
          def get_f():
            pass
        "#,
        indoc! {r#"
          (function_definition
            name: (identifier) @name)
          {
            node n
            scan (source-text @name) {
              "get_.*" {
                attr (n) is_getter = #true
              }
            }
          }
        "#},
        indoc! {r#"
          node 0
            is_getter: #true
        "#},
    );
}

#[test]
fn can_execute_scan_of_local_variable() {
    check_execution(
        r#"
          def get_f():
            pass
        "#,
        indoc! {r#"
          (function_definition
            name: (identifier) @name)
          {
            node n
            let val = (source-text @name)
            scan val {
              "get_.*" {
                attr (n) is_getter = #true
              }
            }
          }
        "#},
        indoc! {r#"
          node 0
            is_getter: #true
        "#},
    );
}

#[test]
fn can_execute_shorthand() {
    check_execution(
        indoc! { r#"
          def get_f():
            pass
        "#},
        indoc! {r#"
            attribute def = x => source_node = x, symbol = (source-text x)
            (function_definition name: (identifier) @name) {
              node n
              attr (n) def = @name
            }
        "#},
        indoc! {r#"
          node 0
            source_node: [syntax node identifier (1, 5)]
            symbol: "get_f"
        "#},
    );
}

#[test]
fn can_access_inherited_attribute() {
    check_execution(
        indoc! { r#"
          def get_f():
            pass
        "#},
        indoc! {r#"
            inherit .test
            (function_definition)@def {
              node @def.test
              attr (@def.test) in_def
            }
            (pass_statement)@pass {
              attr (@pass.test) in_pass
            }
        "#},
        indoc! {r#"
          node 0
            in_def: #true
            in_pass: #true
        "#},
    );
}

#[test]
fn can_overwrite_inherited_attribute() {
    check_execution(
        indoc! { r#"
          def get_f():
            pass
        "#},
        indoc! {r#"
            inherit .test
            (function_definition)@def {
              node @def.test
              attr (@def.test) in_def
            }
            (pass_statement)@pass {
              node @pass.test
            }
            (pass_statement)@pass {
              attr (@pass.test) in_pass
            }
        "#},
        indoc! {r#"
          node 0
            in_def: #true
          node 1
            in_pass: #true
        "#},
    );
}

#[test]
fn cannot_access_non_inherited_variable() {
    fail_execution(
        indoc! { r#"
          def get_f():
            pass
        "#},
        indoc! {r#"
            (function_definition)@def {
              node @def.test
            }
            (pass_statement)@pass {
              attr (@pass.test) in_pass
            }
        "#},
    );
}

#[test]
fn can_add_edge_twice() {
    check_execution(
        indoc! { r#"
            pass
        "#},
        indoc! {r#"
            (module) {
              node n1;
              node n2;
              edge n1 -> n2;
              edge n1 -> n2;
            }
        "#},
        indoc! {r#"
          node 0
          edge 0 -> 1
          node 1
        "#},
    );
}

#[test]
fn can_set_node_attribute_value_twice() {
    check_execution(
        indoc! { r#"
            pass
        "#},
        indoc! {r#"
            (module) {
              node n;
              attr (n) foo = #true;
            }
        "#},
        indoc! {r#"
          node 0
            foo: #true
        "#},
    );
}

#[test]
fn cannot_change_attribute_value() {
    check_execution(
        indoc! { r#"
            pass
        "#},
        indoc! {r#"
            (module) {
              node n1;
              node n2;
              edge n1 -> n2;
              attr (n1 -> n2) foo = #true;
            }
        "#},
        indoc! {r#"
          node 0
          edge 0 -> 1
            foo: #true
          node 1
        "#},
    );
}
