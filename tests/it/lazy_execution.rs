// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
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
    parser.set_language(tree_sitter_python::language()).unwrap();
    let tree = parser.parse(python_source, None).unwrap();
    let mut ctx = Context::new();
    let mut file = File::new(tree_sitter_python::language());
    file.parse(&mut ctx, dsl_source).expect("Cannot parse file");
    let mut functions = Functions::stdlib(&mut ctx);
    let mut globals = Variables::new();
    globals
        .add(ctx.add_identifier("filename"), "test.py".into())
        .map_err(|_| ExecutionError::DuplicateVariable("filename".into()))?;
    let graph = file.execute_lazy(&mut ctx, &tree, python_source, &mut functions, &globals)?;
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
            let node1 = (node)
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
          edge 0 -> 2
            precedence: 14
          node 1
            name: "node2"
            parent: [graph node 2]
          node 2
            name: "node1"
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
            name: "alpha"
          edge 0 -> 2
          node 1
          edge 1 -> 0
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
          (module) @root
          {
            var current_node = (node)

            scan "alpha/beta/gamma/delta.py" {
               "([^/]+)/"
               {
                 let v = $1
                 node new_node
                 attr (new_node) name = v
                 edge current_node -> new_node
                 set current_node = new_node
               }

               "([^/]+)\\.py$"
               {
                 let v = $1
                 node new_node
                 attr (new_node) name = v
                 edge current_node -> new_node
               }
            }
          }
        "#},
        indoc! {r#"
          node 0
            name: "alpha"
          edge 0 -> 1
          node 1
            name: "beta"
          edge 1 -> 2
          node 2
            name: "gamma"
          edge 2 -> 3
          node 3
            name: "delta"
          node 4
          edge 4 -> 0
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
          (module) @root
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
fn can_use_variable_multiple_times() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            let x = (node)
            let y = x
            let z = x
            node n
            attr (n) v1 = y, v2 = x
          }
        "#},
        indoc! {r#"
          node 0
            v1: [graph node 1]
            v2: [graph node 1]
          node 1
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

#[test]
fn can_build_node() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
          }
        "#},
        indoc! {r#"
          node 0
        "#},
    );
}

#[test]
fn can_build_node_with_attrs() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            attr (node0) name = "node0", source = @root
          }
        "#},
        indoc! {r#"
          node 0
            name: "node0"
            source: [syntax node module (1, 1)]
        "#},
    );
}

#[test]
fn can_build_edge() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            node node1
            edge node0 -> node1
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
fn can_build_edges() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            node node1
            edge node0 -> node1
            node node2
            edge node1 -> node2
            edge node2 -> node0
          }
        "#},
        indoc! {r#"
          node 0
          edge 0 -> 1
          node 1
          edge 1 -> 2
          node 2
          edge 2 -> 0
        "#},
    );
}

#[test]
fn can_set_mutable_local_variables() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var node = #null

            set node = (node)

            let new_node = (node)
            edge new_node -> node
            set node = new_node
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
fn scoped_variables_can_appear_out_of_order() {
    check_execution(
        indoc! {r#"
          import a
          from b import c
          print(a.d.f)
        "#},
        indoc! {r#"
          (identifier) @id
          {
            attr (@id.node) name = (source-text @id)
          }

          (identifier) @id
          {
            let @id.node = (node)
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
fn variables_can_be_scoped_in_arbitrary_expressions() {
    check_execution(
        indoc! {r#"
          import a
          from b import c
          print(a.d.f)
        "#},
        indoc! {r#"
          (call function:(_)@fun arguments: (argument_list (_)@arg)) {
          ; let @arg.no_object.lala = 3 ; error
            let @arg.object.lala = 3
            let @arg.object.object.lala = 12
          ; let @arg.object.object = 42 ; error
          }
          (attribute object:(_)@obj)@attr {
            let @attr.object = @obj
            let @attr.no_object = 7
          }
        "#},
        indoc! {r#""#},
    );
}

#[test]
fn can_mutate_inside_scan_no_branch_simple() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node n

            var x = 0

            scan "b" {
               "c" {
                 set x = (plus x 1)
               }
            }

            attr (n) len = x
          }
        "#},
        indoc! {r#"
          node 0
            len: 0
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_once_first_branch_simple() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node n

            var x = 0

            scan "b" {
               "b" {
                 set x = (plus x 1)
               }
            }

            attr (n) len = x
          }
        "#},
        indoc! {r#"
          node 0
            len: 1
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_once_first_branch() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node n

            var x = 0
            var y = 0
            var s = ""

            scan "b" {
               "b" { ; it's a "b"
                 attr (n) fst = #null
                 set x = (plus x 1)
                 set s = $0
               }
               "(.)" { ; it's something else
                 attr (n) snd = #null
                 let local_y = (plus y 1)
                 set y = local_y
                 set s = $1
               }
            }

            attr (n) len = (plus x y)
            attr (n) str = s
          }
        "#},
        indoc! {r#"
          node 0
            fst: #null
            len: 1
            str: "b"
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_once_second_branch() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node n

            var x = 0
            var y = 0
            var s = ""

            scan "a" {
               "b" { ; it's a "b"
                 attr (n) fst = #null
                 set x = (plus x 1)
                 set s = $0
               }
               "(.)" { ; it's something else
                 attr (n) snd = #null
                 let local_y = (plus y 1)
                 set y = local_y
                 set s = $1
               }
            }

            attr (n) len = (plus x y)
            attr (n) str = s
          }
        "#},
        indoc! {r#"
          node 0
            len: 1
            snd: #null
            str: "a"
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_once_no_branch() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node n

            var x = 0
            var y = 0
            var s = ""

            scan "a" {
               "b" { ; it's a "b"
                 attr (n) fst = #null
                 set x = (plus x 1)
                 set s = $0
               }
               "(c)" { ; it's something else
                 attr (n) snd = #null
                 let local_y = (plus y 1)
                 set y = local_y
                 set s = $1
               }
            }

            attr (n) len = (plus x y)
            attr (n) str = s
          }
        "#},
        indoc! {r#"
          node 0
            len: 0
            str: ""
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_multiple_times_simple() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var x = 0
            var y = 0

            scan "ab" {
               "b" { ; count "b"s
                  set x = (plus x 1)
               }
               "(.)" { ; count the rest
                  set y = (plus y 1)
               }
            }

            node n
            attr (n) len = (plus x y)
          }
        "#},
        indoc! {r#"
          node 0
            len: 2
        "#},
    );
}

#[test]
fn can_mutate_inside_scan_multiple_times() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var x = 0
            var y = 0
            var s = ""

            scan "abcd" {
               "b" { ; count "b"s
                 set x = (plus x 1)
                 set s = $0
               }
               "(.)" { ; count the rest
                 let local_y = (plus y 1)
                 set y = local_y
                 set s = $1
               }
            }

            node n
            attr (n) len = (plus x y)
            attr (n) str = s
          }
        "#},
        indoc! {r#"
          node 0
            len: 4
            str: "d"
        "#},
    );
}

#[test]
fn can_mutate_inside_nested_scan_multiple_times_simple() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var x = 0

            scan "ab|c|d" {
              "\\|" {}
              "[^|]+" {
                scan $0 {
                  "(.)" { ; count the rest
                    set x = (plus x 1)
                  }
                }
              }
            }

            node n
            attr (n) len = x
          }
        "#},
        indoc! {r#"
          node 0
            len: 4
        "#},
    );
}

#[test]
fn can_mutate_inside_nested_scan_multiple_times() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var x = 0
            var y = 0
            var s = ""

            scan "ab|c|d" {
              "\\|" {}
              "[^|]+" {
                scan $0 {
                  "b" { ; count "b"s
                    set x = (plus x 1)
                    set s = $0
                  }
                  "(.)" { ; count the rest
                    let local_y = (plus y 1)
                    set y = local_y
                    set s = $1
                  }
                }
              }
            }

            node n
            attr (n) len = (plus x y)
            attr (n) str = s
          }
        "#},
        indoc! {r#"
          node 0
            len: 4
            str: "d"
        "#},
    );
}

#[test]
fn variable_let_executed_once() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            let x = (node)
            attr ((node)) ref = x
            attr ((node)) ref = x
          }
        "#},
        indoc! {r#"
          node 0
            ref: [graph node 1]
          node 1
          node 2
            ref: [graph node 1]
        "#},
    );
}

#[test]
fn variable_set_executed_once() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var x = #null
            set x = (node)
            attr ((node)) ref = x
            attr ((node)) ref = x
          }
        "#},
        indoc! {r#"
          node 0
            ref: [graph node 1]
          node 1
          node 2
            ref: [graph node 1]
        "#},
    );
}
