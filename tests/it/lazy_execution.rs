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
use tree_sitter_graph::graph::Graph;
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
        .add_immutable(ctx.add_identifier("filename"), "test.py".into())
        .map_err(|_| ExecutionError::DuplicateVariable("filename".into()))?;
    let mut graph = Graph::new();
    file.execute_lazy(
        &mut ctx,
        &tree,
        python_source,
        &mut functions,
        &mut globals,
        &mut graph,
    )?;
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
            name: "delta"
          node 1
            name: "gamma"
          edge 1 -> 0
          node 2
            name: "beta"
          edge 2 -> 1
          node 3
            name: "alpha"
          edge 3 -> 2
          node 4
          edge 4 -> 3
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
fn can_execute_if() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
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
fn can_execute_elif() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            node node0
            if #false {
              attr (node0) val = 0
            } elif #true {
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
          (module) @root
          {
            node node0
            if #false {
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
fn skip_if_without_true_conditions() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            let x = #false
            node node0
            if x {
              attr (node0) val = 0
            } elif #false {
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
fn can_execute_for_in_nonempty_list() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var n = 0
            for x in [1, 2, 3] {
              set n = (plus n x)
            }
            node node0
            attr (node0) val = n
          }
        "#},
        indoc! {r#"
          node 0
            val: 6
        "#},
    );
}

#[test]
fn can_execute_for_in_empty_list() {
    check_execution(
        "pass",
        indoc! {r#"
          (module) @root
          {
            var n = 0
            for x in [] {
              set n = (plus n x)
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
fn can_execute_star_capture() {
    check_execution(
        indoc! {r#"
          pass
          pass
        "#},
        indoc! {r#"
          (module (_)* @stmts)
          {
            var n = 0
            for x in @stmts {
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
fn can_execute_plus_capture() {
    check_execution(
        indoc! {r#"
          pass
          pass
        "#},
        indoc! {r#"
          (module (_)+ @stmts)
          {
            var n = 0
            for x in @stmts {
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
fn can_execute_optional_capture() {
    check_execution(
        indoc! {r#"
        "#},
        indoc! {r#"
          (module (_)? @stmt)
          {
            node node0
            if (is-null @stmt) {
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
fn can_execute_null_check_on_nonempty_capture() {
    check_execution(
        indoc! {r#"
          pass
        "#},
        indoc! {r#"
          (module (_)? @stmt)
          {
            node node0
            if @stmt? {
              attr (node0) val = 1
            } else {
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
fn can_execute_null_check_on_empty_capture() {
    check_execution(
        indoc! {r#"
        "#},
        indoc! {r#"
          (module (_)? @stmt)
          {
            node node0
            if @stmt? {
              attr (node0) val = 1
            } else {
              attr (node0) val = 0
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
          }
        "#},
        indoc! {r#""#},
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
            snd: #null
            len: 1
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
