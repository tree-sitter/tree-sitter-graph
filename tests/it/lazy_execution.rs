// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

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

#[allow(unused)]
fn check_execution(python_source: &str, dsl_source: &str, expected_graph: &str) {
    match execute(python_source, dsl_source) {
        Ok(actual_graph) => assert_eq!(actual_graph, expected_graph),
        Err(e) => panic!("Could not execute file: {}", e),
    }
}

#[allow(unused)]
fn fail_execution(python_source: &str, dsl_source: &str) {
    if let Ok(_) = execute(python_source, dsl_source) {
        panic!("Execution succeeded unexpectedly");
    }
}
