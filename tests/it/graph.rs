// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use indoc::indoc;
use tree_sitter::Parser;
use tree_sitter_graph::graph::Graph;
use tree_sitter_graph::graph::Value;
use tree_sitter_graph::Context;

#[test]
fn can_overwrite_attributes() {
    let mut ctx = Context::new();
    let mut graph = Graph::new();
    let node = graph.add_graph_node();
    let attrs = &mut graph[node].attributes;
    let name = ctx.add_identifier("name");
    attrs.add(name, "node0").unwrap();
    attrs.add(name, "overwritten").unwrap_err();
    assert_eq!(*attrs.get(name).unwrap(), Value::from("overwritten"));
}

#[test]
fn can_display_graph() {
    let python_source = "pass";
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_python::language()).unwrap();
    let tree = parser.parse(python_source, None).unwrap();

    let mut ctx = Context::new();
    let mut graph = Graph::new();
    let root = graph.add_syntax_node(tree.root_node());
    let node0 = graph.add_graph_node();
    graph[node0]
        .attributes
        .add(ctx.add_identifier("name"), "node0")
        .unwrap();
    graph[node0]
        .attributes
        .add(ctx.add_identifier("source"), root)
        .unwrap();
    let node1 = graph.add_graph_node();
    graph[node1]
        .attributes
        .add(ctx.add_identifier("name"), "node1")
        .unwrap();
    let node2 = graph.add_graph_node();
    graph[node2]
        .attributes
        .add(ctx.add_identifier("name"), "node2")
        .unwrap();
    graph[node2]
        .attributes
        .add(ctx.add_identifier("parent"), node1)
        .unwrap();
    let edge01 = graph[node0]
        .add_edge(node1)
        .unwrap_or_else(|_| unreachable!());
    edge01
        .attributes
        .add(ctx.add_identifier("precedence"), 14)
        .unwrap();
    assert_eq!(
        graph.display_with(&ctx).to_string(),
        indoc! {"
          node 0
            name: node0
            source: [syntax node module (0, 0)]
          edge 0 -> 1
            precedence: 14
          node 1
            name: node1
          node 2
            name: node2
            parent: [graph node 1]
        "}
    );
}
