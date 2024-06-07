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
use tree_sitter_graph::Identifier;

#[test]
fn can_overwrite_attributes() {
    let mut graph = Graph::new();
    let node = graph.add_graph_node();
    let attrs = &mut graph[node].attributes;
    let name = Identifier::from("name");
    attrs.add(name.clone(), "node0").unwrap();
    attrs.add(name.clone(), "overwritten").unwrap_err();
    assert_eq!(*attrs.get(&name).unwrap(), Value::from("overwritten"));
}

#[test]
fn can_iterate_graph_nodes() {
    let mut graph = Graph::new();
    let node0 = graph.add_graph_node();
    let node1 = graph.add_graph_node();
    let node2 = graph.add_graph_node();
    let nodes = graph.iter_nodes().collect::<Vec<_>>();
    assert_eq!(nodes, vec![node0, node1, node2]);
}

#[test]
fn can_iterate_graph_edges() {
    let mut graph = Graph::new();
    let node0 = graph.add_graph_node();
    let node1 = graph.add_graph_node();
    let node2 = graph.add_graph_node();
    let _ = graph[node0].add_edge(node1);
    let _ = graph[node0].add_edge(node2);
    let edges = graph[node0]
        .iter_edges()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();
    assert_eq!(edges, vec![node1, node2]);
}

#[test]
fn can_display_graph() {
    let python_source = "pass";
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_python::language()).unwrap();
    let tree = parser.parse(python_source, None).unwrap();

    let mut graph = Graph::new();
    let root = graph.add_syntax_node(tree.root_node());
    let node0 = graph.add_graph_node();
    graph[node0]
        .attributes
        .add(Identifier::from("name"), "node0")
        .unwrap();
    graph[node0]
        .attributes
        .add(Identifier::from("source"), root)
        .unwrap();
    let node1 = graph.add_graph_node();
    graph[node1]
        .attributes
        .add(Identifier::from("name"), "node1")
        .unwrap();
    let node2 = graph.add_graph_node();
    graph[node2]
        .attributes
        .add(Identifier::from("name"), "node2")
        .unwrap();
    graph[node2]
        .attributes
        .add(Identifier::from("parent"), node1)
        .unwrap();
    let edge01 = graph[node0]
        .add_edge(node1)
        .unwrap_or_else(|_| unreachable!());
    edge01
        .attributes
        .add(Identifier::from("precedence"), 14)
        .unwrap();
    assert_eq!(
        graph.pretty_print().to_string(),
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
        "#}
    );
}
