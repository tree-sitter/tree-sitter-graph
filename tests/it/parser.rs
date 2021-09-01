// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use tree_sitter_graph::ast::*;
use tree_sitter_graph::Context;

#[test]
fn can_parse_blocks() {
    let mut ctx = Context::new();
    let source = r#"
        (function_definition
          name: (identifier) @cap1) @cap2
        {
          node loc1
          node @cap2.prop1
          edge @cap2.prop1 -> loc1
          attr (@cap2.prop1 -> loc1) precedence
          attr (@cap2.prop1) push = "str2", pop
          var @cap2.var1 = loc1
          set @cap2.var1 = loc1
        }
    "#;
    let mut file = File::new(tree_sitter_python::language());
    file.parse(&mut ctx, source).expect("Cannot parse file");

    let loc1 = ctx.add_identifier("loc1");
    let precedence = ctx.add_identifier("precedence");
    let pop = ctx.add_identifier("pop");
    let prop1 = ctx.add_identifier("prop1");
    let push = ctx.add_identifier("push");
    let cap2 = ctx.add_identifier("@cap2");
    let var1 = ctx.add_identifier("var1");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            CreateGraphNode { node: loc1.into() }.into(),
            CreateGraphNode {
                node: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: prop1,
                }
                .into(),
            }
            .into(),
            CreateEdge {
                source: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: prop1
                }
                .into(),
                sink: loc1.into(),
            }
            .into(),
            AddEdgeAttribute {
                source: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: prop1
                }
                .into(),
                sink: loc1.into(),
                attributes: vec![Attribute {
                    name: precedence,
                    value: Expression::TrueLiteral
                }],
            }
            .into(),
            AddGraphNodeAttribute {
                node: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: prop1
                }
                .into(),
                attributes: vec![
                    Attribute {
                        name: push,
                        value: String::from("str2").into(),
                    },
                    Attribute {
                        name: pop,
                        value: Expression::TrueLiteral
                    },
                ],
            }
            .into(),
            DeclareMutable {
                variable: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: var1,
                }
                .into(),
                value: loc1.into(),
            }
            .into(),
            Assign {
                variable: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            index: 1,
                            name: cap2
                        }
                        .into()
                    ),
                    name: var1,
                }
                .into(),
                value: loc1.into(),
            }
            .into(),
        ]]
    );
}

#[test]
fn can_parse_literals() {
    let mut ctx = Context::new();
    let source = r#"
        (identifier)
        {
          let f = #false
          let n = #null
          let t = #true
        }
    "#;
    let mut file = File::new(tree_sitter_python::language());
    file.parse(&mut ctx, source).expect("Cannot parse file");

    let f = ctx.add_identifier("f");
    let n = ctx.add_identifier("n");
    let t = ctx.add_identifier("t");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            DeclareImmutable {
                variable: f.into(),
                value: Expression::FalseLiteral,
            }
            .into(),
            DeclareImmutable {
                variable: n.into(),
                value: Expression::NullLiteral,
            }
            .into(),
            DeclareImmutable {
                variable: t.into(),
                value: Expression::TrueLiteral,
            }
            .into(),
        ]]
    );
}

#[test]
fn can_parse_strings() {
    let mut ctx = Context::new();
    let source = r#"
        (identifier)
        {
          let loc1 = "\"abc,\ndef\\"
        }
    "#;
    let mut file = File::new(tree_sitter_python::language());
    file.parse(&mut ctx, source).expect("Cannot parse file");

    let loc1 = ctx.add_identifier("loc1");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![DeclareImmutable {
            variable: loc1.into(),
            value: String::from("\"abc,\ndef\\").into(),
        }
        .into()]]
    );
}
