// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use tree_sitter::CaptureQuantifier::*;

use tree_sitter_graph::ast::*;
use tree_sitter_graph::Identifier;
use tree_sitter_graph::Location;
use tree_sitter_graph::ParseError;

#[test]
fn can_parse_blocks() {
    let source = r#"
        (function_definition
          name: (identifier) @_cap1) @cap2
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
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let loc1 = Identifier::from("loc1");
    let precedence = Identifier::from("precedence");
    let pop = Identifier::from("pop");
    let prop1 = Identifier::from("prop1");
    let push = Identifier::from("push");
    let cap2 = Identifier::from("cap2");
    let var1 = Identifier::from("var1");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            CreateGraphNode {
                node: UnscopedVariable {
                    name: loc1.clone(),
                    location: Location { row: 4, column: 15 }
                }
                .into(),
                location: Location { row: 4, column: 10 }
            }
            .into(),
            CreateGraphNode {
                node: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location { row: 5, column: 15 }
                        }
                        .into()
                    ),
                    name: prop1.clone(),
                    location: Location { row: 5, column: 21 }
                }
                .into(),
                location: Location { row: 5, column: 10 },
            }
            .into(),
            CreateEdge {
                source: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location { row: 6, column: 15 }
                        }
                        .into()
                    ),
                    name: prop1.clone(),
                    location: Location { row: 6, column: 21 }
                }
                .into(),
                sink: UnscopedVariable {
                    name: loc1.clone(),
                    location: Location { row: 6, column: 30 },
                }
                .into(),
                location: Location { row: 6, column: 10 },
            }
            .into(),
            AddEdgeAttribute {
                source: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location { row: 7, column: 16 }
                        }
                        .into()
                    ),
                    name: prop1.clone(),
                    location: Location { row: 7, column: 22 },
                }
                .into(),
                sink: UnscopedVariable {
                    name: loc1.clone(),
                    location: Location { row: 7, column: 31 },
                }
                .into(),
                attributes: vec![Attribute {
                    name: precedence,
                    value: Expression::TrueLiteral
                }],
                location: Location { row: 7, column: 10 },
            }
            .into(),
            AddGraphNodeAttribute {
                node: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location { row: 8, column: 16 }
                        }
                        .into()
                    ),
                    name: prop1.clone(),
                    location: Location { row: 8, column: 22 },
                }
                .into(),
                attributes: vec![
                    Attribute {
                        name: push.clone(),
                        value: String::from("str2").into(),
                    },
                    Attribute {
                        name: pop.clone(),
                        value: Expression::TrueLiteral,
                    },
                ],
                location: Location { row: 8, column: 10 },
            }
            .into(),
            DeclareMutable {
                variable: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location { row: 9, column: 14 }
                        }
                        .into()
                    ),
                    name: var1.clone(),
                    location: Location { row: 9, column: 20 },
                }
                .into(),
                value: UnscopedVariable {
                    name: loc1.clone(),
                    location: Location { row: 9, column: 27 },
                }
                .into(),
                location: Location { row: 9, column: 10 },
            }
            .into(),
            Assign {
                variable: ScopedVariable {
                    scope: Box::new(
                        Capture {
                            quantifier: One,
                            name: cap2.clone(),
                            file_capture_index: 1,
                            stanza_capture_index: 1,
                            location: Location {
                                row: 10,
                                column: 14
                            }
                        }
                        .into()
                    ),
                    name: var1.clone(),
                    location: Location {
                        row: 10,
                        column: 20
                    },
                }
                .into(),
                value: UnscopedVariable {
                    name: loc1.clone(),
                    location: Location {
                        row: 10,
                        column: 27
                    },
                }
                .into(),
                location: Location {
                    row: 10,
                    column: 10
                },
            }
            .into(),
        ]]
    );
}

#[test]
fn can_parse_literals() {
    let source = r#"
        (identifier)
        {
          let f = #false
          let n = #null
          let t = #true
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let f = Identifier::from("f");
    let n = Identifier::from("n");
    let t = Identifier::from("t");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: f.clone(),
                    location: Location { row: 3, column: 14 }
                }
                .into(),
                value: Expression::FalseLiteral,
                location: Location { row: 3, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: n.clone(),
                    location: Location { row: 4, column: 14 }
                }
                .into(),
                value: Expression::NullLiteral,
                location: Location { row: 4, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: t.clone(),
                    location: Location { row: 5, column: 14 }
                }
                .into(),
                value: Expression::TrueLiteral,
                location: Location { row: 5, column: 10 },
            }
            .into(),
        ]]
    );
}

#[test]
fn can_parse_strings() {
    let source = r#"
        (identifier)
        {
          let loc1 = "\"abc,\ndef\\"
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let loc1 = Identifier::from("loc1");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![DeclareImmutable {
            variable: UnscopedVariable {
                name: loc1.clone(),
                location: Location { row: 3, column: 14 }
            }
            .into(),
            value: String::from("\"abc,\ndef\\").into(),
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_lists() {
    let source = r#"
        (identifier)
        {
          let list1 = [1, 2, 3]
          let list2 = []
          let list3 = ["hello", "world",]
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let list1 = Identifier::from("list1");
    let list2 = Identifier::from("list2");
    let list3 = Identifier::from("list3");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: list1.clone(),
                    location: Location { row: 3, column: 14 }
                }
                .into(),
                value: ListLiteral {
                    elements: vec![
                        IntegerConstant { value: 1 }.into(),
                        IntegerConstant { value: 2 }.into(),
                        IntegerConstant { value: 3 }.into(),
                    ],
                }
                .into(),
                location: Location { row: 3, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: list2.clone(),
                    location: Location { row: 4, column: 14 }
                }
                .into(),
                value: ListLiteral { elements: vec![] }.into(),
                location: Location { row: 4, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: list3.clone(),
                    location: Location { row: 5, column: 14 }
                }
                .into(),
                value: ListLiteral {
                    elements: vec![
                        StringConstant {
                            value: String::from("hello")
                        }
                        .into(),
                        StringConstant {
                            value: String::from("world")
                        }
                        .into(),
                    ],
                }
                .into(),
                location: Location { row: 5, column: 10 },
            }
            .into()
        ]]
    );
}

#[test]
fn can_parse_sets() {
    let source = r#"
        (identifier)
        {
          let set1 = {1, 2, 3}
          let set2 = {}
          let set3 = {"hello", "world",}
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let set1 = Identifier::from("set1");
    let set2 = Identifier::from("set2");
    let set3 = Identifier::from("set3");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: set1.clone(),
                    location: Location { row: 3, column: 14 }
                }
                .into(),
                value: SetLiteral {
                    elements: vec![
                        IntegerConstant { value: 1 }.into(),
                        IntegerConstant { value: 2 }.into(),
                        IntegerConstant { value: 3 }.into(),
                    ],
                }
                .into(),
                location: Location { row: 3, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: set2.clone(),
                    location: Location { row: 4, column: 14 }
                }
                .into(),
                value: SetLiteral { elements: vec![] }.into(),
                location: Location { row: 4, column: 10 },
            }
            .into(),
            DeclareImmutable {
                variable: UnscopedVariable {
                    name: set3.clone(),
                    location: Location { row: 5, column: 14 }
                }
                .into(),
                value: SetLiteral {
                    elements: vec![
                        StringConstant {
                            value: String::from("hello")
                        }
                        .into(),
                        StringConstant {
                            value: String::from("world")
                        }
                        .into(),
                    ],
                }
                .into(),
                location: Location { row: 5, column: 10 },
            }
            .into()
        ]]
    );
}

#[test]
fn can_parse_print() {
    let source = r#"
        (identifier)
        {
          print "x =", 5
        }    
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![
                StringConstant {
                    value: String::from("x =")
                }
                .into(),
                IntegerConstant { value: 5 }.into(),
            ],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn cannot_parse_nullable_regex() {
    let source = r#"
        (module) @root
        {
          scan "abc" {
            "|" {
            }
          }
          node n
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_star_capture() {
    let source = r#"
        (module (_)* @stmts)
        {
          print @stmts
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmts = Identifier::from("stmts");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: ZeroOrMore,
                name: stmts,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_star_multiple_capture() {
    let source = r#"
        (module (_) @stmt * @stmts)
        {
          print @stmt
          print @stmts
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmt = Identifier::from("stmt");
    let stmts = Identifier::from("stmts");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            Print {
                values: vec![Capture {
                    quantifier: ZeroOrMore,
                    name: stmt,
                    file_capture_index: 0,
                    stanza_capture_index: 0,
                    location: Location { row: 3, column: 16 },
                }
                .into()],
                location: Location { row: 3, column: 10 },
            }
            .into(),
            Print {
                values: vec![Capture {
                    quantifier: ZeroOrMore,
                    name: stmts,
                    file_capture_index: 1,
                    stanza_capture_index: 1,
                    location: Location { row: 4, column: 16 },
                }
                .into()],
                location: Location { row: 4, column: 10 },
            }
            .into()
        ]]
    );
}

#[test]
fn can_parse_plus_capture() {
    let source = r#"
        (module (_)+ @stmts)
        {
          print @stmts
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmts = Identifier::from("stmts");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: OneOrMore,
                name: stmts,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_optional_capture() {
    let source = r#"
        (module (_)? @stmt)
        {
          print @stmt
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmt = Identifier::from("stmt");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: ZeroOrOne,
                name: stmt,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_parent_optional_capture() {
    let source = r#"
        (module (_) @stmt) ?
        {
          print @stmt
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmt = Identifier::from("stmt");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: ZeroOrOne,
                name: stmt,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_alternative_capture() {
    let source = r#"
        (module [(_) (_) @stmt])
        {
          print @stmt
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmt = Identifier::from("stmt");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: ZeroOrOne,
                name: stmt,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_nested_plus_and_optional_capture() {
    let source = r#"
        (module (_)+ @stmt) ?
        {
          print @stmt
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let stmt = Identifier::from("stmt");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![Capture {
                quantifier: ZeroOrMore,
                name: stmt,
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 16 },
            }
            .into()],
            location: Location { row: 3, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn can_parse_if() {
    let source = r#"
        (module (pass_statement)? @x)
        {
          if some @x {
            print "x is not null"
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let x = Identifier::from("x");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![If {
            arms: vec![IfArm {
                conditions: vec![Condition::Some {
                    value: Capture {
                        quantifier: ZeroOrOne,
                        name: x,
                        file_capture_index: 0,
                        stanza_capture_index: 0,
                        location: Location { row: 3, column: 18 },
                    }
                    .into(),
                    location: Location { row: 3, column: 13 },
                }],
                statements: vec![Print {
                    values: vec![StringConstant {
                        value: "x is not null".into()
                    }
                    .into()],
                    location: Location { row: 4, column: 12 }
                }
                .into()],
                location: Location { row: 3, column: 10 }
            }],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn can_parse_if_elif() {
    let source = r#"
        (module (pass_statement)? @x)
        {
          if none @x {
            print "x is null"
          } elif some @x {
            print "x is not null"
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let x = Identifier::from("x");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![If {
            arms: vec![
                IfArm {
                    conditions: vec![Condition::None {
                        value: Capture {
                            quantifier: ZeroOrOne,
                            name: x.clone(),
                            file_capture_index: 0,
                            stanza_capture_index: 0,
                            location: Location { row: 3, column: 18 },
                        }
                        .into(),
                        location: Location { row: 3, column: 13 },
                    }
                    .into(),],
                    statements: vec![Print {
                        values: vec![StringConstant {
                            value: "x is null".into()
                        }
                        .into()],
                        location: Location { row: 4, column: 12 }
                    }
                    .into()],
                    location: Location { row: 3, column: 10 }
                },
                IfArm {
                    conditions: vec![Condition::Some {
                        value: Capture {
                            quantifier: ZeroOrOne,
                            name: x.clone(),
                            file_capture_index: 0,
                            stanza_capture_index: 0,
                            location: Location { row: 5, column: 22 },
                        }
                        .into(),
                        location: Location { row: 5, column: 17 },
                    }],
                    statements: vec![Print {
                        values: vec![StringConstant {
                            value: "x is not null".into()
                        }
                        .into()],
                        location: Location { row: 6, column: 12 }
                    }
                    .into()],
                    location: Location { row: 5, column: 12 }
                }
            ],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn can_parse_if_else() {
    let source = r#"
        (module (pass_statement)? @x)
        {
          if none @x {
            print "x is null"
          } else {
            print "x is not null"
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let x = Identifier::from("x");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![If {
            arms: vec![
                IfArm {
                    conditions: vec![Condition::None {
                        value: Capture {
                            quantifier: ZeroOrOne,
                            name: x,
                            file_capture_index: 0,
                            stanza_capture_index: 0,
                            location: Location { row: 3, column: 18 },
                        }
                        .into(),
                        location: Location { row: 3, column: 13 },
                    }],
                    statements: vec![Print {
                        values: vec![StringConstant {
                            value: "x is null".into()
                        }
                        .into()],
                        location: Location { row: 4, column: 12 }
                    }
                    .into()],
                    location: Location { row: 3, column: 10 }
                },
                IfArm {
                    conditions: vec![],
                    statements: vec![Print {
                        values: vec![StringConstant {
                            value: "x is not null".into()
                        }
                        .into()],
                        location: Location { row: 6, column: 12 }
                    }
                    .into()],
                    location: Location { row: 5, column: 12 }
                }
            ],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn cannot_parse_if_some_list_capture() {
    let source = r#"
        (module (_)+ @xs) @root
        {
          if some @xs {
            node n
          }
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_for_in() {
    let source = r#"
        (module (_)* @xs)
        {
          for x in @xs {
            print x
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let xs = Identifier::from("xs");
    let x = Identifier::from("x");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![ForIn {
            variable: UnscopedVariable {
                name: x.clone(),
                location: Location { row: 3, column: 14 }
            },
            value: Capture {
                quantifier: ZeroOrMore,
                name: xs.clone(),
                file_capture_index: 0,
                stanza_capture_index: 0,
                location: Location { row: 3, column: 19 },
            }
            .into(),
            statements: vec![Print {
                values: vec![UnscopedVariable {
                    name: x.clone(),
                    location: Location { row: 4, column: 18 },
                }
                .into()],
                location: Location { row: 4, column: 12 }
            }
            .into()],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn cannot_parse_for_in_optional_capture() {
    let source = r#"
        (module (_)? @xs) @root
        {
          for x in @xs {
            node n
          }
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn cannot_parse_scan_of_nonlocal_call_expression() {
    let source = r#"
      (function_definition
      name: (identifier) @name)
      {
        node n
        scan (source-text @name.val) {
          "get_.*" {
            attr (n) is_getter = #true
          }
        }
      }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn cannot_parse_scan_of_nonlocal_variable() {
    let source = r#"
      (function_definition
      name: (identifier) @name)
      {
        node n
        let val = (source-text @name.val)
        scan val {
          "get_.*" {
            attr (n) is_getter = #true
          }
        }
      }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_list_comprehension() {
    let source = r#"
        (module (_)* @xs)
        {
          print [ (named-child-index x) for x in @xs ]
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![ListComprehension {
                element: Box::new(
                    Call {
                        function: "named-child-index".into(),
                        parameters: vec![UnscopedVariable {
                            name: "x".into(),
                            location: Location { row: 3, column: 37 }
                        }
                        .into()]
                    }
                    .into()
                ),
                variable: UnscopedVariable {
                    name: "x".into(),
                    location: Location { row: 3, column: 44 }
                },
                value: Box::new(
                    Capture {
                        name: "xs".into(),
                        quantifier: ZeroOrMore,
                        file_capture_index: 0,
                        stanza_capture_index: 0,
                        location: Location { row: 3, column: 49 }
                    }
                    .into()
                ),
                location: Location { row: 3, column: 16 }
            }
            .into()],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn can_parse_set_comprehension() {
    let source = r#"
        (module (_)* @xs)
        {
          print { (named-child-index x) for x in @xs }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![SetComprehension {
                element: Box::new(
                    Call {
                        function: "named-child-index".into(),
                        parameters: vec![UnscopedVariable {
                            name: "x".into(),
                            location: Location { row: 3, column: 37 }
                        }
                        .into()]
                    }
                    .into()
                ),
                variable: UnscopedVariable {
                    name: "x".into(),
                    location: Location { row: 3, column: 44 }
                },
                value: Box::new(
                    Capture {
                        name: "xs".into(),
                        quantifier: ZeroOrMore,
                        file_capture_index: 0,
                        stanza_capture_index: 0,
                        location: Location { row: 3, column: 49 }
                    }
                    .into()
                ),
                location: Location { row: 3, column: 16 }
            }
            .into()],
            location: Location { row: 3, column: 10 }
        }
        .into()]]
    );
}

#[test]
fn can_parse_global() {
    let source = r#"
        global root

        (identifier) {
          node n
          edge n -> root
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    assert_eq!(
        file.globals,
        vec![Global {
            name: "root".into(),
            quantifier: One,
            default: None,
            location: Location { row: 1, column: 15 },
        }]
    );

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![
            CreateGraphNode {
                node: UnscopedVariable {
                    name: "n".into(),
                    location: Location { row: 4, column: 15 },
                }
                .into(),
                location: Location { row: 4, column: 10 },
            }
            .into(),
            CreateEdge {
                source: UnscopedVariable {
                    name: "n".into(),
                    location: Location { row: 5, column: 15 },
                }
                .into(),
                sink: UnscopedVariable {
                    name: "root".into(),
                    location: Location { row: 5, column: 20 },
                }
                .into(),
                location: Location { row: 5, column: 10 },
            }
            .into(),
        ]]
    );
}

#[test]
fn can_parse_global_with_default() {
    let source = r#"
        global PKG_NAME = ""

        (identifier) {
          print PKG_NAME
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    assert_eq!(
        file.globals,
        vec![Global {
            name: "PKG_NAME".into(),
            quantifier: One,
            default: Some("".into()),
            location: Location { row: 1, column: 15 },
        }]
    );

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![Print {
            values: vec![UnscopedVariable {
                name: "PKG_NAME".into(),
                location: Location { row: 4, column: 16 }
            }
            .into()],
            location: Location { row: 4, column: 10 },
        }
        .into()]]
    );
}

#[test]
fn cannot_parse_undeclared_global() {
    let source = r#"
        (identifier) {
          node n
          edge n -> root
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_list_global() {
    let source = r#"
        global roots*

        (identifier) {
          for root in roots {
            node n
            edge n -> root
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    assert_eq!(
        file.globals,
        vec![Global {
            name: "roots".into(),
            quantifier: ZeroOrMore,
            default: None,
            location: Location { row: 1, column: 15 },
        }]
    );

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![ForIn {
            variable: UnscopedVariable {
                name: "root".into(),
                location: Location { row: 4, column: 14 },
            }
            .into(),
            value: UnscopedVariable {
                name: "roots".into(),
                location: Location { row: 4, column: 22 },
            }
            .into(),
            statements: vec![
                CreateGraphNode {
                    node: UnscopedVariable {
                        name: "n".into(),
                        location: Location { row: 5, column: 17 },
                    }
                    .into(),
                    location: Location { row: 5, column: 12 },
                }
                .into(),
                CreateEdge {
                    source: UnscopedVariable {
                        name: "n".into(),
                        location: Location { row: 6, column: 17 },
                    }
                    .into(),
                    sink: UnscopedVariable {
                        name: "root".into(),
                        location: Location { row: 6, column: 22 },
                    }
                    .into(),
                    location: Location { row: 6, column: 12 },
                }
                .into(),
            ],
            location: Location { row: 4, column: 10 },
        }
        .into(),]]
    );
}

#[test]
fn can_parse_optional_global() {
    let source = r#"
        global root?

        (identifier) {
          if some root {
            node n
            edge n -> root
          }
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    assert_eq!(
        file.globals,
        vec![Global {
            name: "root".into(),
            quantifier: ZeroOrOne,
            default: None,
            location: Location { row: 1, column: 15 },
        }]
    );

    let statements = file
        .stanzas
        .into_iter()
        .map(|s| s.statements)
        .collect::<Vec<_>>();
    assert_eq!(
        statements,
        vec![vec![If {
            arms: vec![IfArm {
                conditions: vec![Condition::Some {
                    value: UnscopedVariable {
                        name: "root".into(),
                        location: Location { row: 4, column: 18 },
                    }
                    .into(),
                    location: Location { row: 4, column: 13 },
                },],
                statements: vec![
                    CreateGraphNode {
                        node: UnscopedVariable {
                            name: "n".into(),
                            location: Location { row: 5, column: 17 },
                        }
                        .into(),
                        location: Location { row: 5, column: 12 },
                    }
                    .into(),
                    CreateEdge {
                        source: UnscopedVariable {
                            name: "n".into(),
                            location: Location { row: 6, column: 17 },
                        }
                        .into(),
                        sink: UnscopedVariable {
                            name: "root".into(),
                            location: Location { row: 6, column: 22 },
                        }
                        .into(),
                        location: Location { row: 6, column: 12 },
                    }
                    .into(),
                ],
                location: Location { row: 4, column: 10 },
            },],
            location: Location { row: 4, column: 10 },
        }
        .into(),]]
    );
}

#[test]
fn cannot_parse_global_with_unknown_quantifier() {
    let source = r#"
        global root%

        (module) {
          node root
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn cannot_parse_hiding_global() {
    let source = r#"
        global root

        (module) {
          node root
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn cannot_parse_set_global() {
    let source = r#"
        global root

        (module) {
          set root = #null
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_shorthand() {
    let source = r#"
        attribute def = x => source_node = x, symbol = (source-text x)
        (function_definition name: (identifier) @name) {
          node n
          attr (n) sh = @name
        }
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("Cannot parse file");

    let shorthands = file.shorthands.into_iter().collect::<Vec<_>>();
    assert_eq!(
        shorthands,
        vec![AttributeShorthand {
            name: "def".into(),
            variable: UnscopedVariable {
                name: "x".into(),
                location: Location { row: 1, column: 24 }
            },
            attributes: vec![
                Attribute {
                    name: "source_node".into(),
                    value: UnscopedVariable {
                        name: "x".into(),
                        location: Location { row: 1, column: 43 }
                    }
                    .into()
                },
                Attribute {
                    name: "symbol".into(),
                    value: Call {
                        function: "source-text".into(),
                        parameters: vec![UnscopedVariable {
                            name: "x".into(),
                            location: Location { row: 1, column: 68 }
                        }
                        .into()]
                    }
                    .into(),
                }
            ],
            location: Location { row: 1, column: 18 }
        }]
    );
}

#[test]
fn cannot_parse_multiple_patterns() {
    let source = r#"
        (function_definition)
        (pass_statement)
        {
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn query_parse_errors_have_file_location() {
    let source = r#"
        ; skip the first line
        (module (non_existing_node))
        {}
    "#;
    let err = match File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        Ok(_) => panic!("Parse succeeded unexpectedly"),
        Err(ParseError::QueryError(e)) => e,
        Err(e) => panic!("Unexpected error: {}", e),
    };
    assert_eq!(err.row, 2, "expected row 2, got {}", err.row);
    assert_eq!(err.column, 17, "expected column 17, got {}", err.column);
    assert_eq!(err.offset, 48, "expected offset 48, got {}", err.offset);
}

#[test]
fn multiline_query_parse_errors_have_file_location() {
    let source = r#"
        (module) @root
        {}
        [
          (module (pass_statement))
          (module
            (non_existing_node))
        ]
        {}
    "#;
    let err = match File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        Ok(_) => panic!("Parse succeeded unexpectedly"),
        Err(ParseError::QueryError(e)) => e,
        Err(e) => panic!("Unexpected error: {}", e),
    };
    assert_eq!(err.row, 6, "expected row 6, got {}", err.row);
    assert_eq!(err.column, 13, "expected column 13, got {}", err.column);
    assert_eq!(err.offset, 112, "expected offset 112, got {}", err.offset);
}

#[test]
fn cannot_parse_unused_capture() {
    let source = r#"
        (function_definition name: (identifier) @name) {
        }
    "#;
    if let Ok(_) = File::from_str(tree_sitter_python::LANGUAGE.into(), source) {
        panic!("Parse succeeded unexpectedly");
    }
}

#[test]
fn can_parse_explicitly_unused_capture() {
    let source = r#"
        (function_definition name: (identifier) @_name) {
        }
    "#;
    File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("parse to succeed");
}

#[test]
fn can_parse_inherit_directives() {
    let source = r#"
        inherit .scope
    "#;
    let file =
        File::from_str(tree_sitter_python::LANGUAGE.into(), source).expect("parse to succeed");
    assert!(file.inherited_variables.contains("scope".into()));
}
