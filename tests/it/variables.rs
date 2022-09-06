// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2022, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use tree_sitter_graph::Variables;

#[test]
fn can_create_nested_variables() {
    fn f<'a>(v: &'a Variables<'a>) -> Variables<'a> {
        let mut w = Variables::nested(v);
        w.add("bar".into(), 2.into()).expect("Failed to set bar");
        w
    }
    let mut v = Variables::new();
    v.add("foo".into(), 1.into()).expect("Failed to set foo");
    let w = f(&v);
    w.get(&"foo".into()).expect("Failed to get foo");
}
