// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! This library defines a [DSL][] for constructing arbitrary graph structures from source code
//! that has been parsed using [tree-sitter][].
//!
//! [DSL]: reference/index.html
//! [tree-sitter]: https://docs.rs/tree-sitter/
//! [tree-sitter-python]: https://docs.rs/tree-sitter-python/
//!
//! # Overview
//!
//! You can use [tree-sitter][] to parse the content of source code into a _concrete syntax tree_.
//! There are many interesting use cases where you want to use this parsed syntax tree to create
//! some _other_ graph structure.  This library lets you do that, using a declarative [DSL][] to
//! identify patterns in the parsed syntax tree, along with rules for which nodes and edges to
//! create for the syntax nodes that match those patterns.  You can also annotate each node and
//! edge with an arbitrary set of attributes.
//!
//! You can create any graph structure, as long as each node "belongs to" or "is associated with"
//! exactly one node in the concrete syntax tree.  There are no limitations on how you use edges to
//! connect the nodes in the graph: you are not limited to creating a tree, and in particular, you
//! are not limited to creating a tree that "lines" up with the parsed syntax tree.

#[cfg(doc)]
pub mod reference;
