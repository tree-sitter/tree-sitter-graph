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
//! There are no limitations on what graph structure you create: you are not limited to creating a
//! tree, and in particular, you are not limited to creating a tree that "lines" up with the parsed
//! syntax tree.

#[cfg(doc)]
pub mod reference;

pub mod ast;
mod checker;
mod execution;
pub mod functions;
pub mod graph;
pub mod parse_error;
mod parser;
mod variables;

pub use execution::error::ExecutionError;
pub use execution::CancellationError;
pub use execution::CancellationFlag;
pub use execution::ExecutionConfig;
pub use execution::NoCancellation;
pub use parser::Location;
pub use parser::ParseError;
pub use variables::Globals as Variables;
pub use variables::Iter as VariableIter;
pub use variables::VariableError;

use std::borrow::Borrow;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use serde::Serialize;
use serde::Serializer;

/// An identifier that appears in a graph DSL file or in the graph that is produced as an output.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Identifier(Arc<String>);

impl Identifier {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn into_string(mut self) -> String {
        Arc::make_mut(&mut self.0);
        Arc::try_unwrap(self.0).unwrap()
    }
}

impl Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Deref for Identifier {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Identifier {
        Identifier(Arc::new(String::from(value)))
    }
}

impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<'a> PartialEq<&'a str> for Identifier {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_str() == *other
    }
}

impl Serialize for Identifier {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}
