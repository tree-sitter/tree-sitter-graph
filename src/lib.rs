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
mod execution;
pub mod functions;
pub mod graph;
mod parser;

pub use execution::ExecutionError;
pub use execution::Variables;
pub use parser::Location;
pub use parser::ParseError;

use string_interner::symbol::SymbolU32;
use string_interner::StringInterner;

use std::fmt;

/// An identifier that appears in a graph DSL file or in the graph that is produced as an output.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(SymbolU32);

impl DisplayWithContext for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        write!(f, "{}", ctx.identifiers.resolve(self.0).unwrap())
    }
}

/// A context in which graph DSL files are executed.
#[derive(Default)]
pub struct Context {
    identifiers: StringInterner,
}

impl Context {
    /// Creates a new, empty execution context.
    pub fn new() -> Context {
        Context::default()
    }

    /// Adds an identifier to the context.
    #[inline(always)]
    pub fn add_identifier<T: AsRef<str>>(&mut self, identifier: T) -> Identifier {
        Identifier(self.identifiers.get_or_intern(identifier))
    }

    /// Returns the [`Identifier`][] instance for an identifier, if it has already been added to
    /// the context.  Returns `None` otherwise.
    #[inline(always)]
    pub fn get_identifier<T: AsRef<str>>(&self, identifier: T) -> Option<Identifier> {
        self.identifiers.get(identifier).map(Identifier)
    }

    pub fn resolve(&self, identifier: Identifier) -> &str {
        self.identifiers.resolve(identifier.0).unwrap()
    }
}

/// Trait to Display with a given Context
pub trait DisplayWithContext
where
    Self: Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result;

    fn display_with<'a>(&'a self, ctx: &'a Context) -> Box<dyn fmt::Display + 'a> {
        struct ContextDisplayImpl<'a, T: DisplayWithContext>(&'a T, &'a Context);

        impl<'a, T: DisplayWithContext> fmt::Display for ContextDisplayImpl<'a, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.fmt(f, self.1)
            }
        }

        Box::new(ContextDisplayImpl(self, ctx))
    }
}

impl<T: DisplayWithContext> DisplayWithContext for Box<T> {
    fn fmt(&self, f: &mut fmt::Formatter, ctx: &Context) -> fmt::Result {
        self.as_ref().fmt(f, ctx)
    }
}