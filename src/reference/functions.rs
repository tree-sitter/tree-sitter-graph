// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

//! This section defines the standard library of functions available to graph DSL files.
//!
//! Note that the process that is executing the graph DSL file has control over which function it
//! provides.  Most of the time, you will have (at least) the functions defined here available.
//! There might be additional functions available, and in rare cases, there might be a completely
//! different set of functions available!
//!
//! # Graph manipulation functions
//!
//! ## `node`
//!
//! Creates a new graph node.
//!
//!   - Input parameters: none
//!   - Output: a reference to the new graph node
//!
//! # Mathematical functions
//!
//! ## `plus`
//!
//! Adds integers together.
//!
//!   - Input parameters: zero or more integers
//!   - Output: the sum of all of the input integers
//!
//! # String functions
//!
//! ## `replace`
//!
//! Applies a regular expression to a string, replacing any text that matches.
//!
//!   - Input parameters:
//!     - `text`: a string to look for matches in
//!     - `pattern`: a string defining the regular expression to search for
//!     - `replacement`: the text to replace any matches with
//!
//! Note that the regular expression syntax that we support is exactly that used by Rust's
//! [`regex`][] crate.  In particular, the `pattern` is passed in to [`Regex::new`][], and the
//! `replacement` text passed in to [`Regex::replace_all`][].
//!
//! [`regex`]: https://docs.rs/regex/
//! [`Regex::new`]: https://docs.rs/regex/*/regex/struct.Regex.html#method.new
//! [`Regex::replace_all`]: https://docs.rs/regex/*/regex/struct.Regex.html#method.replace_all
//!
//! # Syntax manipulation functions
//!
//! ## `child-index`
//!
//! Returns the "named child index" of a syntax node within its parent.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - The index of `node` within its parent's list of _named_ children (i.e., the index that
//!       would cause `ts_node_named_child` to return `node`)
//!
//! Note that `node` must not refer to the source file's root node, and it must be a _named_ child
//! of its parent (i.e., it can't be an anonymous leaf node).
//!
//! ## `source-text`
//!
//! Returns the source text represented by a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - A string containing the source text represented by `node`
//!
//! ## `start-column`
//!
//! Returns the zero-based start column of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - The zero-based start column of `node`
//!
//! ## `start-row`
//!
//! Returns the zero-based start row of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - The zero-based start row of `node`
//!
//! ## `end-column`
//!
//! Returns the zero-based end column of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - The zero-based end column of `node`
//!
//! ## `end-row`
//!
//! Returns the zero-based end row of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output parameter:
//!     - The zero-based end row of `node`
