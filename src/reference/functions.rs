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
//! # General functions
//!
//! ## `eq`
//!
//! Check if values are equal.
//!
//!   - Input parameters: two values
//!   - Output value: a boolean indicating whether the values are equal or not
//!
//! ## `is-null`
//!
//! Check if an optional value is missing.
//!
//!   - Input parameters: one value
//!   - Output value: a boolean indicating whether the value is null or not
//!
//! # Graph manipulation functions
//!
//! ## `node`
//!
//! Creates a new graph node.
//!
//!   - Input parameters: none
//!   - Output value: a reference to the new graph node
//!
//! # Logical functions
//!
//! ## `not`
//!
//! Negates a boolean value.
//!
//!   - Input parameters: one boolean
//!   - Output value: the negation of the input value
//!
//! ## `and`
//!
//! Computes the conjunction of boolean values:
//! true if none of the inputs are false, otherwise false.
//!
//!   - Input parameters: zero or more booleans
//!   - Output value: the conjunction of all the input booleans
//!
//! ## `or`
//!
//! Computes the disjunction of boolean values:
//! true if any of the inputs are true, otherwise false.
//!
//!   - Input parameters: zero or more booleans
//!   - Output value: the disjunction of all the input booleans
//!
//! # Mathematical functions
//!
//! ## `plus`
//!
//! Adds integers together.
//!
//!   - Input parameters: zero or more integers
//!   - Output value: the sum of all of the input integers
//!
//! # String functions
//!
//! ## `format`
//!
//! Formats a string according to the given format string and arguments.
//!
//!   - Input parameters:
//!     - `format`: a format string containing placeholders
//!     - as many additional parameters as there are placeholders in the format string
//!
//!   - Output value: a formatted string with the placeholders replaced by formatted values
//!
//! Placeholders are written as `{}`. To produce literal braces, use `{{` and `}}` instead.
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
//! # List functions
//!
//! ## `concat`
//!
//! Concatenate list arguments.
//!
//!  - Input parameters: list values
//!  - Output value: the concatenation of the input lists
//!
//! ## `is-empty`
//!
//! Test whether a list is empty or not.
//!
//!   - Input parameters: a list value
//!   - Output value: a boolean indicating whether the list is empty or not
//!
//! ## `join`
//!
//! Join a list of values using the given separator
//!
//!  - Input parameters:
//!    - `list`: A list of values
//!    - `sep`: An optional separator string
//! - Output value:
//!   - A string consisting of the formatted values from the list separated by
//!     the separator string
//!
//! ## `length`
//!
//! Determine the length of a list.
//!
//!   - Input parameters: a list value
//!   - Output value: an integer indicating the length of the list
//!
//! # Syntax manipulation functions
//!
//! ## `named-child-index`
//!
//! Returns the index of a "named child" within its parent.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The index of `node` within its parent's list of _named_ children (i.e., the index that
//!       would cause `ts_node_named_child` to return `node`)
//!
//! ## `named-child-count`
//!
//! Returns the number of "named children" of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The number of _named_ children in `node`
//!
//! ## `source-text`
//!
//! Returns the source text represented by a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - A string containing the source text represented by `node`
//!
//! ## `node-type`
//!
//! Returns a syntax node's type as a string.  (The type is the name of the node's grammar rule in
//! the underlying tree-sitter grammar.)
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - A string containing the type of `node`
//!
//! ## `start-column`
//!
//! Returns the zero-based start column of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The zero-based start column of `node`
//!
//! ## `start-row`
//!
//! Returns the zero-based start row of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The zero-based start row of `node`
//!
//! ## `end-column`
//!
//! Returns the zero-based end column of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The zero-based end column of `node`
//!
//! ## `end-row`
//!
//! Returns the zero-based end row of a syntax node.
//!
//!   - Input parameters:
//!     - `node`: A syntax node
//!   - Output value:
//!     - The zero-based end row of `node`
