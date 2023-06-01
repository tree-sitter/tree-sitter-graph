# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.10.3 -- 2023-06-01

### DSL

#### Added

- Scoped variables can be inherited by child nodes by specifying `inherit .var_name` as part of the source. Using `@node.var_name` will either use the value set on the node itself, or otherwise the one set on the closest enclosing node.

## v0.10.2 -- 2023-05-25

### Library

#### Added

- Edge debug information now contains the TSG location of the `edge` statement if `ExecutionConfig::location_attr` is set.

#### Changed

- The TSG location in the debug information is formatted more explicitly as `line XX column YY` instead of `(XX, YY)`.

## v0.10.1 -- 2023-05-15

### Library

#### Added

- The `ast::File::try_visit_matches` method can be used to execute the stanza query matching without executing the stanza bodies.

## v0.10.0 -- 2023-05-10

### DSL

#### Changed

- Unused captures in patterns are now considered errors, unless they start with an underscore.

### Library

#### Fixed

- Some execution errors were not always reported in lazy execution mode, depending on whether variables were used or not. This has been fixed to behave more consistently, so that errors are always reported regardless of variable use.

### CLI

#### Added

- Errors from setting a scoped variable twice on the same node are now reported with source snippets for both statements involved.

## v0.9.2 -- 2023-04-14

### Library

#### Added

- The `parse_error::Excerpt` type, which is used to show source excerpts, is now public.

## v0.9.1 -- 2023-04-03

### CLI

#### Added

- Errors from parsing and checking the TSG file are now displayed with source excerpts.

## v0.9.0 -- 2023-03-31

### Library

#### Changed

- The method `ParseError::display(source: &str, verbose: bool)` has been replaced by two methods `ParseError::display(path: &path, source: &str)` and `ParseError::display_pretty(path: &Path, source: &str)`.

### CLI

#### Changed

- Parse errors are now displayed with the same excerpt style as execution errors.

## 0.8.0 -- 2023-03-29

### Library

#### Fixed

- Fix error formatting panic with strict execution.

#### Changed

- The traits stored in `Functions` must now implement `Send` and `Sync` to ensure they can be shared more easily in concurrent situations.

## 0.7.0 -- 2022-10-18

### DSL

#### Added

- Global variables can be given default string values using `global NAME = "STRING"` syntax.
- Function `eq` to compare two values for equality.
- Function `format` to format strings.
- Function `concat` to concatenate lists.
- Function `join` to join a list of values using an optional separator.

#### Removed

- References to undefined variables now result in errors, instead of assuming its an undeclared global variable.

### Library

#### Fixed

- Query errors now report the correct column for errors on the first row of a query.

#### Added

- The `Variables` type has additional methods `is_empty` and `iter`.

#### Changed

- References to `ExecutionConfig` passed to `File::execute*` are not required to be mutable anymore.

## 0.6.1 -- 2022-09-06

### Library

#### Fixed

- Cancellation errors during execution are now propagated all the way up, instead of being wrapped in other errors.

#### Added

- The `ExecutionError` type now supports formatting errors with excerpts from the program and TSG source by using the `.display_pretty` method.

#### Changed

- `Variables::nested` does now accept a non-mutable reference.

### CLI

#### Added

- Execution errors now include excerpts from the program and TSG source when printed.

## 0.6.0 -- 20220-08-23

### Library

#### Added

- Cancellation of `ast::File` execution is now supported by passing an implementation of the `CancellationFlag` trait.
  The `NoCancellation` type provides a noop implementation.

#### Fixed

- Fixed bug in `Identifier` so `ast::File` instances can be safely shared between threads.

#### Changed

- Functions are not passed as mutable anymore, so that they can safely used concurrently and reused between executions.
- `ast::File::execute` requires an extra `CancellationFlag` parameter. Use `&NoCancellation` if no cancellation is required.

### CLI

#### Added

- Global variables can be provided by passing `--global VAR=VAL` flags. Only string values are supported.

## 0.5.1 -- 2022-05-11

### DSL

- Reference the new VS Code extension in the README.

## 0.5.0 -- 2022-05-09

### DSL

#### Fixed

- Report query errors with correct source locations.

### Library

#### Changed

- In JSON output, all values are represented as objects with a `type` field
  indicating the value type, and additional value fields that vary per type.

### CLI

#### Added

- Flag `--output`/`-o` to set JSON output path.

## 0.4.0 -- 2022-03-21

### DSL

#### Added

- Global variable declarations.
- Attribute shorthands.
- List and set comprehensions.

#### Deprecated

- Use of undeclared global variables.

### Library

#### Added

- Module `parse_error` for finding and displaying `tree-sitter` parse errors.
- Option in `ExecutionConfig` to automatically add debug graph node attributes that describe the source location and variable name of the originating `node` statement in the DSL source.

#### Changed

- Calls to `execute` will fail with a runtime error if declared global variables are missing in the global environment.
- Calls to `execute` will not fail early on parse trees with errors. Errors may occur during query execution if matched parts of the tree are missing.
- The seperate arguments to `execute` are replaced by a single `ExecutionConfig` argument, which makes it easier to add optional arguments without breaking all use sites.

#### Removed

- The `execute_lazy` method has been removed. Lazy evalaution is enabled by setting the `lazy` flag in the `ExecutionConfig`.

### CLI

#### Added

- Flag `--allow-parse-errors` that allows running against input files with parse errors.

#### Changed

- Improved formatting of parse error messages.

## 0.3.0 - 2022-02-08

### DSL

#### Added

- Loop statement to iterate over list values.
- Conditional statement to check optional and boolean values.
- Boolean functions `and`, `or`, and `not`.
- Lazy evaluation strategy that does not rely on textual stanza order.

#### Changed

- Variables are now scoped in blocks, and will not escape or overwrite variables defined in enclosing blocks.
- Fix nested function calls, which would lose their arguments.
- Fix nested `scan` statements, which would lose outer captures after the inner statement.
- Construct capture values based in query quantifiers, e.g., create a list for `@cap*` or an optional value for `@cap?`.
- Rename `child-index` to the more accurate `named-child-index`

#### Removed

- The `scan` statement cannot be applied to values that are not local to the stanza, i.e., values that depend on scoped variables.

### Library

#### Added

- Method `File::execute_lazy` to use lazy evaluation strategy.
- Methods `File::execute_into` and `File::execute_lazy_into` to use an existing graph instance, instead of creating a new one.
- Various `Value::into_*` and `Value::as_*` methods.

#### Changed

- The method `Graph::display_with` is renamed to `Graph::pretty_print`.

#### Deprecated

- The method `File::parse` is deprecated because it it not sound anymore, and should be replaced by the `File::from_str` method.
- The method `Value::into_syntax_node` is deprecated, and should be replaced by explicit graph indexing.

#### Removed

- The `Context` object for string interning is removed, along with the `ctx` parameters of methods.

### CLI

#### Added

- JSON output mode for the resulting graph, controlled by the `--json` flag.
- A `--lazy` flag to use the lazy evaluation strategy.
- A `--quiet` flag to suppress graph output.
