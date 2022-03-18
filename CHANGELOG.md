# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### DSL

#### Added

- Global variable declarations.
- Attribute shorthands.

#### Deprecated

- Use of undeclared global variables.

### Library

#### Changed

- Calls to `execute` will fail with a runtime error if declared global variables are missing in the global environment.

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
