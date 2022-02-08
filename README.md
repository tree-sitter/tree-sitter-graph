# tree-sitter-graph

The `tree-sitter-graph` library defines a DSL for constructing arbitrary graph
structures from source code that has been parsed using [tree-sitter][].

[tree-sitter]: https://tree-sitter.github.io/

- [API documentation](https://docs.rs/tree-sitter-graph/)
- [Release notes](https://github.com/tree-sitter/tree-sitter-graph/blob/main/CHANGELOG.md)

## Usage

This package can be used either as a library or command-line program.

To use it as a library, add the following to your `Cargo.toml`:

``` toml
[dependencies]
tree-sitter-graph = "0.3"
```

To use it as a program, install it via `cargo install`:

```
$ cargo install --features cli tree-sitter-graph
$ tree-sitter-graph --help
```

## Development

The project is written in Rust, and requires a recent version installed.
Rust can be installed and updated using [rustup][].

[rustup]: https://rustup.rs/

Build the project by running:

```
$ cargo build
```

Run the tests by running:

```
$ cargo test
```

The project consists of a library and a CLI.
By default, running `cargo` only applies to the library.
To run `cargo` commands on the CLI as well, add `--features cli` or `--all-features`.

Sources are formatted using the standard Rust formatted, which is applied by running:

```
$ cargo fmt
```