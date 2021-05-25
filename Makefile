.PHONY: all check

all: grammar/src/parser.c
	cargo build

check: grammar/src/parser.c
	cd grammar && tree-sitter test
	cargo test

grammar/src/parser.c: grammar/package.json grammar/grammar.js
	cd grammar && npm install
	cd grammar && tree-sitter generate
