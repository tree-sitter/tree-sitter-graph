// -*- coding: utf-8 -*-
// ------------------------------------------------------------------------------------------------
// Copyright © 2021, tree-sitter authors.
// Licensed under either of Apache License, Version 2.0, or MIT license, at your option.
// Please see the LICENSE-APACHE or LICENSE-MIT files in this distribution for license details.
// ------------------------------------------------------------------------------------------------

use std::path::Path;

use anyhow::anyhow;
use anyhow::Context as _;
use anyhow::Result;
use clap::builder::ArgAction;
use clap::App;
use clap::Arg;
use tree_sitter::Parser;
use tree_sitter_config::Config;
use tree_sitter_graph::ast::File;
use tree_sitter_graph::functions::Functions;
use tree_sitter_graph::graph;
use tree_sitter_graph::parse_error::ParseError;
use tree_sitter_graph::CancellationFlags;
use tree_sitter_graph::ExecutionConfig;
use tree_sitter_graph::Identifier;
use tree_sitter_graph::Variables;
use tree_sitter_loader::Loader;

const BUILD_VERSION: &'static str = env!("CARGO_PKG_VERSION");

const MAX_PARSE_ERRORS: usize = 5;

fn main() -> Result<()> {
    init_log();

    let matches = App::new("tree-sitter-graph")
        .version(BUILD_VERSION)
        .author("Douglas Creager <dcreager@dcreager.net>")
        .about("Generates graph structures from tree-sitter syntax trees")
        .arg(Arg::with_name("tsg").index(1).required(true))
        .arg(Arg::with_name("source").index(2).required(true))
        .arg(
            Arg::with_name("quiet")
                .short('q')
                .long("quiet")
                .help("Suppress console output"),
        )
        .arg(
            Arg::with_name("lazy")
                .short('z')
                .long("lazy")
                .help("Use lazy evaluation (experimental)"),
        )
        .arg(Arg::with_name("scope").long("scope").takes_value(true))
        .arg(Arg::with_name("json").long("json").takes_value(false))
        .arg(
            Arg::with_name("output")
                .short('o')
                .long("output")
                .requires("json")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("allow-parse-errors")
                .long("allow-parse-errors")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("global")
                .long("global")
                .takes_value(true)
                .action(ArgAction::Append),
        )
        .get_matches();

    let tsg_path = Path::new(matches.value_of("tsg").unwrap());
    let source_path = Path::new(matches.value_of("source").unwrap());
    let current_dir = std::env::current_dir().unwrap();
    let quiet = matches.is_present("quiet");
    let lazy = matches.is_present("lazy");
    let globals = matches.get_many::<String>("global").unwrap_or_default();
    let mut globals_ = Variables::new();
    for kv in globals {
        let kv_ = kv
            .split_once('=')
            .with_context(|| format!("Expected key-value pair separated by '=', got {}.", kv))?;
        globals_.add(
            Identifier::from(kv_.0),
            graph::Value::String(kv_.1.to_string()),
        )?;
    }

    let config = Config::load()?;
    let mut loader = Loader::new()?;
    let loader_config = config.get()?;
    loader.find_all_languages(&loader_config)?;
    let language = loader.select_language(source_path, &current_dir, matches.value_of("scope"))?;

    let tsg = std::fs::read(tsg_path)
        .with_context(|| format!("Cannot read TSG file {}", tsg_path.display()))?;
    let tsg = String::from_utf8(tsg)?;
    let file = File::from_str(language, &tsg)
        .with_context(|| format!("Cannot parsing TSG file {}", tsg_path.display()))?;

    let source = std::fs::read(source_path)
        .with_context(|| format!("Cannot read source file {}", source_path.display()))?;
    let source = String::from_utf8(source)?;
    let mut parser = Parser::new();
    parser.set_language(language)?;
    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Cannot parse {}", source_path.display()))?;
    let allow_parse_errors = matches.is_present("allow-parse-errors");
    if !allow_parse_errors {
        let parse_errors = ParseError::all(&tree);
        if !parse_errors.is_empty() {
            for parse_error in parse_errors.iter().take(MAX_PARSE_ERRORS) {
                let line = parse_error.node().start_position().row;
                let column = parse_error.node().start_position().column;
                eprintln!(
                    "{}:{}:{}: {}",
                    source_path.display(),
                    line + 1,
                    column + 1,
                    parse_error.display(&source, true)
                );
            }
            if parse_errors.len() > MAX_PARSE_ERRORS {
                let more_errors = parse_errors.len() - MAX_PARSE_ERRORS;
                eprintln!(
                    "{} more parse error{} omitted",
                    more_errors,
                    if more_errors > 1 { "s" } else { "" },
                );
            }
            return Err(anyhow!("Cannot parse {}", source_path.display()));
        }
    }

    let functions = Functions::stdlib();
    let mut config = ExecutionConfig::new(&functions, &globals_).lazy(lazy);
    let graph = file
        .execute(&tree, &source, &mut config, &CancellationFlags::none())
        .with_context(|| format!("Cannot execute TSG file {}", tsg_path.display()))?;

    let json = matches.is_present("json");
    let output_path = matches.value_of("output").map(|str| Path::new(str));
    if json {
        graph.display_json(output_path).unwrap_or(());
    } else if !quiet {
        print!("{}", graph.pretty_print());
    }

    Ok(())
}

fn init_log() {
    let _ = env_logger::builder()
        .format_level(false)
        .format_target(false)
        .format_timestamp(None)
        .init();
}
