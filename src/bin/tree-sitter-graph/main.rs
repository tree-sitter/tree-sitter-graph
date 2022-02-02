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
use clap::App;
use clap::Arg;
use tree_sitter::Parser;
use tree_sitter_config::Config;
use tree_sitter_graph::ast::File;
use tree_sitter_graph::functions::Functions;
use tree_sitter_graph::Context;
use tree_sitter_graph::Variables;
use tree_sitter_loader::Loader;

const BUILD_VERSION: &'static str = env!("CARGO_PKG_VERSION");

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
                .short("q")
                .long("quiet")
                .help("Suppress console output"),
        )
        .arg(
            Arg::with_name("lazy")
                .short("z")
                .long("lazy")
                .help("Use lazy evaluation (experimental)"),
        )
        .arg(Arg::with_name("scope").long("scope").takes_value(true))
        .arg(Arg::with_name("json").long("json").takes_value(false))
        .get_matches();

    let tsg_path = Path::new(matches.value_of("tsg").unwrap());
    let source_path = Path::new(matches.value_of("source").unwrap());
    let current_dir = std::env::current_dir().unwrap();
    let quiet = matches.is_present("quiet");
    let lazy = matches.is_present("lazy");
    let config = Config::load()?;
    let mut loader = Loader::new()?;
    let loader_config = config.get()?;
    loader.find_all_languages(&loader_config)?;
    let language = loader.select_language(source_path, &current_dir, matches.value_of("scope"))?;
    let mut parser = Parser::new();
    parser.set_language(language)?;
    let tsg = std::fs::read(tsg_path)
        .with_context(|| format!("Error reading TSG file {}", tsg_path.display()))?;
    let tsg = String::from_utf8(tsg)?;
    let source = std::fs::read(source_path)
        .with_context(|| format!("Error reading source file {}", source_path.display()))?;
    let source = String::from_utf8(source)?;
    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Could not parse {}", source_path.display()))?;
    let mut ctx = Context::new();
    let file = File::from_source(language, &mut ctx, &tsg)
        .with_context(|| anyhow!("Error parsing TSG file {}", tsg_path.display()))?;
    let mut functions = Functions::stdlib(&mut ctx);
    let globals = Variables::new();
    let graph = if lazy {
        file.execute_lazy(&mut ctx, &tree, &source, &mut functions, &globals)
    } else {
        file.execute(&mut ctx, &tree, &source, &mut functions, &globals)
    }
    .with_context(|| anyhow!("Could not execute TSG file {}", tsg_path.display()))?;
    let json = matches.is_present("json");
    if json {
        graph.display_json(&ctx);
    } else if !quiet {
        print!("{}", graph.display_with(&ctx));
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
