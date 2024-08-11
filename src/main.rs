use std::{collections::HashMap, fs::read_to_string, path::Path};

use cache::CompilerCache;
use clap::Parser as _;
use error::Error;
use lexer::tokenize;
use parser::Parser;
use resolver::resolve_program;
use sorter::sort_program;
use typechecker::typecheck_program;
use walkdir::{DirEntry, WalkDir};

mod ast;
mod cache;
mod error;
mod lexer;
mod parser;
mod resolver;
mod sorter;
mod typechecker;
mod types;

const FINO_FILE_EXTENSION: &str = ".fn";

#[derive(clap::Parser, Debug)]
struct Args {
    // Input path
    #[arg(help = "Input path")]
    src: String,

    // Output path
    #[arg(short, long, default_value = "a", help = "Output path")]
    out: String,
}

fn get_module_name(root: &Path, file: &Path) -> Vec<String> {
    if file == root {
        vec![String::from(
            file.with_extension("")
                .file_name()
                .expect("Failed to get file name")
                .to_str()
                .expect("Failed to convert OsStr to str"),
        )]
    } else {
        file.with_extension("")
            .to_path_buf()
            .strip_prefix(root)
            .expect("Failed to strip root path prefix")
            .components()
            .map(|c| {
                String::from(
                    c.as_os_str()
                        .to_str()
                        .expect("Failed to convert OsStr to str"),
                )
            })
            .collect()
    }
}

fn run_compiler(files: Vec<DirEntry>, root: &Path) -> Result<(), Error> {
    let mut compiler_cache = CompilerCache::new();

    let mut operator_map = HashMap::new();
    let mut parser_inputs = Vec::new();

    // Tokenize all files and collect all operator declarations before parsing
    for file in files {
        let path = file.path();
        let source = read_to_string(path).expect(format!("Failed to read file {:?}", path).as_str());

        let tokens = tokenize(&source, path.to_path_buf(), &mut operator_map)?;
        parser_inputs.push((tokens, path.to_path_buf()));
    }

    let mut parser = Parser::new(&mut compiler_cache, operator_map);
    let mut program = Vec::new();

    for (tokens, path) in parser_inputs {
        program.push(parser.parse_module(
            tokens,
            get_module_name(root, path.as_path()),
            path,
        )?);
    }

    sort_program(&mut compiler_cache, &mut program)?;
    resolve_program(&mut compiler_cache, &mut program)?;
    typecheck_program(&mut compiler_cache, &program)?;

    println!("{:#?}", program);
    println!("{:?}", compiler_cache.expr_type_map);

    Ok(())
}

fn main() {
    let args = Args::parse();
    let root = Path::new(&args.src);

    let files = WalkDir::new(root)
        .into_iter()
        .map(|e| match e {
            Ok(entry) => entry,
            Err(err) => panic!("{}", err),
        })
        .filter(|e| {
            e.file_name()
                .to_str()
                .map(|s| s.ends_with(FINO_FILE_EXTENSION))
                .unwrap_or(false)
        })
        .collect::<Vec<DirEntry>>();

    if let Err(err) = run_compiler(files, root) {
        err.report();
    }
}
