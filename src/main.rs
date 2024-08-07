use std::{
    collections::HashMap, fs::read_to_string, path::Path
};

use cache::CompilerCache;
use clap::Parser as _;
use error::Error;
use modulesort::toposort_modules;
use parser::Parser;
use walkdir::{DirEntry, WalkDir};

mod ast;
mod cache;
mod error;
mod inference;
mod lexer;
mod modulesort;
mod parser;
mod resolution;
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
        vec![String::from(file
            // Remove .fn extension
            .with_extension("")
            .file_name()
            .expect("Failed to get file name")
            .to_str()
            .expect("Failed to convert OsStr to str"))]
    } else {
        file
            // Remove .fn extension
            .with_extension("")
            .to_path_buf()
            // Remove path prefix from root
            .strip_prefix(root)
            .expect("Failed to strip root path prefix")
            .components()
            // Map to collection of strings
            .map(|c|
                String::from(c
                    .as_os_str()
                    .to_str()
                    .expect("Failed to convert OsStr to str")))
            .collect()
    }
}

fn run_compiler(files: Vec<DirEntry>, root: &Path) -> Result<(), Error> {
    let mut compiler_cache = CompilerCache {
        location_map: HashMap::new(),
    };

    let mut parser = Parser::new(&mut compiler_cache);
    let mut program = Vec::new();

    for file in files {
        let path = file.path();
        let source = read_to_string(path).expect(format!("Failed to read file {:?}", path).as_str());
    
        program.push(parser.parse(&source, get_module_name(root, path), path.to_path_buf())?);
    }

    program = toposort_modules(&mut compiler_cache, program)?;

    println!("{:?}", program);

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
        }).filter(|e| {
            e.file_name()
                .to_str()
                .map(|s| s.ends_with(FINO_FILE_EXTENSION))
                .unwrap_or(false)
        }).collect::<Vec<DirEntry>>();
    
    if let Err(err) = run_compiler(files, root) {
        err.report();
    }
}
