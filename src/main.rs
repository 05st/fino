use std::{
    collections::HashMap, fs::read_to_string, path::Path
};

use ast::Module;
use cache::CompilerCache;
use clap::Parser as _;
use modulesort::toposort_modules;
use parser::Parser;
use walkdir::WalkDir;

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

fn main() {
    let args = Args::parse();
    let root_path = Path::new(&args.src);

    let mut compiler_cache = CompilerCache {
        location_map: HashMap::new(),
    };

    let mut program: Vec<Module> = Vec::new();

    let files_iter = WalkDir::new(root_path)
        .into_iter()
        .map(|e| match e {
            Ok(entry) => entry,
            Err(err) => panic!("{}", err),
        }).filter(|e| {
            e.file_name()
                .to_str()
                .map(|s| s.ends_with(FINO_FILE_EXTENSION))
                .unwrap_or(false)
        });

    let mut parser = Parser::new(&mut compiler_cache);
    for entry in files_iter {
        let file_path = entry.path();
        let input = read_to_string(file_path).expect(format!("Failed to read file {:?}", file_path).as_str());

        let parse_result = parser.parse(&input, get_module_name(root_path, file_path));
        match parse_result {
            Ok(module) => program.push(module),
            Err(err) => todo!(),
        }
    }

    match toposort_modules(program) {
        Ok(sorted_program) => println!("{:?}", sorted_program),
        Err(sort_err) => todo!(),
    }
}
