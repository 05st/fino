use std::path::Path;

use cache::CompilerCache;
use clap::Parser as _;
use error::Error;
use parser::parse_program;
use resolver::resolve_program;
use sorter::sort_program;
// use typechecker::typecheck_program;
use walkdir::{DirEntry, WalkDir};

mod ast;
mod cache;
mod error;
mod lexer;
mod parser;
mod resolver;
mod sorter;
// mod typechecker;
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

fn run_compiler(files: Vec<DirEntry>, root: &Path) -> Result<(), Error> {
    let mut compiler_cache = CompilerCache::default();

    parse_program(&mut compiler_cache, files, root)?;
    sort_program(&mut compiler_cache)?;
    resolve_program(&mut compiler_cache)?;
    println!("{:#?}", compiler_cache.modules);
    // typecheck_program(&mut compiler_cache)?;

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
