use std::path::Path;

use cache::CompilerCache;
use clap::Parser as _;
use llvm::compile_llvm;
use error::Error;
use parser::parse_program;
use resolver::resolve_program;
use sorter::sort_program;
use transformer::transform_program;
use typechecker::typecheck_program;
use walkdir::{DirEntry, WalkDir};

mod ast;
mod cache;
mod llvm;
mod error;
mod lexer;
mod literal;
mod location;
mod mir;
mod parser;
mod resolver;
mod sorter;
mod transformer;
mod typechecker;
mod types;

const FINO_FILE_EXTENSION: &str = ".fn";

#[derive(clap::Parser, Debug)]
struct Args {
    // Input path
    #[arg(help = "Input path")]
    src: String,

    // Output path
    #[arg(short, long, default_value = "a.bc", help = "Output path")]
    out: String,
}

fn collect_fino_files(root: &Path) -> Vec<DirEntry> {
    WalkDir::new(root)
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
        .collect::<Vec<DirEntry>>()
}

fn run_compiler(files: Vec<DirEntry>, root: &Path, out: &Path) -> Result<(), Error> {
    let mut compiler_cache = CompilerCache::default();

    parse_program(&mut compiler_cache, files, root)?;
    sort_program(&mut compiler_cache)?;
    resolve_program(&mut compiler_cache)?;
    typecheck_program(&mut compiler_cache)?;

    let mir = transform_program(&mut compiler_cache);
    compile_llvm(mir, out);

    Ok(())
}

fn main() {
    let args = Args::parse();
    let root = Path::new(&args.src);
    let out = Path::new(&args.out);

    let files = collect_fino_files(root);

    if let Err(err) = run_compiler(files, root, out) {
        err.report();
    }
}
