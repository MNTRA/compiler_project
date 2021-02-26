#![feature(core_intrinsics)]
#![feature(range_bounds_assert_len)]
#![feature(associated_type_defaults)]

use parser::ast;

pub mod parser;
pub mod util;

fn main() {
    let source = load_source_from_file("test_file.code");
    let parser = ast::Parser::new();
    let _ast = parser.parse(&source);
}

#[allow(dead_code)]
fn get_line() -> Option<String> {
    use std::io;
    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(_) => Some(buffer),
        Err(e) => panic!(e),
    }
}

fn load_source_from_file(filename: &str) -> String {
    let package_dir = env!("CARGO_MANIFEST_DIR");
    match std::fs::read_to_string(format!("{}\\{}", package_dir, filename)) {
        Ok(text) => text,
        Err(e) => panic!(e),
    }
}