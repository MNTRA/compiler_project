use std::{path::Path, rc::Rc, sync::Arc};
use alloc_counter::{
    AllocCounter,
    count_alloc
};

const GLOBAL_ALLOCATOR: mimalloc::MiMalloc =  mimalloc::MiMalloc;

#[global_allocator]
static A: AllocCounter<mimalloc::MiMalloc> = AllocCounter(GLOBAL_ALLOCATOR);

const TEST_FILE_PATH: &str = "test_file.code";

fn main() {
    let info = driver::CompilerInfo {
        source: std::fs::read_to_string(TEST_FILE_PATH).unwrap(),
        file_path: Arc::from(Path::new(TEST_FILE_PATH)),
    };

    let (a, _) = count_alloc(|| {
        driver::compiler_main(info);
    });

    println!("Allocs: {}", a.0);
    println!("Reallocs: {}", a.1);
    println!("Deallocs: {}", a.2);
}
