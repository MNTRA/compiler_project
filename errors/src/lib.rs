#![feature(format_args_capture)]

pub mod error_codes;
pub use error_codes::*;

#[derive(Debug)]
pub struct FatalError;
impl FatalError {
    pub fn raise() -> ! { std::panic::resume_unwind(Box::new(Self)) }
}

pub struct CompilerBug(String);
impl CompilerBug {
    pub fn raise(msg: impl Into<String>, line: u32, file: &str) -> ! {
        let msg = msg.into();
        std::panic::resume_unwind(Box::new(Self(
            format!("Compiler Bug: {msg} at line {line} in {file}")
        ))) 
    }
    pub fn description(&self) -> String {
        self.0.clone()
    }
}
impl std::fmt::Debug for CompilerBug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}