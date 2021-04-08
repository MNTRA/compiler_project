#![feature(
    const_fn,
    format_args_capture
)]


mod module;
mod parser;
mod state;
mod function;
mod expressions;

use std::sync::Arc;

use diagnostics::DiagnosticBuilder;
use errors::{
    CompilerBug,
    FatalError,
};
use lexer::{
    console_printer::CONSOLE_PRINTER,
    SyntaxTokenStream,
};
use parser::Parser;

pub async fn parse_root<'a>(info: ParserInfo<'a>) -> Result<(), ()> {
    pretty_print_tokens(info.source);
    Parser::from(info).parse_module().await.map_err(
        |builder| builder.with_title("Invalid Syntax").report()
    )
}

pub struct ParserInfo<'a> {
    source: &'a String,
}

#[derive(Default)]
pub struct ParseInfoBuilder<'a> {
    source: Option<&'a String>,
}

impl<'a> ParseInfoBuilder<'a> {
    pub fn new() -> Self {
        Self {
            source: None,
        }
    }

    pub fn source(
        mut self,
        source: &'a String,
    ) -> Self {
        if self.source.is_none() {
            self.source = Some(source);
        } else {
            CompilerBug::raise("Tried to call source() more than once", line!(), file!());
        }
        self
    }

    pub fn build(self) -> ParserInfo<'a> {
        ParserInfo {
            source: self.source.expect("Source cannot be none."),
        }
    }
}

fn pretty_print_tokens(src: &String) {
    let mut console = CONSOLE_PRINTER.lock().unwrap();
    let mut stream = SyntaxTokenStream::new(&src);
    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(src, token) }
        if token.is_none() {
            break;
        }
    }
}

fn print_tokens(src: &String) {
    let stream = SyntaxTokenStream::new(src);
    for token in stream {
        println!("{:#?}", token.ty);
    }
}
