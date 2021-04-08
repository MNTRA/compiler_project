#![feature(type_alias_impl_trait)]

pub mod ast;
mod errors;
pub mod parse_stream;
pub mod parsers;
pub mod tokens;

use diagnostics::Reporter;
use lexer::{
    console_printer::CONSOLE_PRINTER,
    syntax_token_stream::SyntaxTokenStream,
};

pub fn parse_string(
    src: &String,
    reporter: Reporter,
) {
    pretty_print_tokens(&src);

    let ast = parse_stream::Parser::new(src).parse(reporter).unwrap();
    println!("{:#?}", ast);
}

use parse_stream::{
    ParseResult,
    ParseStream,
};

pub trait Parse<'src> {
    type Output;
    fn parse(
        stream: &mut ParseStream<'src>,
        reporter: &mut Reporter,
    ) -> ParseResult<Self::Output>;
}

fn pretty_print_tokens(src: &str) {
    let mut console = CONSOLE_PRINTER.lock().unwrap();
    let mut stream = SyntaxTokenStream::new(src);
    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(token) }
        if token.is_none() {
            break;
        }
    }
}

fn print_tokens(src: &str) {
    let stream = SyntaxTokenStream::new(src);
    for token in stream {
        println!("{:#?}", token.ty);
    }
}
