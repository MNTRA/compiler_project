#![feature(type_alias_impl_trait)]

pub mod ast;
pub mod parse_stream;
pub mod parsers;
pub mod tokens;

use lexer::{
    console_printer::CONSOLE_PRINTER,
    syntax_token_stream::SyntaxTokenStream,
};

pub fn run() {
    let code = include_str!("../../test_file.code");

    let mut stream = SyntaxTokenStream::new(code);
    let mut console = CONSOLE_PRINTER.lock().unwrap();

    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(token) }
        //std::thread::sleep(std::time::Duration::from_micros(40));
        if let None = token {
            break;
        }
    }
    let ast = parse_stream::SyntaxTokenParser::new(code).parse().unwrap();
    println!("{:#?}", ast);
}

use parse_stream::{
    ParseResult,
    ParseStream,
};

pub trait Parser<'src> {
    type Output;
    fn parse(stream: &mut ParseStream<'src>) -> ParseResult<Self::Output>;
}
