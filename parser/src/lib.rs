pub mod ast;
pub mod tokens;
pub mod parsers;
pub mod parse_stream;

use lexer::{
    console_printer::CONSOLE_PRINTER,
    syntax_token_stream::SyntaxTokenStream,
};

pub fn run() {
    let code = include_str!("../../test_file.code");
    
    let mut stream = SyntaxTokenStream::new(code);
    let mut console = CONSOLE_PRINTER.lock().unwrap();
    
    let mut tokens = Vec::new();

    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(token) }
        tokens.push(token);
        //std::thread::sleep(std::time::Duration::from_micros(40));
        if let None = token {
            break;
        }
    }
    
    //println!("{:#?}", tokens);

    let ast = parse_stream::SyntaxTokenParser::new(code).parse().unwrap();
    println!("{:#?}", ast);
}

use parse_stream::{ParseResult, ParseStream};

pub trait Parser<'src>  {
    type Output;
    fn parse(stream: &mut ParseStream<'src>) -> ParseResult<Self::Output>;
}
