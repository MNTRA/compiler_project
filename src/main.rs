use lexer::{
    console_printer::CONSOLE_PRINTER,
    syntax_token_stream::SyntaxTokenStream
};

fn main() {
    let code = include_str!("../test_file.code");
    let mut stream = SyntaxTokenStream::new(code);
    
    let mut console = CONSOLE_PRINTER.lock().unwrap();

    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(token) }
        if let None = token {
            break;
        }
    }
}
