use std::any::Any;

use collections::stream::{
    Dynamic,
    DynamicError,
};
use lexer::{
    SyntaxToken,
    SyntaxTokenStream,
    SyntaxTokenType,
};
use thiserror::Error;

use crate::{Parser, Token, parsers::expressions::Expr};

pub type ParseResult<T> = Result<T, ParseError>;

pub struct SyntaxTokenParser<'src> {
    stream: ParseStream<'src>,
}

impl<'src> SyntaxTokenParser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            stream: ParseStream::new(src),
        }
    }

    pub fn parse(mut self) -> ParseResult<()> {
        let mut stream = self.stream;
        let expr = stream.parse::<Expr>()?;
        stream.parse::<Token![";"]>()?;
        println!("{:#?}", expr);
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("NoTokens")]
    EndOfTokenStream,
    #[error("UnexpectedToken [{0}]")]
    UnexpectedToken(SyntaxTokenType),
}


pub struct ParseStream<'src> {
    stream: Dynamic<SyntaxTokenStream<'src>>,
    data : Option<Box<dyn Any>>,
}

impl<'src> ParseStream<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            stream: Dynamic::new(SyntaxTokenStream::new(src)),
            data: None,
        }
    }

    /// Calls `Parser::parse` on `T` by first clearing any whitespace tokens in
    /// the stream until a token is found. This is useful when parsing
    /// syntax that doesn't care about its surrounding whitespace.
    pub fn parse<T: Parser<'src>>(&mut self) -> ParseResult<T::Output> {
        self.consume_whitespace()?;
        self.parse_immediate::<T>()
    }

    /// Calls `Parser::parse` on `T` without clearing any whitespace tokens
    /// before it. This is useful when parsing syntax that consists of
    /// multiple tokens that cannot be seperated by whitespace e.g `==`,
    /// `<<`, `+=`
    pub fn parse_immediate<T: Parser<'src>>(&mut self) -> ParseResult<T::Output> {
        match T::parse(&mut *self) {
            Ok(item) => Ok(item),
            Err(e) => {
                println!("{:?}", &e);
                Err(e)
            }
        }
    }

    /// Calls `Parser::parse` on `T` by first clearing any whitespace tokens in
    /// the stream until a token is found. This is useful when parsing
    /// syntax that doesn't care about its surrounding whitespace.
    pub fn peek(&mut self, offset: usize) -> ParseResult<SyntaxToken<'src>> {
        loop {
            let token = self.peek_immediate(offset)?;
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(_) => {
                    continue;
                },
                _ => {
                    return Ok(token);
                },
            }
        }
    }

    /// Calls `Parser::parse` on `T` without clearing any whitespace tokens
    /// before it. This is useful when parsing syntax that consists of
    /// multiple tokens that cannot be seperated by whitespace e.g `==`,
    /// `<<`, `+=`
    pub fn peek_immediate(&mut self, offset: usize) -> ParseResult<SyntaxToken<'src>> {
        match self.stream.peek(offset) {
            // TODO (George): Can we avoid the clone??
            // Does cloning this even matter??
            Ok(item) => Ok(item.clone()),
            Err(_) => Err(ParseError::EndOfTokenStream),
        }
    }


    pub fn store_data (&mut self, val: impl Any) {
        self.data = Some(Box::new(val));
    }

    pub fn get_data<T: 'static> (&mut self) -> Box<T> {
        let boxed_any = std::mem::take(&mut self.data)
            .expect("Data was None");

        match boxed_any.downcast::<T>() {
            Ok(boxed_t) => { boxed_t },
            Err(_) => {
                panic!("Unable to downcast T")
            },
        }
    }


    fn consume_whitespace(&mut self) -> ParseResult<()> {
        loop {
            let token = self.peek_immediate(0)?;
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(_) => {
                    self.consume();
                },
                _ => {
                    return Ok(());
                },
            }
        }
    }

    pub fn consume(&mut self) {
        let token = self.stream.next().unwrap();

        match token.ty {
            SyntaxTokenType::Whitespace | SyntaxTokenType::Control(_) => {
            },
            _ => {
                println!("Consumed: \"{}\"", token.data.src);
            },
        }

        //self.span.set_end(token.data.span.end());
    }

    pub fn print_token(&mut self) {
        println!(
            "{:#?}",
            self.stream.peek(0).expect("This should never panic")
        )
    }
}
