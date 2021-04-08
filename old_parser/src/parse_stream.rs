use std::any::Any;

use collections::stream::Dynamic;
use diagnostics::Reporter;
use lexer::{
    SyntaxToken,
    SyntaxTokenStream,
    SyntaxTokenType,
};
use thiserror::Error;

use crate::{
    ast::Ast,
    parsers::common::GlobalScope,
    Parse,
};

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'src> {
    stream: ParseStream<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            stream: ParseStream::new(src),
        }
    }

    pub fn parse(
        mut self,
        reporter: Reporter,
    ) -> ParseResult<Ast> {
        let result = self.stream.parse::<GlobalScope>(&mut reporter);
        match result {
            Ok(scope) => Ok(Ast::from(scope)),
            Err(err) => {
                println!("{:#?}", err);
                Err(err)
            },
        }
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("NoTokens")]
    EndOfTokenStream,
    #[error("UnexpectedToken [{0}]")]
    UnexpectedToken(SyntaxTokenType),
}

pub trait ParseClosure<'a, T>: FnMut(&mut ParseStream<'a>) -> ParseResult<T> {}

pub struct ParseStream<'src> {
    stream: Dynamic<SyntaxTokenStream<'src>>,
    data: Option<Box<dyn Any>>,
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
    pub fn parse<T: Parse<'src>>(
        &mut self,
        reporter: &mut Reporter,
    ) -> ParseResult<T::Output> {
        self.consume_whitespace()?;
        self.parse_immediate::<T>(reporter)
    }

    /// Calls `Parser::parse` on `T` without clearing any whitespace tokens
    /// before it. This is useful when parsing syntax that consists of
    /// multiple tokens that cannot be seperated by whitespace e.g `==`,
    /// `<<`, `+=`
    pub fn parse_immediate<T: Parse<'src>>(
        &mut self,
        reporter: &mut Reporter,
    ) -> ParseResult<T::Output> {
        T::parse(&mut *self, reporter)
    }

    pub fn parse_call<'a, T>(
        &mut self,
        mut func: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        func(self)
    }

    pub fn peek(
        &mut self,
        mut offset: usize,
    ) -> ParseResult<SyntaxToken<'src>> {
        loop {
            let token = self.peek_immediate(offset)?;
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(_) => {
                    offset += 1;
                    continue;
                },
                _ => {
                    return Ok(token);
                },
            }
        }
    }

    pub fn peek_immediate(
        &mut self,
        offset: usize,
    ) -> ParseResult<SyntaxToken<'src>> {
        match self.stream.peek(offset) {
            // TODO (George): Can we avoid the clone??
            // Does cloning this even matter??
            Ok(item) => Ok(item.clone()),
            Err(_) => Err(ParseError::EndOfTokenStream),
        }
    }

    pub fn store_data(
        &mut self,
        val: impl Any,
    ) {
        self.data = Some(Box::new(val));
    }

    pub fn get_data<T: 'static>(&mut self) -> Box<T> {
        let boxed_any = std::mem::take(&mut self.data).expect("Data was None");

        match boxed_any.downcast::<T>() {
            Ok(boxed_t) => boxed_t,
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

    pub fn consume(&mut self) { let _ = self.stream.consume().unwrap(); }

    pub fn print_token(&mut self) {
        println!(
            "{:#?}",
            self.stream.peek(0).expect("This should never panic")
        )
    }
}

#[macro_export]
macro_rules! unexpected_token {
    () => {
        let token = stream.peek(0)?;
        Err(ParseError::UnexpectedToken(token.ty))
    };
    ($STREAM:ident) => {
        let token = $STREAM.peek(0)?;
        return Err(ParseError::UnexpectedToken(token.ty));
    };
}
