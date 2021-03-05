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

use crate::{Parser, ast::Ast, parsers::scopes::GlobalScope};

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

    pub fn parse(mut self) -> ParseResult<Ast> {
        let global_scope = self.stream.parse::<GlobalScope>()?;
        Ok(Ast::from(global_scope))
    }
}


#[derive(Error, Debug)]
pub enum ParseError {
    #[error("NoTokens")]
    EndOfTokenStream,
    #[error("UnexpectedToken")]
    UnexpectedToken(SyntaxTokenType),
}

pub struct ParseStream<'src> {
    stream: Dynamic<SyntaxTokenStream<'src>>,
}

impl<'src> ParseStream<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            stream: Dynamic::new(SyntaxTokenStream::new(src)),
        }
    }

    // Keywords
    pub fn parse_keyword(&mut self) -> ParseResult<()> {
        type ST = SyntaxTokenType;
        let token = self.get_next_token()?;
        match token.ty {
            ST::Punctuation(_) => {
                self.consume();
                return Ok(());
            },
            _ => return Err(ParseError::UnexpectedToken(token.ty)),
        }
    }

    // Punctuation
    pub fn parse_punctuation(&mut self) -> ParseResult<()> {
        type ST = SyntaxTokenType;
        let token = self.get_next_token()?;
        match token.ty {
            ST::Punctuation(_) => {
                self.consume();
                return Ok(());
            },
            _ => return Err(ParseError::UnexpectedToken(token.ty)),
        }
    }

    // Literals
    pub fn parse_literal(&mut self) -> ParseResult<()> {
        type ST = SyntaxTokenType;
        let token = self.get_next_token()?;
        match token.ty {
            ST::Control(_) => {
                self.consume();
                return Ok(());
            },
            _ => Err(ParseError::UnexpectedToken(token.ty)),
        }
    }

    // Whitespace
    pub fn parse_control(&mut self) -> ParseResult<()> {
        type ST = SyntaxTokenType;
        let token = self.get_next_token()?;
        match token.ty {
            ST::Control(_) => {
                self.consume();
                return Ok(());
            },
            _ => Err(ParseError::UnexpectedToken(token.ty)),
        }
    }



    pub fn consume(&mut self) {
        let _token = self.stream.next().unwrap();
        //self.span.set_end(token.data.span.end());
    }

    pub fn get_next_token(&mut self) -> ParseResult<SyntaxToken<'src>> {
        let result = self.stream.peek(0);
        match result {
            // TODO (George): Can we avoid this clone?
            // Does the clone even matter?
            Ok(token) => Ok(token.clone()),
            Err(e) => match e {
                DynamicError::EndOfStream => Err(ParseError::EndOfTokenStream),
                _ => unreachable!(),
            },
        }
    }

    pub fn skip_whitespace(&mut self) -> ParseResult<()> {
        loop {
            let token = self.get_next_token()?;
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(_) => {
                    let _ = self.stream.consume();
                },
                _ => {
                    return Ok(());
                },
            }
        }
    }

    /// Calls `Parser::parse` on `T` by first clearing any whitespace tokens in
    /// the stream until a token is found. This is useful when parsing
    /// syntax that doesn't care about its surrounding whitespace.
    pub fn parse<T: Parser<'src>>(&mut self) -> ParseResult<T::Output> {
        self.skip_whitespace()?;
        self.parse_immediate::<T>()
    }

    /// Calls `Parser::parse` on `T` without clearing any whitespace tokens
    /// before it. This is useful when parsing syntax that consists of
    /// multiple tokens that cannot be seperated by whitespace e.g `==`,
    /// `<<`, `+=`
    pub fn parse_immediate<T: Parser<'src>>(&mut self) -> ParseResult<T::Output> {
        T::parse(&mut *self)
    }
}
