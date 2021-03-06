use std::marker::PhantomData;

use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    Parser,
};

pub mod common;

#[derive(Default, Debug)]
pub struct Punctuated<T, U> {
    _marker: PhantomData<(fn() -> T, fn() -> U)>,
}

impl<'a, T, U> Parser<'a> for Punctuated<T, U>
where
    T: Parser<'a>,
    U: Parser<'a>,
{
    type Output = Vec<T::Output>;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = Vec::with_capacity(3);
        loop {
            let item = stream.parse::<T>()?;
            out.push(item);
            match stream.parse::<U>() {
                Ok(_) => continue,
                Err(_) => break,
            };
        }
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct Enclosed<T, X, U> {
    _marker: PhantomData<(fn() -> T, fn() -> X, fn() -> U)>,
}

impl<'a, T, X, U> Parser<'a> for Enclosed<T, X, U>
where
    T: Parser<'a>,
    X: Parser<'a>,
    U: Parser<'a>,
{
    type Output = X::Output;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let out: Self::Output;
        stream.parse::<T>()?;
        out = stream.parse::<X>()?;
        stream.parse::<U>()?;
        Ok(out)
    }
}

impl<'a, T> Parser<'a> for Option<T>
where
    T: Parser<'a>,
{
    type Output = Option<T::Output>;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        match stream.parse::<T>() {
            Ok(item) => return Ok(Some(item)),
            Err(err) => match err {
                ParseError::UnexpectedToken(_) => Ok(None),
                ParseError::EndOfTokenStream => Err(err),
            },
        }
    }
}
