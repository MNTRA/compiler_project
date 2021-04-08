use std::marker::PhantomData;

use diagnostics::Reporter;

use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    Parse,
};

/// Combinators are Types that provide generalised parsing functionality this
/// isnt specialied to any particular syntax.

#[derive(Default, Debug)]
pub struct Punctuated<T, U> {
    _marker: PhantomData<(fn() -> T, fn() -> U)>,
}

impl<'a, T, U> Parse<'a> for Punctuated<T, U>
where
    T: Parse<'a>,
    U: Parse<'a>,
{
    type Output = Vec<T::Output>;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = Vec::with_capacity(3);
        loop {
            let item = stream.parse::<T>(reporter)?;
            out.push(item);
            match stream.parse::<U>(reporter) {
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

impl<'a, T, X, U> Parse<'a> for Enclosed<T, X, U>
where
    T: Parse<'a>,
    X: Parse<'a>,
    U: Parse<'a>,
{
    type Output = X::Output;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let out: Self::Output;
        stream.parse::<T>(reporter)?;
        out = stream.parse::<X>(reporter)?;
        stream.parse::<U>(reporter)?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct Seperated<T, X, U> {
    _marker: PhantomData<(fn() -> T, fn() -> X, fn() -> U)>,
}

impl<'a, T, X, U> Parse<'a> for Seperated<T, X, U>
where
    T: Parse<'a>,
    X: Parse<'a>,
    U: Parse<'a>,
{
    type Output = (T::Output, U::Output);
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let lhs_out = stream.parse::<T>(reporter)?;
        stream.parse::<X>(reporter)?;
        let rhs_out = stream.parse::<U>(reporter)?;
        Ok((lhs_out, rhs_out))
    }
}

impl<'a, T> Parse<'a> for Option<T>
where
    T: Parse<'a>,
{
    type Output = Option<T::Output>;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        match stream.parse::<T>(reporter) {
            Ok(item) => return Ok(Some(item)),
            Err(err) => match err {
                ParseError::UnexpectedToken(_) => Ok(None),
                ParseError::EndOfTokenStream => Err(err),
            },
        }
    }
}

impl<'a, T> Parse<'a> for Vec<T>
where
    T: Parse<'a>,
{
    type Output = Vec<T::Output>;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out: Vec<T::Output> = Vec::new();
        loop {
            match stream.parse::<T>(reporter) {
                Ok(item) => out.push(item),
                Err(err) => match err {
                    ParseError::UnexpectedToken(_) => return Ok(out),
                    ParseError::EndOfTokenStream => return Err(err),
                },
            }
        }
    }
}
