use std::marker::PhantomData;

use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    Parser,
    Token,
};

#[derive(Debug, Default)]
pub struct GlobalScope {
    functions: Vec<FuncItem>,
}

impl<'a> Parser<'a> for GlobalScope {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = GlobalScope::default();
        loop {
            match stream.parse::<GlobalScopeItem>() {
                Ok(gci) => match gci {
                    GlobalScopeItem::Func(func) => out.functions.push(func),
                },
                Err(err) => {
                    match err {
                        ParseError::UnexpectedToken(_) => break, //return Err(err),
                        ParseError::EndOfTokenStream => break,
                    }
                },
            }
        }
        Ok(out)
    }
}

#[derive(Debug)]
enum GlobalScopeItem {
    Func(FuncItem),
}

impl GlobalScopeItem {
    fn parse_visibility(stream: &mut ParseStream<'_>) -> ParseResult<Visibility> {
        match stream.parse::<Token![Pub]>() {
            Ok(_) => return Ok(Visibility::Public),
            Err(err) => match err {
                ParseError::UnexpectedToken(_) => Ok(Visibility::Private),
                ParseError::EndOfTokenStream => Err(err),
            },
        }
    }
}

impl<'a> Parser<'a> for GlobalScopeItem {
    type Output = Self;
    fn parse(mut stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        //  Check for the `pub` keyword
        let vis = Self::parse_visibility(&mut stream)?;
        match stream.parse::<FuncItem>() {
            Ok(mut func) => {
                func.vis = vis;
                Ok(GlobalScopeItem::Func(func))
            },
            Err(e) => Err(e),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Default for Visibility {
    fn default() -> Self { Self::Private }
}

#[derive(Default, Debug)]
pub struct FuncItem {
    pub vis: Visibility,
    pub sig: FnSig,
}

impl<'a> Parser<'a> for FuncItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FuncItem::default();
        stream.parse::<Token![Fn]>()?;
        out.sig = stream.parse::<FnSig>()?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct FnSig {
    ident: Token![Ident],
    args: Vec<FnArg>,
}

impl<'a> Parser<'a> for FnSig {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FnSig::default();
        out.ident = stream.parse::<Token![Ident]>()?;
        type FuncArgs = Enclosed<Token!["("], Option<Punctuated<FnArg, Token![","]>>, Token![")"]>;
        out.args = stream.parse::<FuncArgs>()?.unwrap_or_default();
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct FnArg {
    ident: Token![Ident],
    ty: Token![Ident],
}

impl<'a> Parser<'a> for FnArg {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FnArg::default();
        out.ident = stream.parse::<Token![Ident]>()?;
        stream.parse::<Token![":"]>()?;
        out.ty = stream.parse::<Token![Ident]>()?;
        Ok(out)
    }
}

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
    T: Parser<'a>
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
