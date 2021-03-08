use crate::{
    parse_stream::{
        ParseResult,
        ParseStream,
    },
    parsers::{
        combinators::{
            Enclosed,
            Punctuated,
        },
        common::{
            Scope,
            Type,
            TypedIdent,
            Visibility,
        },
    },
    Parser,
    Token,
};

#[derive(Default, Debug)]
pub struct FuncItem {
    pub vis: Visibility,
    pub sig: FnSignature,
}

impl<'a> Parser<'a> for FuncItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FuncItem::default();
        stream.parse::<Token![Fn]>()?;
        out.sig = stream.parse::<FnSignature>()?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct FnSignature {
    ident: Token![Ident],
    args: Vec<TypedIdent>,
    ret: Type,
    scope: Scope,
}

type FuncArgs = Enclosed<Token!["("], Option<Punctuated<TypedIdent, Token![","]>>, Token![")"]>;

impl<'a> Parser<'a> for FnSignature {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FnSignature::default();
        out.ident = stream.parse::<Token![Ident]>()?;
        out.args = stream.parse::<FuncArgs>()?.unwrap_or_default();
        if stream.parse::<Option<Token!["->"]>>()?.is_some() {
            out.ret = stream.parse::<Type>()?;
        }
        out.scope = stream.parse::<Scope>()?;
        Ok(out)
    }
}
