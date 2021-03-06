use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    parsers::{
        Punctuated,
        Enclosed,
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
    args: Vec<FnArg>,
    ret: Type,
    scope: Vec<ScopeItem>
}

type FuncArgs = Enclosed<Token!["("], Option<Punctuated<FnArg, Token![","]>>, Token![")"]>;

impl<'a> Parser<'a> for FnSignature {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FnSignature::default();
        out.ident = stream.parse::<Token![Ident]>()?;
        out.args = stream.parse::<FuncArgs>()?.unwrap_or_default();
        stream.parse::<Token!["->"]>()?;
        out.ret = stream.parse::<Type>()?;
        // stream.parse::<Token!["{"]>()?;
        // stream.parse::<Token!["}"]>()?;
        out.scope = stream.parse::<Scope>()?.unwrap_or_default();
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct FnArg {
    mutable: bool,
    ident: Token![Ident],
    ty: Type,
}

impl<'a> Parser<'a> for FnArg {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = FnArg::default();
        out.mutable = stream.parse::<Option<Token![Mut]>>()?.is_some();
        out.ident = stream.parse::<Token![Ident]>()?;
        stream.parse::<Token![":"]>()?;
        out.ty = stream.parse::<Type>()?;
        Ok(out)
    }
}

pub type Scope = Enclosed<Token!["{"], Option<Punctuated<ScopeItem, Token![";"]>>, Token!["}"]>;

#[derive(Default, Debug)]
pub struct ScopeItem;
impl<'a> Parser<'a> for ScopeItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        Ok(ScopeItem::default())
    }
}


#[derive(Debug)]
pub enum Type {
    Tuple(Vec<TypeSig>),
    Standalone(TypeSig)
}

type Tuple =  Enclosed<Token!["("], Option<Punctuated<TypeSig, Token![","]>>, Token![")"]>;

impl Default for Type {
    fn default() -> Self { Self::Tuple(Vec::new()) }
}

impl<'a> Parser<'a> for Type {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        if let Ok(type_sig) = stream.parse::<TypeSig>() {
            Ok(Type::Standalone(type_sig))
        } else {
            let tuple_types = stream.parse::<Tuple>()?.unwrap_or_default();
            Ok(Type::Tuple(tuple_types))
        }
    }
}

#[derive(Default, Debug)]
pub struct TypeSig {
    reference: Option<TypeRef>,
    ident: Token![Ident],
}

impl<'a> Parser<'a> for TypeSig {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = TypeSig::default();
        out.reference = stream.parse::<Option<TypeRef>>()?;
        out.ident = stream.parse::<Token![Ident]>()?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct TypeRef {
    mutable: bool,
}

impl<'a> Parser<'a> for TypeRef {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = TypeRef::default();
        stream.parse::<Token!["&"]>()?;
        out.mutable = stream.parse::<Option<Token![Mut]>>()?.is_some();
        return Ok(out);
    }
}

