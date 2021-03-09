use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    parsers::{
        combinators::{
            Enclosed,
            Punctuated,
        },
        expressions::{
            Expr,
            ExprItem,
        },
        function::FuncItem,
    },
    unexpected_token,
    Parser,
    Token,
};

/// The outermost scope of the translation unit.
#[derive(Debug, Default)]
pub struct GlobalScope {
    functions: Vec<FuncItem>,
    statics: Vec<StaticItem>,
}

impl<'a> Parser<'a> for GlobalScope {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = GlobalScope::default();
        loop {
            match stream.parse::<GlobalScopeItem>() {
                Ok(gci) => match gci {
                    GlobalScopeItem::Func(func) => out.functions.push(func),
                    GlobalScopeItem::Static(static_item) => out.statics.push(static_item),
                },
                Err(err) => match err {
                    ParseError::UnexpectedToken(_) => break,
                    ParseError::EndOfTokenStream => break,
                },
            }
        }
        Ok(out)
    }
}

/// Any item that can legally live in the global scope
#[derive(Debug)]
enum GlobalScopeItem {
    Func(FuncItem),
    Static(StaticItem),
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
        let mut parse_main = || -> ParseResult<Self::Output> {
            if let Some(mut func_item) = stream.parse::<Option<FuncItem>>()? {
                func_item.vis = vis;
                return Ok(Self::Func(func_item));
            };
            if let Some(mut static_item) = stream.parse::<Option<StaticItem>>()? {
                static_item.vis = vis;
                return Ok(Self::Static(static_item));
            };
            unexpected_token!(stream);
        };

        match parse_main() {
            Ok(out) => Ok(out),
            Err(e) => {
                println!("{:#?}", e);
                Err(e)
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct ExprScope {
    items: Vec<ScopeItem>,
    expr: Expr,
}

impl ExprScope {
    pub fn try_parse_parens<'a>(stream: &mut ParseStream<'a>) -> ParseResult<Option<Expr>> {
        todo!()
    }
}

impl<'a> Parser<'a> for ExprScope {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = ExprScope::default();
        stream.parse::<Option<Token!["{"]>>()?;
        loop {
            if let Some(item) = stream.parse::<Option<ScopeItem>>()? {
                out.items.push(item)
            } else {
                break;
            }
        }
        out.expr = stream.parse::<Expr>()?;
        stream.parse::<Option<Token!["}"]>>()?;
        Ok(out)
    }
}

#[derive(Debug)]
enum ScopeItem {
    Func(FuncItem),
    Static(StaticItem),
    Let(LetItem),
    Expr(Expr),
}

impl<'a> Parser<'a> for ScopeItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        if let Some(item) = stream.parse::<Option<FuncItem>>()? {
            return Ok(Self::Func(item));
        };
        if let Some(item) = stream.parse::<Option<StaticItem>>()? {
            return Ok(Self::Static(item));
        };
        if let Some(item) = stream.parse::<Option<LetItem>>()? {
            return Ok(Self::Let(item));
        };
        if let Some(item) = stream.parse::<Option<ExprItem>>()? {
            return Ok(Self::Expr(item));
        };
        unexpected_token!(stream);
    }
}

/// The presence of the `pub` keyword
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Default for Visibility {
    fn default() -> Self { Self::Private }
}

#[derive(Debug)]
pub enum Type {
    Tuple(Vec<TypeSig>),
    Standalone(TypeSig),
    Unknown,
}

/// `(TypeSig, ... )`
pub type Tuple = Enclosed<Token!["("], Option<Punctuated<TypeSig, Token![","]>>, Token![")"]>;

impl Default for Type {
    fn default() -> Self { Self::Unknown }
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

/// `Ident`
///
/// `&Ident`
///
/// `&mut Ident`
#[derive(Default, Debug)]
pub struct TypeSig {
    reference: Option<Mutability>,
    ident: Token![Ident],
}

impl<'a> Parser<'a> for TypeSig {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = TypeSig::default();
        out.reference = stream.parse::<Option<Reference>>()?;
        out.ident = stream.parse::<Token![Ident]>()?;
        Ok(out)
    }
}

/// The `&` or `&mut` tokens.
#[derive(Default, Debug)]
pub struct Reference;
impl<'a> Parser<'a> for Reference {
    type Output = Mutability;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token!["&"]>()?;
        if stream.parse::<Option<Token![Mut]>>()?.is_some() {
            Ok(Mutability::Mutable)
        } else {
            Ok(Mutability::Immutable)
        }
    }
}

/// The presence of the `mut` keyword.
#[derive(Debug)]
pub enum Mutability {
    Mutable,
    Immutable,
}

impl Default for Mutability {
    fn default() -> Self { Self::Immutable }
}

#[derive(Default, Debug)]
pub struct IdentTypeDecl;
impl<'a> Parser<'a> for IdentTypeDecl {
    type Output = Type;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token![":"]>()?;
        Ok(stream.parse::<Type>()?)
    }
}

#[derive(Default, Debug)]
pub struct TypedIdent {
    mutability: Mutability,
    ident: Token![Ident],
    ty: Type,
}

impl<'a> Parser<'a> for TypedIdent {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = TypedIdent::default();
        if stream.parse::<Option<Token![Mut]>>()?.is_some() {
            out.mutability = Mutability::Mutable;
        }
        out.ident = stream.parse::<Token![Ident]>()?;
        out.ty = stream.parse::<IdentTypeDecl>()?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct MaybeTypedIdent {
    pub mutability: Mutability,
    pub ident: Token![Ident],
    pub ty: Type,
}

impl<'a> Parser<'a> for MaybeTypedIdent {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        if stream.parse::<Option<Token![Mut]>>()?.is_some() {
            out.mutability = Mutability::Mutable;
        }
        out.ident = stream.parse::<Token![Ident]>()?;
        if let Some(ty) = stream.parse::<Option<IdentTypeDecl>>()? {
            out.ty = ty;
        }
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct StaticItem {
    vis: Visibility,
    sig: TypedIdent,
    value: Expr,
}

impl<'a> Parser<'a> for StaticItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        stream.parse::<Token![Static]>()?;
        out.sig = stream.parse::<TypedIdent>()?;
        stream.parse::<Token!["="]>()?;
        out.value = stream.parse::<Expr>()?;
        stream.parse::<Token![";"]>()?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct LetItem {
    sig: MaybeTypedIdent,
    value: Expr,
}

impl<'a> Parser<'a> for LetItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        stream.parse::<Token![Let]>()?;
        out.sig = stream.parse::<MaybeTypedIdent>()?;
        stream.parse::<Token!["="]>()?;
        out.value = stream.parse::<Expr>()?;
        stream.parse::<Token![";"]>()?;
        Ok(out)
    }
}
