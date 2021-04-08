use diagnostics::Reporter;

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
            ScopeExpr
        },
        function::FuncItem,
    },
    unexpected_token,
    Parse,
    Token,
};


/// The outermost scope of the translation unit.
#[derive(Debug, Default)]
pub struct GlobalScope {
    functions: Vec<FuncItem>,
    statics: Vec<StaticItem>,
    temp: Expr,
}

impl<'a> Parse<'a> for GlobalScope {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = GlobalScope::default();
        loop {
            match stream.parse::<GlobalScopeItem>(reporter) {
                Ok(gci) => match gci {
                    GlobalScopeItem::Func(func) => out.functions.push(func),
                    GlobalScopeItem::Static(static_item) => out.statics.push(static_item),
                },
                Err(err) => {
                    println!("[ERROR] {}", err);
                    match err {
                        ParseError::UnexpectedToken(_) => break,
                        ParseError::EndOfTokenStream => break,
                    }
                },
            }
        }

        out.temp = stream.parse::<ScopeExpr>(reporter)?;
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
    fn parse_visibility(stream: &mut ParseStream<'_>, reporter: &mut Reporter) -> ParseResult<Visibility> {
        match stream.parse::<Token![Pub]>(reporter) {
            Ok(_) => return Ok(Visibility::Public),
            Err(err) => match err {
                ParseError::UnexpectedToken(_) => Ok(Visibility::Private),
                ParseError::EndOfTokenStream => Err(err),
            },
        }
    }
}

impl<'a> Parse<'a> for GlobalScopeItem {
    type Output = Self;
    fn parse(mut stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        //  Check for the `pub` keyword
        let vis = Self::parse_visibility(&mut stream, reporter)?;
        let mut parse_main = || -> ParseResult<Self::Output> {
            if let Some(mut func_item) = stream.parse::<Option<FuncItem>>(reporter)? {
                func_item.vis = vis;
                return Ok(Self::Func(func_item));
            };
            if let Some(mut static_item) = stream.parse::<Option<StaticItem>>(reporter)? {
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

impl<'a> Parse<'a> for Type {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        if let Ok(type_sig) = stream.parse::<TypeSig>(reporter) {
            Ok(Type::Standalone(type_sig))
        } else {
            let tuple_types = stream.parse::<Tuple>(reporter)?.unwrap_or_default();
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

impl<'a> Parse<'a> for TypeSig {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = TypeSig::default();
        out.reference = stream.parse::<Option<Reference>>(reporter)?;
        out.ident = stream.parse::<Token![Ident]>(reporter)?;
        Ok(out)
    }
}

/// The `&` or `&mut` tokens.
#[derive(Default, Debug)]
pub struct Reference;
impl<'a> Parse<'a> for Reference {
    type Output = Mutability;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        stream.parse::<Token!["&"]>(reporter)?;
        if stream.parse::<Option<Token![Mut]>>(reporter)?.is_some() {
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
impl<'a> Parse<'a> for IdentTypeDecl {
    type Output = Type;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        stream.parse::<Token![":"]>(reporter)?;
        Ok(stream.parse::<Type>(reporter)?)
    }
}

#[derive(Default, Debug)]
pub struct TypedIdent {
    mutability: Mutability,
    ident: Token![Ident],
    ty: Type,
}

impl<'a> Parse<'a> for TypedIdent {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = TypedIdent::default();
        if stream.parse::<Option<Token![Mut]>>(reporter)?.is_some() {
            out.mutability = Mutability::Mutable;
        }
        out.ident = stream.parse::<Token![Ident]>(reporter)?;
        out.ty = stream.parse::<IdentTypeDecl>(reporter)?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct MaybeTypedIdent {
    pub mutability: Mutability,
    pub ident: Token![Ident],
    pub ty: Type,
}

impl<'a> Parse<'a> for MaybeTypedIdent {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        if stream.parse::<Option<Token![Mut]>>(reporter)?.is_some() {
            out.mutability = Mutability::Mutable;
        }
        out.ident = stream.parse::<Token![Ident]>(reporter)?;
        if let Some(ty) = stream.parse::<Option<IdentTypeDecl>>(reporter)? {
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

impl<'a> Parse<'a> for StaticItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        stream.parse::<Token![Static]>(reporter)?;
        out.sig = stream.parse::<TypedIdent>(reporter)?;
        stream.parse::<Token!["="]>(reporter)?;
        out.value = stream.parse::<Expr>(reporter)?;
        stream.parse::<Token![";"]>(reporter)?;
        Ok(out)
    }
}

#[derive(Default, Debug)]
pub struct LetItem {
    sig: MaybeTypedIdent,
    value: Expr,
}

impl<'a> Parse<'a> for LetItem {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        let mut out = Self::default();
        stream.parse::<Token![Let]>(reporter)?;
        out.sig = stream.parse::<MaybeTypedIdent>(reporter)?;
        stream.parse::<Token!["="]>(reporter)?;
        out.value = stream.parse::<Expr>(reporter)?;
        stream.parse::<Token![";"]>(reporter)?;
        Ok(out)
    }
}
