// use crate::{
//     parse_stream::{
//         ParseError,
//         ParseResult,
//         ParseStream,
//     },
//     parsers::{
//         combinators::{
//             Enclosed,
//             Punctuated,
//         },
//         function::FuncItem,
//     },
//     Parser,
//     Token,
// };

// use super::expressions::Expression;


// /// The outermost scope of the translation unit.
// #[derive(Debug, Default)]
// pub struct GlobalScope {
//     functions: Vec<FuncItem>,
// }

// impl<'a> Parser<'a> for GlobalScope {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let mut out = GlobalScope::default();
//         loop {
//             match stream.parse::<GlobalScopeItem>() {
//                 Ok(gci) => match gci {
//                     GlobalScopeItem::Func(func) => out.functions.push(func),
//                 },
//                 Err(err) => {
//                     match err {
//                         ParseError::UnexpectedToken(_) => break, //return Err(err),
//                         ParseError::EndOfTokenStream => break,
//                     }
//                 },
//             }
//         }
//         Ok(out)
//     }
// }

// /// Any item that can legally live in the global scope
// #[derive(Debug)]
// enum GlobalScopeItem {
//     Func(FuncItem),
// }

// impl GlobalScopeItem {
//     fn parse_visibility(stream: &mut ParseStream<'_>) -> ParseResult<Visibility> {
//         match stream.parse::<Token![Pub]>() {
//             Ok(_) => return Ok(Visibility::Public),
//             Err(err) => match err {
//                 ParseError::UnexpectedToken(_) => Ok(Visibility::Private),
//                 ParseError::EndOfTokenStream => Err(err),
//             },
//         }
//     }
// }

// impl<'a> Parser<'a> for GlobalScopeItem {
//     type Output = Self;
//     fn parse(mut stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         //  Check for the `pub` keyword
//         let vis = Self::parse_visibility(&mut stream)?;
//         match stream.parse::<FuncItem>() {
//             Ok(mut func) => {
//                 func.vis = vis;
//                 Ok(GlobalScopeItem::Func(func))
//             },
//             Err(e) => Err(e),
//         }
//     }
// }

// /// The presence of the `pub` keyword
// #[derive(Debug, Copy, Clone, PartialEq, Eq)]
// pub enum Visibility {
//     Public,
//     Private,
// }

// impl Default for Visibility {
//     fn default() -> Self { Self::Private }
// }

// #[derive(Default, Debug)]
// pub struct Scope {
//     exprs: Vec<Expression>
// }

// impl<'a> Parser<'a> for Scope {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let mut out = Scope::default();
//         stream.parse::<Token!["{"]>()?;
//         while let Some(expr) = stream.parse::<Option<Expression>>()? {
//             out.exprs.push(expr);
//         }
//         stream.parse::<Token!["}"]>()?;
//         //out.exprs = stream.parse::<Enclosed<Token!["{"], Vec<Expression>, Token!["}"]>>()?;
//         Ok(out)
//     }
// }

// #[derive(Debug)]
// pub enum Type {
//     Tuple(Vec<TypeSig>),
//     Standalone(TypeSig),
//     Unknown,
// }

// /// `(TypeSig, ... )`
// type Tuple = Enclosed<Token!["("], Option<Punctuated<TypeSig, Token![","]>>, Token![")"]>;

// impl Default for Type {
//     fn default() -> Self { Self::Unknown }
// }

// impl<'a> Parser<'a> for Type {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         if let Ok(type_sig) = stream.parse::<TypeSig>() {
//             Ok(Type::Standalone(type_sig))
//         } else {
//             let tuple_types = stream.parse::<Tuple>()?.unwrap_or_default();
//             Ok(Type::Tuple(tuple_types))
//         }
//     }
// }

// /// `Ident`
// /// 
// /// `&Ident`
// /// 
// /// `&mut Ident`
// #[derive(Default, Debug)]
// pub struct TypeSig {
//     reference: Option<Mutability>,
//     ident: Token![Ident],
// }

// impl<'a> Parser<'a> for TypeSig {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let mut out = TypeSig::default();
//         out.reference = stream.parse::<Option<Reference>>()?;
//         out.ident = stream.parse::<Token![Ident]>()?;
//         Ok(out)
//     }
// }

// /// The `&` or `&mut` tokens.
// #[derive(Default, Debug)]
// pub struct Reference;
// impl<'a> Parser<'a> for Reference {
//     type Output = Mutability;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         stream.parse::<Token!["&"]>()?;
//         if stream.parse::<Option<Token![Mut]>>()?.is_some() {
//             Ok(Mutability::Mutable)
//         } else {
//             Ok(Mutability::Immutable)
//         }
//     }
// }

// /// The presence of the `mut` keyword.
// #[derive(Debug)]
// pub enum Mutability {
//     Mutable,
//     Immutable,
// }

// impl Default for Mutability {
//     fn default() -> Self { Self::Immutable }
// }


// #[derive(Default, Debug)]
// pub struct IdentTypeDecl;
// impl<'a> Parser<'a> for IdentTypeDecl {
//     type Output = Type;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         stream.parse::<Token![":"]>()?;
//         Ok(stream.parse::<Type>()?)
//     }
// }

// #[derive(Default, Debug)]
// pub struct TypedIdent {
//     mutability: Mutability,
//     ident: Token![Ident],
//     ty: Type,
// }

// impl<'a> Parser<'a> for TypedIdent {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let mut out = TypedIdent::default();
//         if stream.parse::<Option<Token![Mut]>>()?.is_some() {
//             out.mutability = Mutability::Mutable;
//         }
//         out.ident = stream.parse::<Token![Ident]>()?;
//         out.ty = stream.parse::<IdentTypeDecl>()?;
//         Ok(out)
//     }
// }
 
// #[derive(Default, Debug)]
// pub struct MaybeTypedIdent {
//     pub mutability: Mutability,
//     pub ident: Token![Ident],
//     pub ty: Type, 
// }

// impl<'a> Parser<'a> for MaybeTypedIdent {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let mut out = Self::default();
//         if stream.parse::<Option<Token![Mut]>>()?.is_some() {
//             out.mutability = Mutability::Mutable;
//         }
//         out.ident = stream.parse::<Token![Ident]>()?;
//         if let Some(ty) = stream.parse::<Option<IdentTypeDecl>>()? {
//             out.ty = ty;
//         }
//         Ok(out)
//     }
// }
