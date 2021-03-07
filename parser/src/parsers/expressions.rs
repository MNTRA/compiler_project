use std::marker::PhantomData;

use lexer::{
    LiteralType,
    SyntaxTokenType,
};

use super::common::Scope;
use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    parsers::{
        combinators::Seperated,
        common::{
            MaybeTypedIdent,
            Mutability,
            Type,
        },
    },
    Parser,
    Token,
};

#[derive(Debug)]
pub struct LetStmt {
    mutability: Mutability,
    ident: Token![Ident],
    ty: Type,
    expr: Option<Expression>,
}

impl<'a> Parser<'a> for LetStmt {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token![Let]>()?;
        let mti = stream.parse::<MaybeTypedIdent>()?;

        let mutability = mti.mutability;
        let ident   = mti.ident;
        let ty      = mti.ty;
        let expr    = stream.parse::<Option<LetStmtAssign>>()?;

        stream.parse::<Token![";"]>()?;

        Ok(Self{
            mutability,
            ident,
            ty,
            expr,
        })
    }
}

/*
    let x;
    let x = 10;

    let mut c: i32;

    {lhs} [op] {rhs}

    let x = 2;
    1     + 2

*/

#[derive(Default, Debug)]
pub struct LetStmtAssign;
impl<'a> Parser<'a> for LetStmtAssign {
    type Output = Expression;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token!["="]>()?;
        stream.parse::<Expression>()
    }
}

#[derive(Debug)]
pub enum Expression {
    Scope(Scope),
    //LetStmt(Box<LetStmt>),
    Literal(LiteralExpr),
    BinOp(Box<BinOpExpr>),
}

impl<'a> Parser<'a> for Expression {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        // if let Some(scope) = stream.parse::<Option<Scope>>()? {
        //     return Ok(Self::Scope(scope));
        // }
        if let Some(bin_op) = stream.parse::<Option<BinOpExpr>>()? {
            return Ok(Self::BinOp(Box::new(bin_op)));
        }
        return Ok(Self::Literal(
            stream.parse::<LiteralExpr>()?
        ));
    }
}

#[rustfmt::skip]
#[derive(Debug)]
pub enum LiteralExpr {
    String  (Token![String]),
    Char    (Token![Char  ]),
    Float   (Token![Float ]),
    Integer (Token![Int   ]),
}

impl<'a> Parser<'a> for LiteralExpr {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        type ST = SyntaxTokenType;
        let token = stream.get_next_token()?;
        match token.ty {
            ST::Literal(LiteralType::String) => {
                stream.consume();
                Ok(Self::String(Token![String]))
            },
            ST::Literal(LiteralType::Char) => {
                stream.consume();
                Ok(Self::Char(Token![Char]))
            },
            ST::Literal(LiteralType::Float) => {
                stream.consume();
                Ok(Self::Float(Token![Float]))
            },
            ST::Literal(LiteralType::Integer) => {
                stream.consume();
                
                Ok(Self::Integer(Token![Int]))
            },
            _ => Err(ParseError::UnexpectedToken(token.ty)),
        }
    }
}

#[derive(Debug)]
pub enum BinOpExpr {
    Add(AddOp)
}

impl<'a> Parser<'a> for BinOpExpr {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        // if let Some(lit) = stream.parse::<Option<LiteralExpr>>()? {
        //     return Ok(Self::Literal(lit));
        // }
        if let Some(assign) = stream.parse::<Option<AddOp>>()? {
            return Ok(BinOpExpr::Add(assign))
        }

        let token = stream.get_next_token()?;
        Err(ParseError::UnexpectedToken(token.ty))
    }
}

#[derive(Debug)]
pub struct AddOp {
    rhs: LiteralExpr,
    lhs: LiteralExpr,
}
impl<'a> Parser<'a> for AddOp {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let lhs = stream.parse::<LiteralExpr>()?;
        stream.parse::<Token!["+"]>()?;
        let rhs = stream.parse::<LiteralExpr>()?; 
        Ok(Self { lhs, rhs })
    }
}