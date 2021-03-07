use super::combinators::Enclosed;
use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    Parser,
    Token,
};

// use std::marker::PhantomData;

// use lexer::{
//     LiteralType,
//     SyntaxTokenType,
// };

// use super::common::Scope;
// use crate::{
//     parse_stream::{
//         ParseError,
//         ParseResult,
//         ParseStream,
//     },
//     parsers::{
//         combinators::Seperated,
//         common::{
//             MaybeTypedIdent,
//             Mutability,
//             Type,
//         },
//     },
//     Parser,
//     Token,
// };

// #[derive(Debug)]
// pub struct LetStmt {
//     mutability: Mutability,
//     ident: Token![Ident],
//     ty: Type,
//     expr: Option<Expression>,
// }

// impl<'a> Parser<'a> for LetStmt {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         stream.parse::<Token![Let]>()?;
//         let mti = stream.parse::<MaybeTypedIdent>()?;

//         let mutability = mti.mutability;
//         let ident   = mti.ident;
//         let ty      = mti.ty;
//         let expr    = stream.parse::<Option<LetStmtAssign>>()?;

//         stream.parse::<Token![";"]>()?;

//         Ok(Self{
//             mutability,
//             ident,
//             ty,
//             expr,
//         })
//     }
// }

// /*
//     let x;
//     let x = 10;

//     let mut c: i32;

//     {lhs} [op] {rhs}

//     let x = 2;
//     1     + 2

// */
// #[derive(Default, Debug)]
// pub struct LetStmtAssign;
// impl<'a> Parser<'a> for LetStmtAssign {
//     type Output = Expression;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         stream.parse::<Token!["="]>()?;
//         stream.parse::<Expression>()
//     }
// }

// #[derive(Debug)]
// pub enum Expression {
//     Scope(Scope),
//     //LetStmt(Box<LetStmt>),
//     // Literal(LiteralExpr),
//     BinOp(Box<BinOpExpr>),
// }

// impl<'a> Parser<'a> for Expression {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         // if let Some(scope) = stream.parse::<Option<Scope>>()? {
//         //     return Ok(Self::Scope(scope));
//         // }
//         if let Some(bin_op) = stream.parse::<Option<BinOpExpr>>()? {
//             return Ok(Self::BinOp(Box::new(bin_op)));
//         }
//         return Ok(Self::Literal(
//             stream.parse::<Token![Literal]>()?
//         ));
//     }
// }

// #[derive(Debug)]
// pub enum BinOpExpr {
//     Add(AddOp)
// }

// impl<'a> Parser<'a> for BinOpExpr {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         // if let Some(lit) = stream.parse::<Option<LiteralExpr>>()? {
//         //     return Ok(Self::Literal(lit));
//         // }
//         if let Some(assign) = stream.parse::<Option<AddOp>>()? {
//             return Ok(BinOpExpr::Add(assign))
//         }

//         let token = stream.get_next_token()?;
//         Err(ParseError::UnexpectedToken(token.ty))
//     }
// }

// #[derive(Debug)]
// pub struct AddOp {
//     rhs: LiteralExpr,
//     lhs: LiteralExpr,
// }
// impl<'a> Parser<'a> for AddOp {
//     type Output = Self;
//     fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
//         let lhs = stream.parse::<LiteralExpr>()?;
//         stream.parse::<Token!["+"]>()?;
//         let rhs = stream.parse::<LiteralExpr>()?;
//         Ok(Self { lhs, rhs })
//     }
// }

#[derive(Debug)]
pub enum Expr {
    Literal(Token![Literal]),
    BinOp(Box<BinOp>),
    Scope,
}

impl Default for Expr {
    fn default() -> Self { Expr::Scope }
}

impl<'a> Parser<'a> for Expr {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        if let Some(operand) = stream.parse::<Option<Operand>>()? {
            stream.store_data(operand);
            loop {
                let out = stream.parse::<Option<BinOp>>()?;
                match out {
                    Some(_) => continue,
                    None => break,
                }
            }
            // We must! ensure the data stored is a valid Expr
            let data: Expr = *stream.get_data();
            Ok(data)
        } else {
            return Ok(Self::Scope);
        }
    }
}

impl From<Operand> for Expr {
    fn from(operand: Operand) -> Self {
        match operand {
            Operand::Literal(literal) => Self::Literal(literal),
        }
    }
}
impl From<BinOp> for Expr {
    fn from(binop: BinOp) -> Self { Self::BinOp(Box::new(binop)) }
}

#[derive(Debug)]
pub struct BinOp {
    pub ty: Token![Operator],
    pub lhs: Expr,
    pub rhs: Expr,
}

impl<'a> Parser<'a> for BinOp {
    type Output = ();
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        // When we get here the caller (Expr::parse) will have already stored
        // a valid Expr in the stream data or it would have already returned
        // with an error.
        //
        // If the operator is a factor (*, /) we parse the next operand and store
        // it as an Expr in the stream data.
        // If it isn't a factor we create the binop using the stored data as the
        // lhs and parse the rhs as an Expr.

        let operator = stream.parse::<Token![Operator]>()?;
        let binop = if operator.is_factor() {
            let rhs = stream.parse::<Operand>()?;
            let lhs: Expr = *stream.get_data();
            Self {
                ty: operator,
                lhs,
                rhs: Expr::from(rhs),
            }
        } else {
            let lhs: Expr = *stream.get_data();
            let rhs = stream.parse::<Expr>()?;
            Self {
                ty: operator,
                lhs,
                rhs,
            }
        };

        // We must ensure the data is Expr before returning to the Expr parse fn
        stream.store_data(Expr::from(binop));
        Ok(())
    }
}

pub enum Operand {
    Literal(Token![Literal]),
}

impl<'a> Parser<'a> for Operand {
    type Output = Expr;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        type Parened = Enclosed<Token!["("], Expr, Token![")"]>;
        if let Some(expr) = stream.parse::<Option<Parened>>()? {
            return Ok(expr);
        }

        if let Some(literal) = stream.parse::<Option<Token![Literal]>>()? {
            return Ok(Expr::from(Self::Literal(literal)));
        }

        let token = stream.peek(0)?;
        Err(ParseError::UnexpectedToken(token.ty))
    }
}
