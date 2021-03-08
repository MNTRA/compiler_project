use lexer::{PunctuationType, SyntaxTokenType};

use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    parsers::{
        combinators::Enclosed,
        common::{
            MaybeTypedIdent,
            Mutability,
            Type,
        },
    },
    unexpected_token,
    Parser,
    Token,
};

#[derive(Debug)]
pub struct LetStmt {
    mutability: Mutability,
    ident: Token![Ident],
    ty: Type,
    expr: Option<Expr>,
}

impl<'a> Parser<'a> for LetStmt {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token![Let]>()?;
        let mti = stream.parse::<MaybeTypedIdent>()?;

        let mutability = mti.mutability;
        let ident = mti.ident;
        let ty = mti.ty;
        let expr = stream.parse::<Option<LetStmtAssign>>()?;

        stream.parse::<Token![";"]>()?;

        Ok(Self {
            mutability,
            ident,
            ty,
            expr,
        })
    }
}

#[derive(Default, Debug)]
pub struct LetStmtAssign;
impl<'a> Parser<'a> for LetStmtAssign {
    type Output = Expr;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token!["="]>()?;
        stream.parse::<Expr>()
    }
}

pub enum Expr {
    Ident(Token![Ident]),
    Literal(Token![Literal]),
    Tuple,
    BinOp(Box<BinOp>),
    Scope,
}

impl std::fmt::Debug for Expr {
    #[rustfmt::skip]
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Literal(lit) => std::fmt::Debug::fmt(lit, fmt),
            Expr::BinOp(binop) => std::fmt::Debug::fmt(binop, fmt),
            Expr::Ident(ident) => std::fmt::Debug::fmt(ident, fmt),
            Expr::Tuple        =>  { fmt.write_str("(,)") },
            Expr::Scope        => { fmt.write_str("{...}") }
            
        }    
    }
}

impl From<BinOp> for Expr {
    fn from(binop: BinOp) -> Self { Self::BinOp(Box::new(binop)) }
}

impl Default for Expr {
    fn default() -> Self { Expr::Scope }
}


// 1 * 2 + 3

impl Expr {
    pub fn try_parse_binop<'a>(
        stream: &mut ParseStream<'a>,
        lhs: &mut Self,
    ) -> ParseResult<bool> {
        // is_binop is used by the caller to determine whether this function
        // successfully transformed the lhs Expr into a BinOp expr
        let mut is_binop = false;
        loop {
            if let Some(operator) = stream.parse::<Option<Token![Operator]>>()? {
                is_binop = true;

                // Operator precidence requires factor nodes (*, /) are children of
                // term nodes, this ensures the factors are evaluted before the
                // term nodes are.
                let binop = if operator.is_factor() {
                    // Factor binops should only evalute with their immediate neighbor
                    let rhs = stream.parse::<Operand>()?;
                    BinOp {
                        ty: operator,
                        lhs: std::mem::take(lhs),
                        rhs: Expr::from(rhs),
                    }
                } else {
                    // Term binops should evaluate with the rest of the expression
                    let rhs = stream.parse::<Expr>()?;
                    BinOp {
                        ty: operator,
                        lhs: std::mem::take(lhs),
                        rhs,
                    }
                };

                // modify the lhs to be the binop expr, subsequent loops will use the
                // modified lhs to build up the Binop expr tree
                *lhs = Expr::from(binop);
            } else {
                return Ok(is_binop);
            }
        }
    }
}

impl<'a> Parser<'a> for Expr {
    type Output = Self;
    fn parse(mut stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        if let Some(mut expr) = stream.parse::<Option<Operand>>()? {
            if Self::try_parse_binop(&mut stream, &mut expr)? {
                return Ok(expr)
            }
            return Ok(expr)
        } else {
            return Ok(Self::Scope);
        }
    }
}

impl From<Operand> for Expr {
    fn from(operand: Operand) -> Self {
        match operand {
            Operand::Literal(literal) => Self::Literal(literal),
            Operand::Ident(ident) => Self::Ident(ident),
        }
    }
}

pub struct TupleExpr {

}

impl<'a> Parser<'a> for TupleExpr {
    type Output = Expr;
    fn parse(s: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        unexpected_token!(s);
    }
}


#[derive(Debug)]
pub struct BinOp {
    pub ty: Token![Operator],
    pub lhs: Expr,
    pub rhs: Expr,
}

pub enum Operand {
    Ident(Token![Ident]),
    Literal(Token![Literal]),
}

pub enum OperandType {
    Parend,
    Braced,
    Standalone,
}

impl<'a> Parser<'a> for Operand {
    type Output = Expr;
    fn parse(s: &mut ParseStream<'a>) -> ParseResult<Self::Output> {

        type Braced = Enclosed<Token!["{"], Expr, Token!["}"]>;
        if let Some(expr) = s.parse::<Option<Braced>>()? {
            return Ok(expr);
        }
        
        type Parened = Enclosed<Token!["("], Expr, Token![")"]>;
        if let Some(expr) = s.parse::<Option<Parened>>()? {
            return Ok(expr);
        }

        if let Some(literal) = s.parse::<Option<Token![Literal]>>()? {
            return Ok(Expr::from(Self::Literal(literal)));
        }

        if let Some(ident) = s.parse::<Option<Token![Ident]>>()? {
            return Ok(Expr::from(Self::Ident(ident)));
        }

        unexpected_token!(s);
    }
}

