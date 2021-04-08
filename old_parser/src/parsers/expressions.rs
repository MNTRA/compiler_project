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
    Parse,
    Token,
};


#[derive(Debug)]
pub struct LetStmt {
    mutability: Mutability,
    ident: Token![Ident],
    ty: Type,
    expr: Option<Expr>,
}

impl<'a> Parse<'a> for LetStmt {
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
impl<'a> Parse<'a> for LetStmtAssign {
    type Output = Expr;
    fn parse(stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        stream.parse::<Token!["="]>()?;
        stream.parse::<Expr>()
    }
}

#[derive(Debug)]
pub enum Expr {
    Ident(Token![Ident]),
    Literal(Token![Literal]),
    /// `(expr, ...)`
    Tuple(Vec<Expr>),
    BinOp(Box<BinOp>),
    Statement(Box<Expr>),
    Scope(Option<Box<Expr>>),
}

impl From<BinOp> for Expr {
    fn from(binop: BinOp) -> Self { Self::BinOp(Box::new(binop)) }
}
impl From<Vec<Expr>> for Expr {
    fn from(tuple: Vec<Expr>) -> Self { Self::Tuple(tuple) }
}

impl Default for Expr {
    fn default() -> Self { Expr::Scope(None) }
}

impl Expr {
    pub fn try_parse_binop_expr<'a>(
        stream: &mut ParseStream<'a>,
        lhs: &mut Self,
    ) -> ParseResult<bool> {
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

impl<'a> Parse<'a> for Expr {
    type Output = Self;
    fn parse(mut stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        let mut expr = stream.parse::<Operand>()?;
        println!("Got here Expr");
        if Self::try_parse_binop_expr(&mut stream, &mut expr)? {
            return Ok(expr);
        }
        return Ok(expr);
    }
}

impl From<Operand> for Expr {
    fn from(operand: Operand) -> Self {
        match operand {
            Operand::Literal(literal) => Self::Literal(literal),
            Operand::Ident(ident) => Self::Ident(ident),
            Operand::Scoped(expr) => Self::Scope(Some(Box::new(expr))),
        }
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
    Scoped(Expr)
}

impl Operand {
    pub fn try_parse_parens<'a>(stream: &mut ParseStream<'a>) -> ParseResult<Option<Expr>> {
        if stream.parse::<Option<Token!["("]>>()?.is_some() {
            let mut tuple = Vec::new();
            loop {
                let expr = stream.parse::<Expr>()?;
                if stream.parse::<Option<Token![","]>>()?.is_some() {
                    tuple.push(expr);
                } else if stream.parse::<Option<Token![")"]>>()?.is_some() {
                    if !tuple.is_empty() {
                        tuple.push(expr);
                        return Ok(Some(Expr::Tuple(tuple)));
                    } else {
                        return Ok(Some(expr));
                    }
                }
            }  
        }
        Ok(None)
    }
    pub fn try_parse_braces<'a>(stream: &mut ParseStream<'a>) -> ParseResult<Option<Expr>> {
        todo!()
    }
}

impl<'a> Parse<'a> for Operand {
    type Output = Expr;
    fn parse(mut s: &mut ParseStream<'a>) -> ParseResult<Self::Output> {
        // if let Some(expr) = Self::try_parse_braces(&mut s)? {
        //     println!("Got here Operand");
        //     return Ok(expr);
        // }


        if let Some(expr) = Self::try_parse_parens(&mut s)? {
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

#[derive(Debug, Default)]
pub struct ScopeExpr;
impl<'a> Parse<'a> for ScopeExpr {
    type Output = Expr;
    fn parse(mut stream: &mut ParseStream<'a>) -> ParseResult<Self::Output> {

        stream.parse::<Token!["{"]>()?;
        let expr = stream.parse::<Expr>()?;
        stream.parse::<Token!["}"]>()?;
        println!("Got here");

        Ok(Expr::from(expr))
    }
}