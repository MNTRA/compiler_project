use bitflags::*;
use diagnostics::{Reporter, throw_fatal_error};
use lexer::{
    LiteralType,
    PunctuationType,
    Span,
    SyntaxTokenType,
};
use thiserror::Error;

use crate::{Parse, ParseError, ParseResult, ParseStream, errors::InvalidSyntax};

bitflags! {
    struct ExprFlags: u32 {
        const UNOP      = (1 << 0);
        const LITERAL   = (1 << 1);
        const BINOP     = (1 << 2);
        const STMT      = (1 << 3);
        const FNCALL    = (1 << 4);
    }
}

type ST = SyntaxTokenType;
type LT = LiteralType;
type PT = PunctuationType;

impl Default for ExprFlags {
    fn default() -> Self { ExprFlags::all() }
}

#[derive(Default)]
struct ExpressionState {
    flags: ExprFlags,
    parens: u32,
    brace_count: u32,
    span: Span,
}

#[derive(Debug)]
pub enum Expression {
    Unop(Box<Expression>),
    Literal {
        kind: LiteralType,
        value: String
    },
    Ident {
        value: String
    },
    Scope {
        stmts: Vec<Expression>,
        expr: Box<Expression>,
    },
    Tuple(Vec<()>)
}

impl<'src> Parse<'src> for Expression {
    type Output = Expression;
    fn parse(
        mut stream: &mut ParseStream<'src>,
        mut r: Reporter,
    ) -> ParseResult<Self::Output> {
        let mut reporter = r.reporter(stream.position());
        let mut flags = ExprFlags::default();
        let expr_span = Span::default();
        let mut out = Expression::Tuple(vec![]);
        
        if let Some(operand) = try_parse_operand(&mut stream, &mut reporter)? {

        }

        Ok(out)
    }
}

enum Operand {
    Literal
}

fn try_parse_operand (
    mut stream: &mut ParseStream<'_>,
    reporter: &mut Reporter
) -> ParseResult<Option<Expression>> {

    let mut is_unop = false;
    let mut span = Span::default();
    let mut num_tokens = 0;

    loop {
        if let Some(token) = stream.peek_next(num_tokens){
            match token.ty {
                SyntaxTokenType::Punctuation(punc) => {
                    if let Some(unop) = check_unary_operator(&mut stream, punc)? {
                        if is_unop {
                            println!("{:#?}", token);
                            span.set_end(token.span().end());
                            reporter.post_error(InvalidSyntax::new(
                                span,
                                span,
                                "not a valid unary operator"
                            ));
                            throw_fatal_error();
                        } else {
                            is_unop = true;
                            span = token.span();
                            num_tokens += 1;
                            println!("{:#?}", token);
                        }
                    }
                }
                SyntaxTokenType::Identifier => {}
                SyntaxTokenType::Keyword(_) => {}
                SyntaxTokenType::Literal(_) => {}
                SyntaxTokenType::Whitespace => {}
                SyntaxTokenType::Control(_) => {}
                SyntaxTokenType::Unknown => {}
                SyntaxTokenType::Null => {}
            }
        } else {
            return Err(ParseError::EndOfTokenStream);
        }
    }
}

fn consume_num_tokens(stream: &mut ParseStream<'_>, num: u32) {
    for _ in 0..num {
        stream.consume();
    }
}

enum UnOp {
    Negative,
    Deref
}

fn check_unary_operator(
    stream: &mut ParseStream<'_>,
    punc: PunctuationType
) -> ParseResult<Option<UnOp>> {
    let out = match punc {
        PT::Hyphen => {
            Some(UnOp::Negative)
        },
        PT::Asterisk => {
            Some(UnOp::Deref)
        }
        _ => None
    };
    Ok(out)
}

pub fn try_parse_next_token() {

}
