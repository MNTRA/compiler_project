// Std
use std::{any::Any, cell::Cell, iter::Peekable, pin::Pin, sync::Arc};

//External
use edit_distance::edit_distance;

// Workspace
use ast::{
    Ident,
    Module,
    Mutability,
    Reference,
    Type,
};
use diagnostics::{
    DiagnosticBuilder,
    Level,
};
use errors::FatalError;
use lexer::{
    token::NULL_TOKEN,
    KeywordType,
    PunctuationKind,
    SyntaxToken,
    SyntaxTokenStream,
    TokenKind,
};

// Internal
use crate::ParserInfo;

/// SyntaxTokenType
pub type ST = TokenKind;
/// KeywordType
pub type KT = KeywordType;
/// KeywordType
pub type PT = PunctuationKind;

pub struct Parser<'src> {
    src: &'src String,
    current_token: SyntaxToken,
    token_stream: Peekable<SyntaxTokenStream<'src>>,
    brace_checker: BraceChecker,
}

pub type ParseFn<'a> = fn(&mut Parser<'a>);

impl<'src> Parser<'src> {
    pub async fn parse_module(&mut self) -> Result<(), DiagnosticBuilder> {
        // move to the first token.
        self.next_token(true);

        let item = self.parse_module_item()?;

        println!("{:#?}", item);
        Ok(())
    }

    #[inline]
    pub fn current_token(&self) -> SyntaxToken { self.current_token }

    pub fn allowed_to_skip_whitespace(&self) -> bool { true }

    #[inline]
    pub fn next_token(
        &mut self,
        skip_whitespace: bool,
    ) -> bool {
        while let Some(token) = self.token_stream.next() {
            if matches!(token.ty, ST::Whitespace | ST::Control(..)) && skip_whitespace {
                continue;
            }
            self.current_token = token;
            return true;
        }
        self.current_token = NULL_TOKEN;
        false
    }

    pub fn peek_next(&mut self) -> Option<&SyntaxToken> { self.token_stream.peek() }

    pub fn expect_slice<'a>(
        &mut self,
        tys: &'a [ST],
    ) -> Result<(), &'a ST> {
        for ty in tys {
            if self.current_token().ty == *ty {
                self.next_token(false);
            } else {
                println!("Err: {:#?}", self.current_token());

                return Err(ty);
            }
        }
        self.next_token(true);
        Ok(())
    }

    #[inline]
    pub fn expect_ident(&mut self) -> Result<Ident, ErrorData> {
        let (value, span) = if self.current_token().ty == ST::Identifier {
            // TODO: Store idents as ids in an Ident map
            (
                String::from(self.get_current_token_str()),
                self.current_token().span(),
            )
        } else {
            return Err((ST::Identifier, self.current_token()));
        };
        self.next_token(true);
        Ok(Ident {
            value,
            span,
        })
    }

    #[inline]
    pub fn parse_mutability(&mut self) -> Mutability {
        if self.current_token().ty == ST::Keyword(KT::Mut) {
            let span = self.current_token().span();
            self.next_token(true);
            Mutability::Mutable(span)
        } else {
            Mutability::Immutable
        }
    }

    #[inline]
    pub fn expect_punctuation(
        &mut self,
        punc: PT,
    ) -> Result<SyntaxToken, ErrorData> {
        let token = self.current_token();
        if token.ty == ST::Punctuation(punc) {
            self.next_token(true);
            Ok(token)
        } else {
            Err((ST::Punctuation(punc), token))
        }
    }

    #[inline]
    pub fn expect_keyword(
        &mut self,
        kw: KT,
    ) -> Result<SyntaxToken, ErrorData> {
        let token = self.current_token();
        if token.ty == ST::Keyword(kw) {
            self.next_token(true);
            Ok(token)
        } else {
            Err((ST::Keyword(kw), token))
        }
    }

    #[inline]
    pub fn expect_type(&mut self) -> Result<Type, DiagnosticBuilder> {
        let mut span = self.current_token().span();
        let reference = self.parse_reference();
        let ident = self.expect_ident()
            .map_err(handle_unexpected_token)?;
        span.set_end(ident.span.end());

        Ok(Type {
            reference,
            ident,
            span,
        })
    }

    #[inline]
    pub fn parse_reference(&mut self) -> Option<Reference> {
        if let Ok(token) = self.expect_punctuation(PT::Ampersand) {
            let ref_span = token.span();
            if let Ok(token) = self.expect_keyword(KT::Mut) {
                let mut_span = token.span();
                Some(Reference::Exclusive {
                    ref_span,
                    mut_span,
                })
            } else {
                Some(Reference::Shared(ref_span))
            }
        } else {
            None
        }
    }

    pub fn get_current_token_str<'a>(&'a self) -> &'a str {
        let span = self.current_token().span();
        &self.src[span.start()..=span.end()]
    }


    pub fn brace_checker(&mut self) -> &mut BraceChecker { &mut self.brace_checker }

    pub fn brace_count(&self) -> i32 { self.brace_checker.braces }
    pub fn paren_count(&self) -> i32 { self.brace_checker.parens }
    pub fn bracket_count(&self) -> i32 { self.brace_checker.bracks }
    pub fn angle_bracket_count(&self) -> i32 { self.brace_checker.abracks }
}

impl<'src> From<ParserInfo<'src>> for Parser<'src> {
    fn from(info: ParserInfo<'src>) -> Self {
        Self {
            src: info.source,
            current_token: NULL_TOKEN,
            token_stream: SyntaxTokenStream::new(info.source).peekable(),
            brace_checker: BraceChecker::default(),
        }
    }
}

/// Use this struct to track the number of open and closeing braces in the file.
/// This is useful for detecting errors and tracking scopes
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct BraceChecker {
    braces: i32,
    bracks: i32,
    abracks: i32,
    parens: i32,
}

impl BraceChecker {
    #[inline]
    pub fn inc_braces(&mut self) { self.braces += 1; }
    #[inline]
    pub fn dec_braces(&mut self) { self.braces -= 1; }
    #[inline]
    pub fn inc_brackets(&mut self) { self.bracks += 1; }
    #[inline]
    pub fn dec_brackets(&mut self) { self.bracks -= 1; }
    #[inline]
    pub fn inc_angle_brackets(&mut self) { self.abracks += 1; }
    #[inline]
    pub fn dec_angle_brackets(&mut self) { self.abracks -= 1; }
    #[inline]
    pub fn inc_parens(&mut self) { self.parens += 1; }
    #[inline]
    pub fn dec_parens(&mut self) { self.parens -= 1; }
}

pub enum EnclosedKind {
    Parentheses,
    Braces,
    Brackets,
    AngledBrackets,
}

pub type ErrorData = (ST, SyntaxToken);

pub fn handle_unexpected_token<'src>((kind, token): ErrorData) -> DiagnosticBuilder {
    DiagnosticBuilder::default()
        .with_level(Level::Error)
        .with_label(format!(
            "Expected '{kind:?}', found {:?}",
            token.ty
        ))
        .with_marked_span(token.span())
}
