use std::sync::Arc;

use diagnostics::span::{
    Span,
    NULL_SPAN,
};

// Syntax Tokens ===============================================================

pub const NULL_TOKEN: SyntaxToken = SyntaxToken {
    ty: TokenKind::Null,
    span: NULL_SPAN,
};

#[derive(Debug, Clone, Copy)]
pub struct SyntaxToken {
    pub ty: TokenKind,
    pub(crate) span: Span,
}

impl SyntaxToken {
    pub fn span(&self) -> Span { self.span }
    pub fn get_str<'a>(&self, src: &'a String) -> &'a str {
        &src[self.span.start()..=self.span.end()]
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Punctuation(PunctuationKind),
    Identifier,
    Keyword(KeywordType),
    Literal(LiteralType),
    Whitespace,
    Control(ControlType),
    Unknown,
    Null,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LiteralType {
    String,
    Char,
    Integer,
    Float,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum KeywordType {
    Let,
    Fn,
    Mut,
    Pub,
    Module,
    Static,
}

impl KeywordType {
    pub const fn as_str(&self) -> &'static str {
        match self {
            KeywordType::Let    => "let",
            KeywordType::Fn     => "fn",
            KeywordType::Mut    => "mut",
            KeywordType::Pub    => "pub",
            KeywordType::Module => "mod",
            KeywordType::Static => "static",
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match self {
            Self::Punctuation(p) => std::fmt::Display::fmt(p, fmt),
            Self::Identifier => fmt.write_str("Identifier"),
            _ => fmt.write_str("Needs Display Impl"),
        }
    }
}

// Raw Tokens ==================================================================

#[derive(Debug)]
pub struct RawToken {
    pub ty: RawTokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RawTokenData {
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RawTokenKind {
    Word,
    Number,
    Punctuation(PunctuationKind),
    Control(ControlType),
    Whitespace,
    Unknown,
}

// Shared Token Types ==========================================================

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ControlType {
    NewLine,
    Tab,
    Null,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PunctuationKind {
    /// \+
    Plus,
    /// \-
    Hyphen,
    /// \_
    UnderScore,
    /// \*
    Asterisk,
    /// \/
    Slash,
    /// \\
    BackSlash,
    /// \)
    RParen,
    /// \(
    LParen,
    /// \>
    RAngleBracket,
    /// \<
    LAngleBracket,
    /// \}
    RBrace,
    /// \{
    LBrace,
    /// \]
    RBracket,
    /// \[
    LBracket,
    /// \=
    Equals,
    /// \|
    Pipe,
    /// \?
    QuestionMark,
    /// \!
    Exclamation,
    /// \&
    Ampersand,
    /// \.
    Period,
    /// \:
    Colon,
    /// \;
    SemiColon,
    /// \"
    Quote,
    /// \'
    SingleQuote,
    /// \%
    Percent,
    /// \#
    Hash,
    /// \@
    At,
    /// \$
    Dollar,
    /// \~
    Tilde,
    /// \`
    BackQuote,
    /// \,
    Comma,
}

impl std::fmt::Display for PunctuationKind {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match self {
            Self::Plus => fmt.write_str("+"),
            Self::Hyphen => fmt.write_str("-"),
            _ => fmt.write_str("Needs Display Impl"),
        }
    }
}
