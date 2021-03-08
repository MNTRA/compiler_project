// use macros::{
//     define_keyword_token,
// };

use crate::span::Span;

// Syntax Tokens ===============================================================

#[derive(Debug, Clone, Copy)]
pub struct SyntaxToken<'a> {
    pub ty: SyntaxTokenType,
    pub data: SyntaxTokenData<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxTokenData<'a> {
    pub start_line: usize,
    pub src: &'a str,
    pub span: Span,
}

impl<'a> SyntaxTokenData<'a> {
    pub fn token_str(&self) -> &str { self.src }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SyntaxTokenType {
    Punctuation(PunctuationType),
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

impl std::fmt::Display for SyntaxTokenType {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match self {
            Self::Punctuation(p) => std::fmt::Display::fmt(p, fmt),
            _ => fmt.write_str("Needs Display Impl"),
        }
    }
}

// Raw Tokens ==================================================================

#[derive(Debug)]
pub struct RawToken<'a> {
    pub ty: RawTokenType,
    pub data: RawTokenData<'a>,
}

#[derive(Debug, Clone)]
pub struct RawTokenData<'a> {
    pub src: &'a str,
    //pub offset: usize,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RawTokenType {
    Word,
    Number,
    Punctuation(PunctuationType),
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
pub enum PunctuationType {
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

impl std::fmt::Display for PunctuationType {
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
