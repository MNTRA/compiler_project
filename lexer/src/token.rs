// use macros::{
//     define_keyword_token,
// };

use crate::{
    span::Span,
};

// Syntax Tokens ===============================================================

#[derive(Debug, Clone, Copy)]
pub struct SyntaxToken<'a> {
    pub ty: SyntaxTokenType,
    pub data: SyntaxTokenData<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxTokenData<'a> {
    pub(crate) start_line: usize,
    pub(crate) src: &'a str,
    pub(crate) span: Span,
}

impl<'a> SyntaxTokenData<'a> {
    pub fn token_str(&self) -> &str { self.src }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SyntaxTokenType {
    Punctuation(PunctuationType),
    Keyword(KeywordType),
    Identifier,
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
    Import,
    If,
    Else,
    Fn,
    This,
    Match,
    Pub,
    Class,
    Interface,
    Return,
    Module,
    Const,
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



// type ParseResult<T> = Result<T, ()>;

// pub trait Token {
//     const NAME: &'static str;
// }

// define_keyword_token! {
//     "import",
//     "let",
// }

// #[macro_export]
// macro_rules! Token {
//     [import]  => { ::lexer::token::Import };
//     [let]     => { ::lexer::token::Let    };
// }