// STD
use std::iter::Peekable;
use std::str::Chars;

// Internal
use crate::{
    parser::{
        ControlType,
        PunctuationType,
    },
    util::{
        self,
        SourceLocation,
    },
};

#[derive(Debug)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub data: TokenData<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    Word,
    Number,
    Punctuation(PunctuationType),
    Control(ControlType),
    Whitespace,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub struct TokenData<'a> {
    tok: &'a str,
    loc: SourceLocation,
}

impl<'a> TokenData<'a> {
    pub fn new(
        tok: &'a str,
        loc: SourceLocation,
    ) -> Self {
        Self {
            tok,
            loc,
        }
    }

    pub fn token_str(&self) -> &str { self.tok }
    pub fn release_source_location(&mut self) -> SourceLocation {
        let mut temp = SourceLocation::new(0, 0, None);
        std::mem::swap(&mut self.loc, &mut temp);
        temp
    }
    pub fn get_source_location(&self) -> &SourceLocation { &self.loc }
    pub fn len(&self) -> usize { self.tok.len() }
}

impl<'a> std::cmp::PartialEq<TokenType> for Token<'a> {
    fn eq(
        &self,
        other: &TokenType,
    ) -> bool {
        self.ty == *other
    }
}

impl<'a> std::cmp::PartialEq<TokenType> for &Token<'a> {
    fn eq(
        &self,
        other: &TokenType,
    ) -> bool {
        self.ty == *other
    }
}

impl<'a> std::cmp::PartialEq<TokenType> for &mut Token<'a> {
    fn eq(
        &self,
        other: &TokenType,
    ) -> bool {
        self.ty == *other
    }
}

pub struct SourceCode<'a> {
    source: &'a str,
}

impl<'a> SourceCode<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
        }
    }
    pub fn tokens(&self) -> TokenIter { TokenIter::from(self) }
    pub fn get_str(&self) -> &str { &self.source }
}

pub struct TokenIter<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
        }
    }
    pub fn get_source(&self) -> &'a str { &self.source }
}

impl<'a, 'b> From<&'a SourceCode<'a>> for TokenIter<'a> {
    fn from(sc: &'a SourceCode) -> Self { Self::new(&sc.source) }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut char_iter = self.source.chars().peekable();
        util::advance_iter_to(&mut char_iter, self.pos);

        let tokenizer_functions = &[
            tokenizer_whitespace, // NOTE(George): make sure tokenizer_whitespace is first
            tokenizer_escaped_chars,
            tokenizer_punctuation,
            tokenizer_literals,
            tokenizer_words,
            tokenizer_unkwown,
        ];

        loop {
            if let Some(first_char) = char_iter.next() {
                for tokenizer in tokenizer_functions {
                    if let Some((token, len)) =
                        tokenizer(first_char, &mut char_iter, self.pos, self.source)
                    {
                        self.pos += len;
                        return Some(token);
                    }
                }
            }
            break;
        }
        None
    }
}

macro_rules! create_punctuation_token {
    ($TY:ident, $source:ident, $offset:ident) => {
        Some((
            Token {
                ty: TokenType::Punctuation(PunctuationType::$TY),
                data: TokenData::new(
                    &$source[$offset..=$offset],
                    SourceLocation::new(0, $offset, None),
                ),
            },
            1,
        ));
    };
}

#[rustfmt::skip]
fn tokenizer_punctuation<'a>(
    first_char: char,
    _: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    match first_char {
        '+' => create_punctuation_token!(Plus,          source, offset),
        '-' => create_punctuation_token!(Hyphen,        source, offset),
        '_' => create_punctuation_token!(UnderScore,    source, offset),
        '/' => create_punctuation_token!(Slash,         source, offset),
        '\\'=> create_punctuation_token!(BackSlash,     source, offset),
        '*' => create_punctuation_token!(Asterisk,      source, offset),
        ')' => create_punctuation_token!(RParen,        source, offset),
        '(' => create_punctuation_token!(LParen,        source, offset),
        '>' => create_punctuation_token!(RAngleBracket, source, offset),
        '<' => create_punctuation_token!(LAngleBracket, source, offset),
        '}' => create_punctuation_token!(RBrace,        source, offset),
        '{' => create_punctuation_token!(LBrace,        source, offset),
        ']' => create_punctuation_token!(RBracket,      source, offset),
        '[' => create_punctuation_token!(LBracket,      source, offset),
        '=' => create_punctuation_token!(Equals,        source, offset),
        '|' => create_punctuation_token!(Pipe,          source, offset),
        '?' => create_punctuation_token!(QuestionMark,  source, offset),
        '!' => create_punctuation_token!(Exclamation,   source, offset),
        '.' => create_punctuation_token!(Period,        source, offset),
        '&' => create_punctuation_token!(Ampersand,     source, offset),
        ':' => create_punctuation_token!(Colon,         source, offset),
        ';' => create_punctuation_token!(SemiColon,     source, offset),
        '"' => create_punctuation_token!(Quote,         source, offset),
        '\''=> create_punctuation_token!(SingleQuote,   source, offset),
        '%' => create_punctuation_token!(Percent,       source, offset),
        '#' => create_punctuation_token!(Hash,          source, offset),
        '@' => create_punctuation_token!(At,            source, offset),
        '$' => create_punctuation_token!(Dollar,        source, offset),
        '~' => create_punctuation_token!(Tilde,         source, offset),
        '`' => create_punctuation_token!(BackQuote,     source, offset),
        _ => None,
    }
}

fn tokenizer_literals<'a>(
    first_char: char,
    next_chars: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    match first_char {
        '0'..='9' => {
            let mut length: usize = 1;
            while let Some(c) = next_chars.peek() {
                match c {
                    '0'..='9' => {
                        length += 1;
                        next_chars.next();
                    },
                    _ => {
                        return Some((
                            Token {
                                ty: TokenType::Number,
                                data: TokenData::new(
                                    &source[offset..offset + length],
                                    SourceLocation::new(0, offset, None),
                                ),
                            },
                            length,
                        ));
                    },
                }
            }
        },
        _ => {},
    }
    None
}

fn tokenizer_whitespace<'a>(
    first_char: char,
    next_chars: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    match first_char {
        ' ' => {
            let mut length = 1;
            while let Some(c) = next_chars.peek() {
                match c {
                    ' ' => {
                        length += 1;
                        next_chars.next();
                    },
                    _ => {
                        return Some((
                            Token {
                                ty: TokenType::Whitespace,
                                data: TokenData::new(
                                    &source[offset..offset + length],
                                    SourceLocation::new(0, offset, None),
                                ),
                            },
                            length,
                        ));
                    },
                }
            }
        },
        _ => {},
    }
    None
}

fn tokenizer_words<'a>(
    first_char: char,
    next_chars: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    if first_char.is_alphabetic() {
        let mut length: usize = 1;

        while let Some(c) = next_chars.peek() {
            if c.is_alphanumeric() {
                length += 1;
                next_chars.next();
            } else {
                let token = Token {
                    ty: TokenType::Word,
                    data: TokenData::new(
                        &source[offset..offset + length],
                        SourceLocation::new(0, offset, None),
                    ),
                };
                return Some((token, length));
            }
        }
    }
    None
}

fn tokenizer_escaped_chars<'a>(
    first_char: char,
    next_chars: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    match first_char {
        '\r' => {
            if let Some(c) = next_chars.peek() {
                match c {
                    '\n' => {
                        next_chars.next();
                        Some((
                            Token {
                                ty: TokenType::Control(ControlType::NewLine),
                                data: TokenData::new(
                                    &source[offset..=offset + 1],
                                    SourceLocation::new(0, offset, None),
                                ),
                            },
                            2,
                        ))
                    },
                    _ => Some((
                        Token {
                            ty: TokenType::Control(ControlType::NewLine),
                            data: TokenData::new(
                                &source[offset..=offset],
                                SourceLocation::new(0, offset, None),
                            ),
                        },
                        1,
                    )),
                }
            } else {
                Some((
                    Token {
                        ty: TokenType::Control(ControlType::NewLine),
                        data: TokenData::new(
                            &source[offset..=offset],
                            SourceLocation::new(0, offset, None),
                        ),
                    },
                    1,
                ))
            }
        },
        '\n' => Some((
            Token {
                ty: TokenType::Control(ControlType::NewLine),
                data: TokenData::new(
                    &source[offset..=offset],
                    SourceLocation::new(0, offset, None),
                ),
            },
            1,
        )),
        '\t' => Some((
            Token {
                ty: TokenType::Control(ControlType::Tab),
                data: TokenData::new(
                    &source[offset..=offset],
                    SourceLocation::new(0, offset, None),
                ),
            },
            1,
        )),
        '\0' => Some((
            Token {
                ty: TokenType::Control(ControlType::Null),
                data: TokenData::new(
                    &source[offset..=offset],
                    SourceLocation::new(0, offset, None),
                ),
            },
            1,
        )),
        _ => None,
    }
}

fn tokenizer_unkwown<'a>(
    _: char,
    _: &mut Peekable<Chars>,
    offset: usize,
    source: &'a str,
) -> Option<(Token<'a>, usize)> {
    Some((
        Token {
            ty: TokenType::Unknown,
            data: TokenData::new(
                &source[offset..=offset],
                SourceLocation::new(0, offset, None),
            ),
        },
        1,
    ))
}
