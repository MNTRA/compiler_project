use paste::paste;
use diagnostics::Reporter;
use crate::{
    parse_stream::{
        ParseError,
        ParseResult,
        ParseStream,
    },
    Parse,
};

macro_rules! create_parser {
    ($TOK:ident) => {
        create_parser!(::lexer::SyntaxTokenType::$TOK, $TOK);
    };
    (Punctuation, $TOK:ident) => {
        create_parser!(
            ::lexer::SyntaxTokenType::Punctuation(::lexer::PunctuationType::$TOK),
            $TOK
        );
    };
    (Control, $TOK:ident) => {
        create_parser!(
            ::lexer::SyntaxTokenType::Control(::lexer::ControlType::$TOK),
            $TOK
        );
    };
    (Literal, $TOK:ident) => {
        create_parser!(
            ::lexer::SyntaxTokenType::Literal(::lexer::LiteralType::$TOK),
            $TOK
        );
    };
    (Keyword, $TOK:ident) => {
        create_parser!(
            ::lexer::SyntaxTokenType::Keyword(::lexer::KeywordType::$TOK),
            $TOK
        );
    };
    ($MATCH:path, $TY:ident) => {
        paste! {
            #[derive(Default, Debug)]
            pub struct $TY;
            impl<'a> Parse<'a> for $TY {
                type Output = Self;
                #[inline(always)]
                fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
                    // println!("[ Parser call ]: {}", $TY);
                    let token = stream.peek(0)?;
                    if token.ty == $MATCH {
                        stream.consume();
                        return Ok(crate::tokens::$TY);
                    } else {
                        Err(ParseError::UnexpectedToken(token.ty))
                    }
                }
            }
            impl std::fmt::Display for  $TY {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.write_str(stringify!($TY))
                }
            }
        }
    };
}

// Keywords
create_parser!(Keyword, Let);
create_parser!(Keyword, Fn);
create_parser!(Keyword, Mut);
create_parser!(Keyword, Pub);
create_parser!(Keyword, Module);
create_parser!(Keyword, Static);

// Literals
create_parser!(Literal, String);
create_parser!(Literal, Char);
create_parser!(Literal, Integer);
create_parser!(Literal, Float);

// Punctuation
create_parser!(Punctuation, Plus);
create_parser!(Punctuation, Hyphen);
create_parser!(Punctuation, UnderScore);
create_parser!(Punctuation, Asterisk);
create_parser!(Punctuation, Slash);
create_parser!(Punctuation, BackSlash);
create_parser!(Punctuation, RParen);
create_parser!(Punctuation, LParen);
create_parser!(Punctuation, RAngleBracket);
create_parser!(Punctuation, LAngleBracket);
create_parser!(Punctuation, RBrace);
create_parser!(Punctuation, LBrace);
create_parser!(Punctuation, RBracket);
create_parser!(Punctuation, LBracket);
create_parser!(Punctuation, Equals);
create_parser!(Punctuation, Pipe);
create_parser!(Punctuation, QuestionMark);
create_parser!(Punctuation, Exclamation);
create_parser!(Punctuation, Ampersand);
create_parser!(Punctuation, Period);
create_parser!(Punctuation, Colon);
create_parser!(Punctuation, SemiColon);
create_parser!(Punctuation, Percent);
create_parser!(Punctuation, Hash);
create_parser!(Punctuation, At);
create_parser!(Punctuation, Dollar);
create_parser!(Punctuation, Tilde);
create_parser!(Punctuation, BackQuote);
create_parser!(Punctuation, Comma);

create_parser!(Control, NewLine);
create_parser!(Control, Tab);
create_parser!(Control, Null);

create_parser!(Whitespace);
// gen_parser_fn!(Unknown);

#[derive(Default, Debug)]
pub struct Ident {
    // TODO (George): Don't store string, make it a handle to an Ident Map
    value: std::string::String,
}
impl<'a> Parse<'a> for Ident {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        // println!("[ Parser call ]: Ident");
        let token = stream.peek(0)?;
        if token.ty == ::lexer::SyntaxTokenType::Identifier {
            stream.consume();
            return Ok(Ident {
                value: From::from(token.data.src),
            });
        } else {
            Err(ParseError::UnexpectedToken(token.ty))
        }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        fmt.write_str(&self.value)
    }
}

#[derive(Debug)]
pub struct Literal {
    ty: ::lexer::LiteralType,
    value: std::string::String,
}

impl<'a> Parse<'a> for Literal {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        type ST = ::lexer::SyntaxTokenType;
        type LT = ::lexer::LiteralType;

        // println!("[ Parser call ]: Literal");

        let token = stream.peek(0)?;
        match token.ty {
            ST::Literal(ty @ LT::String) => {
                stream.consume();
                Ok(Self {
                    ty,
                    value: std::string::String::from(token.data.src),
                })
            },
            ST::Literal(ty @ LT::Char) => {
                stream.consume();
                Ok(Self {
                    ty,
                    value: std::string::String::from(token.data.src),
                })
            },
            ST::Literal(ty @ LT::Float) => {
                stream.consume();
                Ok(Self {
                    ty,
                    value: std::string::String::from(token.data.src),
                })
            },
            ST::Literal(ty @ LT::Integer) => {
                stream.consume();
                Ok(Self {
                    ty,
                    value: std::string::String::from(token.data.src),
                })
            },
            _ => Err(ParseError::UnexpectedToken(token.ty)),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator {
    #[rustfmt::skip]
    pub fn is_factor(&self) -> bool { 
        match self {
            Self::Multiply => true,
            Self::Divide   => true,
            Self::Add      => false,
            Self::Subtract => false,
        }
    }
}

impl<'a> Parse<'a> for Operator {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        type ST = ::lexer::SyntaxTokenType;
        type PT = ::lexer::PunctuationType;

        // println!("[ Parser call ]: Operator");

        let token = stream.peek(0)?;
        match token.ty {
            ST::Punctuation(PT::Plus) => {
                stream.consume();
                Ok(Self::Add)
            },
            ST::Punctuation(PT::Hyphen) => {
                stream.consume();
                Ok(Self::Subtract)
            },
            ST::Punctuation(PT::Asterisk) => {
                stream.consume();
                Ok(Self::Multiply)
            },
            ST::Punctuation(PT::Slash) => {
                stream.consume();
                Ok(Self::Divide)
            },
            _ => Err(ParseError::UnexpectedToken(token.ty)),
        }
    }
}

#[macro_export]
macro_rules! Token {
    // Identifiers
    [Ident] => { crate::tokens::Ident};

    // Keywords
    [Let   ] => { crate::tokens::Let    };
    [Fn    ] => { crate::tokens::Fn     };
    [Mut   ] => { crate::tokens::Mut    };
    [Pub   ] => { crate::tokens::Pub    };
    [Module] => { crate::tokens::Module };
    [Static] => { crate::tokens::Static };

    // Literals
    [Literal] => { crate::tokens::Literal };
    [String ] => { crate::tokens::String  };
    [Char   ] => { crate::tokens::Char    };
    [Int    ] => { crate::tokens::Integer };
    [Float  ] => { crate::tokens::Float   };

    [Operator] => [ crate::tokens::Operator ];


    ["+" ] => [ crate::tokens::Plus         ];
    ["-" ] => [ crate::tokens::Hyphen       ];
    ["_" ] => [ crate::tokens::UnderScore   ];
    ["*" ] => [ crate::tokens::Asterisk     ];
    ["/" ] => [ crate::tokens::Slash        ];
    ["\\"] => [ crate::tokens::BackSlash    ];
    [")" ] => [ crate::tokens::RParen       ];
    ["(" ] => [ crate::tokens::LParen       ];
    [">" ] => [ crate::tokens::RAngleBracket];
    ["<" ] => [ crate::tokens::LAngleBracket];
    ["}" ] => [ crate::tokens::RBrace       ];
    ["{" ] => [ crate::tokens::LBrace       ];
    [">" ] => [ crate::tokens::RBracket     ];
    ["<" ] => [ crate::tokens::LBracket     ];
    ["=" ] => [ crate::tokens::Equals       ];
    ["|" ] => [ crate::tokens::Pipe         ];
    ["?" ] => [ crate::tokens::QuestionMark ];
    ["!" ] => [ crate::tokens::Exclamation  ];
    ["&" ] => [ crate::tokens::Ampersand    ];
    ["." ] => [ crate::tokens::Period       ];
    [";" ] => [ crate::tokens::SemiColon    ];
    [":" ] => [ crate::tokens::Colon        ];
    ["%" ] => [ crate::tokens::Percent      ];
    ["#" ] => [ crate::tokens::Hash         ];
    ["@" ] => [ crate::tokens::At           ];
    ["$" ] => [ crate::tokens::Dollar       ];
    ["`" ] => [ crate::tokens::Tilde        ];
    ["`" ] => [ crate::tokens::BackQuote    ];
    ["," ] => [ crate::tokens::Comma        ];
    ["=="] => [ crate::tokens::EqEq         ];
    ["->"] => [ crate::tokens::RArrow       ];
    ["<-"] => [ crate::tokens::LArrow       ];
}

/// `==`
pub struct EqEq;
impl<'a> Parse<'a> for EqEq {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        stream.parse::<Token!["="]>(reporter)?;
        stream.parse_immediate::<Token!["="]>(reporter)?;
        Ok(Self)
    }
}

/// `->`
pub struct RArrow;
impl<'a> Parse<'a> for RArrow {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        stream.parse::<Token!["-"]>(reporter)?;
        stream.parse_immediate::<Token![">"]>(reporter)?;
        Ok(Self)
    }
}

/// `<-`
pub struct LArrow;
impl<'a> Parse<'a> for LArrow {
    type Output = Self;
    fn parse(stream: &mut ParseStream<'a>, reporter: &mut Reporter) -> ParseResult<Self::Output> {
        stream.parse::<Token!["<"]>(reporter)?;
        stream.parse_immediate::<Token!["-"]>(reporter)?;
        Ok(Self)
    }
}
