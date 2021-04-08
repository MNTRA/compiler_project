use std::sync::Arc;

use diagnostics::span::Span;
use macros::create_syntax_tokenizer;
use paste::paste;

use crate::{
    cursor::Cursor,
    raw_token_stream::RawTokenStream,
    token::{
        ControlType,
        KeywordType,
        LiteralType,
        PunctuationKind,
        RawToken,
        RawTokenKind,
        SyntaxToken,
        TokenKind,
    },
};

// Syntax Tokens ===============================================================

pub struct SyntaxTokenStream<'a> {
    pos: usize,
    line: usize,
    src: &'a String,
    cursor: Cursor<RawTokenStream<'a>>,
}

impl<'a> SyntaxTokenStream<'a> {
    pub fn new(src: &'a String) -> Self {
        Self {
            pos: 0,
            line: 0,
            src,
            cursor: Cursor::new(RawTokenStream::new(src)),
        }
    }
}

impl<'a> Iterator for SyntaxTokenStream<'a> {
    type Item = SyntaxToken;

    #[rustfmt::skip]
    fn next(&mut self) -> Option<SyntaxToken> {
        if let Ok(token) = self.cursor.peek(0) {
            let token = match token.ty {
                RawTokenKind::Word           => WordTokenizer       ::new(self).tokenize(),
                RawTokenKind::Number         => NumberTokenizer     ::new(self).tokenize(),
                RawTokenKind::Punctuation(p) => PunctuationTokenizer::new(self).tokenize(p),
                RawTokenKind::Control    (c) => ControlTokenizer    ::new(self).tokenize(c),
                RawTokenKind::Whitespace     => WhiteSpaceTokenizer ::new(self).tokenize(),
                RawTokenKind::Unknown        => UnknownTokenizer    ::new(self).tokenize(),
            };
            Some(token)
        } else {
            None
        }
    }
}

// Simple macro that provides a basic converting implmentation
#[rustfmt::skip]
macro_rules! simple_pass_through {
    (Punctuation, $SELF:ident, $TY:ident) => {{
            $SELF.consume();
            return $SELF.create_token(
                TokenKind::Punctuation(PunctuationKind::$TY)
            );
    }};
    (Control, $SELF:ident, $TY:ident) => {{
        $SELF.consume();
        return $SELF.create_token(
            TokenKind::Control(ControlType::$TY)
        );
    }};
}

macro_rules! create_keyword_token {
    ($SELF:ident, $TY:ident) => {{
        $SELF.consume();
        loop {
            if $SELF.tokenize_underscore(0).is_ok() {
            } else if $SELF.tokenize_word(0).is_ok() {
                return WordTokenizer::tokenize_identifier(&mut *$SELF);
            } else {
                return $SELF.create_token(TokenKind::Keyword(KeywordType::$TY));
            }
        }

    }};
}

create_syntax_tokenizer! {
    WordTokenizer,
    fn tokenize(&mut self) -> SyntaxToken {
        let span = self.peek(0).span;
        let word = self.str_from_span(span);
        match word {
            "let"       => create_keyword_token!(self, Let       ),
            "fn"        => create_keyword_token!(self, Fn        ),
            "mut"       => create_keyword_token!(self, Mut       ),
            "pub"       => create_keyword_token!(self, Pub       ),
            "module"    => create_keyword_token!(self, Module    ),
            "static"    => create_keyword_token!(self, Static    ),
            _ => {
                self.consume();
                WordTokenizer::tokenize_identifier(&mut *self)
            }
        }
    }
}

impl<'a, 'b> WordTokenizer<'a, 'b> {
    fn tokenize_identifier(tkn: &mut SyntaxTokenizer<'a, 'b>) -> SyntaxToken {
        type ST = TokenKind;
        // Concat underscores and words into a single identifer
        loop {
            while let Ok(_) = tkn.tokenize_underscore(0) {}
            if tkn.tokenize_word(0).is_err() {
                return tkn.create_token(ST::Identifier);
            }
        }
    }
}

create_syntax_tokenizer! {
    NumberTokenizer,
    fn tokenize(&mut self) -> SyntaxToken {
        type ST = TokenKind;
        type LT = LiteralType;
        self.consume();
        let mut is_float = || {
            // [num][.] ?
            self.tokenize_period(0)?;
            // [num][.][num] ?
            self.tokenize_number(0)?;
            Ok(())
        };
        match is_float() {
            Ok(()) => { self.create_token(ST::Literal(LT::Float  ))},
            Err(()) => { self.create_token(ST::Literal(LT::Integer))},
        }
    }
}

create_syntax_tokenizer! {
    ControlTokenizer,
    fn tokenize(&mut self, c: ControlType) -> SyntaxToken {
        type CT = ControlType;
        match c {
            CT::NewLine => { simple_pass_through!(Control, self, NewLine  ) },
            CT::Tab     => { simple_pass_through!(Control, self, Tab      ) },
            CT::Null    => { simple_pass_through!(Control, self, Null     ) },
        }
    }
}

create_syntax_tokenizer! {
    PunctuationTokenizer,
    #[rustfmt::skip]
    fn tokenize(&mut self, p: PunctuationKind) -> SyntaxToken {
        type TT = RawTokenKind;
        type PT = PunctuationKind;
        type ST = TokenKind;
        type LT = LiteralType;
        match p {
            PT::Plus          => simple_pass_through!(Punctuation, self, Plus         ),
            PT::Hyphen        => simple_pass_through!(Punctuation, self, Hyphen       ),
            PT::UnderScore    => {
                loop {
                    //self.consume();
                    if self.tokenize_underscore(0).is_ok() {
                    } else if self.tokenize_word(0).is_ok() {
                        return WordTokenizer::tokenize_identifier(&mut *self);
                    } else {
                        return self.create_token(ST::Punctuation(PT::UnderScore));
                    }
                }
            },
            PT::Asterisk      => simple_pass_through!(Punctuation, self, Asterisk     ),
            PT::Slash         => simple_pass_through!(Punctuation, self, Slash        ),
            PT::BackSlash     => simple_pass_through!(Punctuation, self, BackSlash    ),
            PT::RParen        => simple_pass_through!(Punctuation, self, RParen       ),
            PT::LParen        => simple_pass_through!(Punctuation, self, LParen       ),
            PT::RAngleBracket => simple_pass_through!(Punctuation, self, RAngleBracket),
            PT::LAngleBracket => simple_pass_through!(Punctuation, self, LAngleBracket),
            PT::RBrace        => simple_pass_through!(Punctuation, self, RBrace       ),
            PT::LBrace        => simple_pass_through!(Punctuation, self, LBrace       ),
            PT::RBracket      => simple_pass_through!(Punctuation, self, RBracket     ),
            PT::LBracket      => simple_pass_through!(Punctuation, self, LBracket     ),
            PT::Equals        => simple_pass_through!(Punctuation, self, Equals       ),
            PT::Pipe          => simple_pass_through!(Punctuation, self, Pipe         ),
            PT::QuestionMark  => simple_pass_through!(Punctuation, self, QuestionMark ),
            PT::Exclamation   => simple_pass_through!(Punctuation, self, Exclamation  ),
            PT::Ampersand     => simple_pass_through!(Punctuation, self, Ampersand    ),
            PT::Period        => simple_pass_through!(Punctuation, self, Period       ),
            PT::Colon         => simple_pass_through!(Punctuation, self, Colon        ),
            PT::SemiColon     => simple_pass_through!(Punctuation, self, SemiColon    ),
            PT::Quote         => {
                self.consume();
                loop {
                    if self.tokenize_quote(0).is_err() {
                        // consume any tokens that fall between the Quotes
                        self.consume();
                    } else {
                        return self.create_token(ST::Literal(LT::String));
                    }
                }
            },
            PT::SingleQuote => {
                self.consume();
                let _: Result<(), ()> = (|| {
                    self.if_next_is(
                        TT::Word,
                        |this| {
                            // char literal length is checked later
                            this.consume();
                            Ok(())
                        }
                    )?;
                    self.tokenize_quote(0)?;
                    Ok(())
                })();
                return self.create_token(ST::Literal(LT::Char));
            },
            PT::Percent    => simple_pass_through!(Punctuation, self, Percent   ),
            PT::Hash       => simple_pass_through!(Punctuation, self, Hash      ),
            PT::At         => simple_pass_through!(Punctuation, self, At        ),
            PT::Dollar     => simple_pass_through!(Punctuation, self, Dollar    ),
            PT::Tilde      => simple_pass_through!(Punctuation, self, Tilde     ),
            PT::BackQuote  => simple_pass_through!(Punctuation, self, BackQuote ),
            PT::Comma      => simple_pass_through!(Punctuation, self, Comma     ),
        }
    }
}

create_syntax_tokenizer! {
    WhiteSpaceTokenizer,
    fn tokenize(&mut self) -> SyntaxToken {
        type ST = TokenKind;
        self.consume();
        self.create_token(ST::Whitespace)
    }
}

create_syntax_tokenizer! {
    UnknownTokenizer,
    fn tokenize(&mut self) -> SyntaxToken {
        type ST = TokenKind;
        self.consume();
        self.create_token(ST::Unknown)
    }
}

macro_rules! define_tokenizer_fn {
    ($MATCH:path, $TY:ident) => {
        paste! {
            #[allow(dead_code)]
            fn [<tokenize_ $TY:lower>](&mut self, offset: usize) -> Result<(), ()> {
                if let Ok(token) = self.stream.cursor.peek(offset) {
                    match token.ty {
                        $MATCH => {
                            self.consume();
                            Ok(())
                        },
                        _ => Err(()),
                    }
                } else {
                    Err(())
                }
            }
        }
    };
}

macro_rules! gen_tokenizer_fn {
    ($TOK:ident) => {
        define_tokenizer_fn!(RawTokenKind::$TOK, $TOK);
    };
    (Punctuation, $TOK:ident) => {
        define_tokenizer_fn!(RawTokenKind::Punctuation(PunctuationKind::$TOK), $TOK);
    };
    (Control, $TOK:ident) => {
        define_tokenizer_fn!(RawTokenKind::Control(ControlType::$TOK), $TOK);
    };
}

struct SyntaxTokenizer<'a, 'b> {
    span: Span,
    stream: &'a mut SyntaxTokenStream<'b>
}

impl<'a, 'b>  SyntaxTokenizer<'a, 'b>  {
    fn new(stream: &'a mut SyntaxTokenStream<'b>) -> Self {
        let span = stream.cursor.peek(0).unwrap().span;
        Self {
            span,
            stream,
        }
    }

    fn create_token(
        &mut self,
        ty: TokenKind,
    ) -> SyntaxToken {
        self.stream.pos += self.span.len();

        // If the token is a newline increment the line count.
        let line = self.stream.line;
        if let TokenKind::Control(ControlType::NewLine) = ty {
            self.stream.line += 1;
        }

        SyntaxToken {
            ty,
            span: {
                let mut span = self.span.clone();
                span.set_line(line);
                span
            },
        }
    }

    fn peek(
        &mut self,
        offset: usize,
    ) -> &RawToken {
        self.stream.cursor.peek(offset).unwrap()
    }

    fn consume(&mut self) {
        let token = self.stream.cursor.next().unwrap();
        self.span.set_end(token.span.end());
    }

    fn str_from_span(&self, span: Span) -> &str {
        &self.stream.src[span.start()..=span.end()]
    }

    gen_tokenizer_fn!(Whitespace);
    gen_tokenizer_fn!(Number);
    gen_tokenizer_fn!(Word);
    gen_tokenizer_fn!(Unknown);

    // Punctuation
    gen_tokenizer_fn!(Punctuation, Plus);
    gen_tokenizer_fn!(Punctuation, Hyphen);
    gen_tokenizer_fn!(Punctuation, UnderScore);
    gen_tokenizer_fn!(Punctuation, Asterisk);
    gen_tokenizer_fn!(Punctuation, Slash);
    gen_tokenizer_fn!(Punctuation, BackSlash);
    gen_tokenizer_fn!(Punctuation, RParen);
    gen_tokenizer_fn!(Punctuation, LParen);
    gen_tokenizer_fn!(Punctuation, RAngleBracket);
    gen_tokenizer_fn!(Punctuation, LAngleBracket);
    gen_tokenizer_fn!(Punctuation, RBrace);
    gen_tokenizer_fn!(Punctuation, LBrace);
    gen_tokenizer_fn!(Punctuation, RBracket);
    gen_tokenizer_fn!(Punctuation, LBracket);
    gen_tokenizer_fn!(Punctuation, Equals);
    gen_tokenizer_fn!(Punctuation, Pipe);
    gen_tokenizer_fn!(Punctuation, QuestionMark);
    gen_tokenizer_fn!(Punctuation, Exclamation);
    gen_tokenizer_fn!(Punctuation, Ampersand);
    gen_tokenizer_fn!(Punctuation, Period);
    gen_tokenizer_fn!(Punctuation, Colon);
    gen_tokenizer_fn!(Punctuation, SemiColon);
    gen_tokenizer_fn!(Punctuation, Quote);
    gen_tokenizer_fn!(Punctuation, SingleQuote);
    gen_tokenizer_fn!(Punctuation, Percent);
    gen_tokenizer_fn!(Punctuation, Hash);
    gen_tokenizer_fn!(Punctuation, At);
    gen_tokenizer_fn!(Punctuation, Dollar);
    gen_tokenizer_fn!(Punctuation, Tilde);
    gen_tokenizer_fn!(Punctuation, BackQuote);

    // Control
    gen_tokenizer_fn!(Control, NewLine);
    gen_tokenizer_fn!(Control, Tab);
    gen_tokenizer_fn!(Control, Null);

    /// Calls the provided function if the next token is the provided
    /// [`RawTokenType`]
    pub fn if_next_is(
        &mut self,
        token: RawTokenKind,
        mut f: impl FnMut(&mut Self) -> Result<(), ()>,
    ) -> Result<(), ()> {
        if let Ok(t) = self.stream.cursor.peek(0) {
            if t.ty == token {
                return f(self);
            } else {
                return Err(());
            }
        }
        Err(())
    }
}
