//std
use std::{
    collections::VecDeque,
    iter::Peekable,
};

// external
use thiserror::Error;
use paste::paste;

// internal
use crate::{
    parser::{
        tokenizer::{
            SourceCode,
            Token,
            TokenData,
            TokenIter,
            TokenType,
        },
        ControlType,
        KeywordType,
        LiteralType,
        PunctuationType,

        SyntaxToken,
        SyntaxTokenType,
    },
    util::{
        self,
        SourceLocation,
    },
};
use macros::create_token_handler;

/// Simple macro that proves a basic converting implmentation
#[rustfmt::skip]
macro_rules! create_token {
    (Punctuation, $SELF:ident, $TY:ident) => {{
        $SELF.create_and_buffer_syntax_token(
            1,
            SyntaxTokenType::Punctuation(PunctuationType::$TY)
        );
        return;
    }};
    (Keyword, $SELF:ident, $TY:ident) => {{
        $SELF.create_and_buffer_syntax_token(
            1,
            SyntaxTokenType::Keyword(KeywordType::$TY)
        );
        return;
    }};
    (Control, $SELF:ident, $TY:ident) => {{
        $SELF.create_and_buffer_syntax_token(
            1,
            SyntaxTokenType::Control(ControlType::$TY)
        );
        return;
    }};
}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler! [
    WordTokenHandler,
    pub fn tokenize(&mut self) {
        let token = self.get_last_buffered_input_token();
        match token.data.token_str() {
            "let"       => create_token!(Keyword, self, Let),
            "import"    => create_token!(Keyword, self, Import),
            "if"        => create_token!(Keyword, self, If),
            "else"      => create_token!(Keyword, self, Else),
            "fn"        => create_token!(Keyword, self, Fn),
            "this"      => create_token!(Keyword, self, This),
            "match"     => create_token!(Keyword, self, Match),
            "pub"       => create_token!(Keyword, self, Pub),
            "class"     => create_token!(Keyword, self, Class),
            "interface" => create_token!(Keyword, self, Interface),
            "return"    => create_token!(Keyword, self, Return),
            "module"    => create_token!(Keyword, self, Module),
            "const"     => create_token!(Keyword, self, Const),
            _ => {
                self.create_identifer_token();
            },
        }
    }
];



impl<'a, 'b> WordTokenHandler<'a, 'b> {

}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler!{
    NumberTokenHandler,
    pub fn tokenize(&self) {

    }
}

impl<'a, 'b> WordTokenHandler<'a, 'b> {

}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler!{
    PunctuationTokenHandler,
    pub fn tokenize(&self) {

    }
}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler!{
    ControlTokenHandler,
    pub fn tokenize(&self) {

    }
}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler!{
    WhitespaceTokenHandler,
    pub fn tokenize(&self) {

    }
}

// UnknownTokenHandler ----------------------------------------------------------------

create_token_handler!{
    UnknownTokenHandler,
    pub fn tokenize(&self) {

    }
}

/// SyntaxTokenizer ----------------------------------------------------------------

macro_rules! define_gen_parse_fn {
    ($MATCH:path, $TY:ident) => {
        paste! {
            #[allow(dead_code)]
            fn [<tokenize_ $TY:lower >](&mut self) -> Result<usize, TokenizingError> {
                if let Some(token) = self.syntax_pass.peek_next_input_token() {
                    match token.ty {
                        $MATCH => {
                            self.syntax_pass.buffer_current_input_token();
                            Ok(1)
                        },
                        _ => Err(TokenizingError::NoMatchFound),
                    }
                } else {
                    Err(TokenizingError::NoMatchFound)
                }
            }
        }
    };
}

macro_rules! gen_parse_fn {
    ($TOK:ident) => {
        define_gen_parse_fn!(TokenType::$TOK, $TOK);
    };
    (Punctuation, $TOK:ident) => {
        define_gen_parse_fn!(TokenType::Punctuation(PunctuationType::$TOK), $TOK);
    };
    (Control, $TOK:ident) => {
        define_gen_parse_fn!(TokenType::Control(ControlType::$TOK), $TOK);
    };
}

pub struct SyntaxTokenizer<'a, 'b> {
    pub(self) syntax_pass: &'b mut SyntaxPassIter<'a>
}

impl<'a, 'b> SyntaxTokenizer<'a, 'b> {
    gen_parse_fn!(Word);
    gen_parse_fn!(Number);
    gen_parse_fn!(Punctuation, Plus);
    gen_parse_fn!(Punctuation, Hyphen);
    gen_parse_fn!(Punctuation, UnderScore);
    gen_parse_fn!(Punctuation, Asterisk);
    gen_parse_fn!(Punctuation, Slash);
    gen_parse_fn!(Punctuation, BackSlash);
    gen_parse_fn!(Punctuation, RParen);
    gen_parse_fn!(Punctuation, LParen);
    gen_parse_fn!(Punctuation, RAngleBracket);
    gen_parse_fn!(Punctuation, LAngleBracket);
    gen_parse_fn!(Punctuation, RBrace);
    gen_parse_fn!(Punctuation, LBrace);
    gen_parse_fn!(Punctuation, RBracket);
    gen_parse_fn!(Punctuation, LBracket);
    gen_parse_fn!(Punctuation, Equals);
    gen_parse_fn!(Punctuation, Pipe);
    gen_parse_fn!(Punctuation, QuestionMark);
    gen_parse_fn!(Punctuation, Exclamation);
    gen_parse_fn!(Punctuation, Ampersand);
    gen_parse_fn!(Punctuation, Period);
    gen_parse_fn!(Punctuation, Colon);
    gen_parse_fn!(Punctuation, SemiColon);
    gen_parse_fn!(Punctuation, Quote);
    gen_parse_fn!(Punctuation, SingleQuote);
    gen_parse_fn!(Punctuation, Percent);
    gen_parse_fn!(Punctuation, Hash);
    gen_parse_fn!(Punctuation, At);
    gen_parse_fn!(Punctuation, Dollar);
    gen_parse_fn!(Punctuation, Tilde);
    gen_parse_fn!(Punctuation, BackQuote);
    gen_parse_fn!(Control, NewLine);
    gen_parse_fn!(Control, Tab);
    gen_parse_fn!(Control, Null);
    gen_parse_fn!(Whitespace);
    gen_parse_fn!(Unknown);

    /// Calls the provided function if the next token is the provided
    /// [`SyntaxTokenType`]
    pub fn if_next_token_is<F: FnMut(&mut Self) -> Result<(), TokenizingError>>(
        &mut self,
        token: TokenType,
        mut f: F,
    ) -> Result<(), TokenizingError> {
        if let Some(t) = self.peek_next_input_token() {
            if t.ty == token {
                return f(self);
            } else {
                return Ok(());
            }
        }
        Err(TokenizingError::EndOfTokens)
    }
}

impl<'a, 'b> std::ops::Deref for SyntaxTokenizer<'a, 'b> {
    type Target = SyntaxPassIter<'a>;
    fn deref(&self) -> &Self::Target { &self.syntax_pass }
}

impl<'a, 'b> std::ops::DerefMut for SyntaxTokenizer<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.syntax_pass }
}

#[derive(Error, Debug)]
pub enum TokenizingError {
    #[error("Token Stream Ended")]
    EndOfTokens,
    #[error("No match was found for token")]
    NoMatchFound,
}

pub struct SyntaxPassIter<'a> {
    token_iter: Peekable<TokenIter<'a>>,
    input_token_buffer: VecDeque<Token<'a>>,
    output_token_buffer: VecDeque<SyntaxToken<'a>>,
    source_str: &'a str,
}

impl<'a, 'b> From<&'a SourceCode<'a>> for SyntaxPassIter<'a> {
    fn from(sc: &'a SourceCode) -> Self { Self::new(sc) }
}

impl<'a> SyntaxPassIter<'a> {
    pub fn new(source: &'a SourceCode) -> Self {
        Self {
            token_iter: TokenIter::from(source).peekable(),
            input_token_buffer: VecDeque::with_capacity(4),
            output_token_buffer: VecDeque::with_capacity(4),
            source_str: source.get_str(),
        }
    }

    #[rustfmt::skip]
    pub fn word_pass(&mut self) {
        let token = self.get_last_buffered_input_token();

        // Is matching against strings fast? Is it better to have a
        // dedicated string comparing algorithm?
        match token.data.token_str() {
            "let"       => create_token!(Keyword, self, Let),
            "import"    => create_token!(Keyword, self, Import),
            "if"        => create_token!(Keyword, self, If),
            "else"      => create_token!(Keyword, self, Else),
            "fn"        => create_token!(Keyword, self, Fn),
            "this"      => create_token!(Keyword, self, This),
            "match"     => create_token!(Keyword, self, Match),
            "pub"       => create_token!(Keyword, self, Pub),
            "class"     => create_token!(Keyword, self, Class),
            "interface" => create_token!(Keyword, self, Interface),
            "return"    => create_token!(Keyword, self, Return),
            "module"    => create_token!(Keyword, self, Module),
            "const"     => create_token!(Keyword, self, Const),
            _ => {
                self.create_identifer_token();
            },
        }
    }

    pub fn number_pass(&mut self) {
        let mut period_count = 0;
        loop {
            if let Some(t) = self.peek_next_input_token() {
                match t.ty {
                    TokenType::Number => {
                        if period_count == 1 {
                            self.buffer_current_input_token();
                            self.create_and_buffer_syntax_token(
                                self.num_buffered_input_tokens(),
                                SyntaxTokenType::Literal(LiteralType::Float),
                            );
                        return;
                        }
                    },
                    TokenType::Punctuation(ty) => match ty {
                        PunctuationType::Period => {
                            if period_count == 0 {
                                self.buffer_current_input_token();
                                period_count += 1;
                            } else {
                                /*
                                    currently peeked = [.]
                                    buffered = [num][.]
                                */
                                self.create_and_buffer_syntax_token(
                                    1,
                                    SyntaxTokenType::Literal(LiteralType::Integer),
                                );
                                self.create_and_buffer_syntax_token(
                                    1,
                                    SyntaxTokenType::Punctuation(PunctuationType::Period),
                                );
                                return;
                            }
                        },
                        _ => {
                            break;
                        },
                    },
                    TokenType::Word => {
                        if period_count == 0 {
                            break;
                        } else {
                            /*
                                currently peeked = [word]
                                buffered = [num][.]
                            */
                            self.create_and_buffer_syntax_token(
                                1,
                                SyntaxTokenType::Literal(LiteralType::Integer),
                            );
                            return;
                        }
                    },
                    _ => {
                        if period_count == 1 {
                            /*
                                currently peeked = [any]
                                buffered = [num][.]
                            */
                            self.create_and_buffer_syntax_token(
                                1,
                                SyntaxTokenType::Literal(LiteralType::Integer),
                            );
                            self.create_and_buffer_syntax_token(
                                1,
                                SyntaxTokenType::Punctuation(PunctuationType::Period),
                            );
                            return;
                        } else {
                            break;
                        }
                    },
                }
            } else {
                break;
            }
        }
        // If we end up here there was either no next token
        // or the next token wasn't part of a number
        self.create_and_buffer_syntax_token(
            self.num_buffered_input_tokens(),
            SyntaxTokenType::Literal(LiteralType::Integer),
        );
    }

    #[rustfmt::skip]
    pub fn punctuation_pass(
        &mut self,
        inital_punct: PunctuationType,
    ) {
        match inital_punct {
            PunctuationType::Plus       => create_token!(Punctuation, self, Plus),
            PunctuationType::Hyphen     => create_token!(Punctuation, self, Hyphen),
            PunctuationType::UnderScore => create_token!(Punctuation, self, UnderScore),
            PunctuationType::Asterisk   => create_token!(Punctuation, self, Asterisk),
            PunctuationType::Slash      => create_token!(Punctuation, self, Slash),
            PunctuationType::BackSlash  => create_token!(Punctuation, self, BackSlash),
            PunctuationType::RParen     => create_token!(Punctuation, self, RParen),
            PunctuationType::LParen     => create_token!(Punctuation, self, LParen),
            PunctuationType::RAngleBracket => create_token!(Punctuation, self, RAngleBracket),
            PunctuationType::LAngleBracket => create_token!(Punctuation, self, LAngleBracket),
            PunctuationType::RBrace     => create_token!(Punctuation, self, RBrace),
            PunctuationType::LBrace     => create_token!(Punctuation, self, LBrace),
            PunctuationType::RBracket   => create_token!(Punctuation, self, RBracket),
            PunctuationType::LBracket   => create_token!(Punctuation, self, LBracket),
            PunctuationType::Equals     => create_token!(Punctuation, self, Equals),
            PunctuationType::Pipe       => create_token!(Punctuation, self, Pipe),
            PunctuationType::QuestionMark => create_token!(Punctuation, self, QuestionMark),
            PunctuationType::Exclamation  => create_token!(Punctuation, self, Exclamation),
            PunctuationType::Ampersand  => create_token!(Punctuation, self, Ampersand),
            PunctuationType::Period     => create_token!(Punctuation, self, Period),
            PunctuationType::Colon      => create_token!(Punctuation, self, Colon),
            PunctuationType::SemiColon  => create_token!(Punctuation, self, SemiColon),
            PunctuationType::Quote => {
                while let Some(token) = self.peek_next_input_token() {
                    match token.ty {
                        TokenType::Punctuation(PunctuationType::Quote) => {
                            self.buffer_current_input_token();
                            self.create_and_buffer_syntax_token(
                                self.num_buffered_input_tokens(),
                                SyntaxTokenType::Literal(LiteralType::String),
                            );
                            return;
                        },
                        _ => {
                            // We are inside a string literal
                            self.buffer_current_input_token();
                        },
                    }
                }
            },
            PunctuationType::SingleQuote => {
                // We are inside a char literal
                if let Some(token) = self.peek_next_input_token() {
                    match token.ty {
                        TokenType::Punctuation(PunctuationType::SingleQuote) => {
                            util::panic_here("char literals must be 1 codepoint long");
                        },
                        _ => {
                            if token.data.len() != 1 {
                                util::panic_here("char litterals must be 1 code point long");
                            }
                            self.buffer_current_input_token();

                            if let Some(token) = self.peek_next_input_token() {
                                match token.ty {
                                    TokenType::Punctuation(PunctuationType::SingleQuote) => {
                                        self.buffer_current_input_token();
                                        // Sanity check
                                        debug_assert!(self.num_buffered_input_tokens() == 3);
                                        self.create_and_buffer_syntax_token(
                                            self.num_buffered_input_tokens(),
                                            SyntaxTokenType::Literal(LiteralType::Char),
                                        );
                                    },
                                    _ => {
                                        util::panic_here("char litterals must be 1 code point long");
                                    },
                                }
                            }
                        },
                    }
                }
            },
            PunctuationType::Percent    => create_token!(Punctuation, self, Percent),
            PunctuationType::Hash       => create_token!(Punctuation, self, Hash),
            PunctuationType::At         => create_token!(Punctuation, self, At),
            PunctuationType::Dollar     => create_token!(Punctuation, self, Dollar),
            PunctuationType::Tilde      => create_token!(Punctuation, self, Tilde),
            PunctuationType::BackQuote  => create_token!(Punctuation, self, BackQuote),
        }
    }

    #[rustfmt::skip]
    pub fn control_pass(
        &mut self,
        control_type: ControlType,
    ) {
        match control_type {
            ControlType::NewLine => create_token!(Control, self, NewLine),
            ControlType::Tab     => create_token!(Control, self, Tab),
            ControlType::Null    => create_token!(Control, self, Null),
        }
    }

    pub fn whitespace_pass(&mut self) {
        self.create_and_buffer_syntax_token(
            self.num_buffered_input_tokens(),
            SyntaxTokenType::Whitespace,
        );
    }
    
    pub fn unknown_pass(&mut self) {
        self.create_and_buffer_syntax_token(
            self.num_buffered_input_tokens(),
            SyntaxTokenType::Unknown,
        );
    }
    
    pub fn peek_next_input_token(&mut self) -> Option<&Token> { self.token_iter.peek() }

    /// Pushes the token into the buffer
    pub fn buffer_current_input_token(&mut self) {
        let token = self.token_iter.next().unwrap();
        self.input_token_buffer.push_back(token);
    }

    // Private --------------------------------------------------------

    fn create_identifer_token(&mut self) {
        loop {
            if let Some(token) = self.peek_next_input_token() {
                match token.ty {
                    TokenType::Punctuation(PunctuationType::UnderScore) => {
                        self.buffer_current_input_token();
                    },
                    TokenType::Number => {
                        self.buffer_current_input_token();
                    },
                    TokenType::Word => {
                        self.buffer_current_input_token();
                        self.create_and_buffer_syntax_token(
                            self.num_buffered_input_tokens(),
                            SyntaxTokenType::Identifier,
                        );
                        if let Some(t) = self.peek_next_input_token() {
                            match t.ty {
                                TokenType::Punctuation(PunctuationType::UnderScore)
                                | TokenType::Number => {}
                                _ => return,
                            }
                        }
                    },
                    // legal but not part of the ident
                    TokenType::Whitespace
                    | TokenType::Punctuation(PunctuationType::Colon)
                    | TokenType::Punctuation(PunctuationType::SemiColon)
                    | TokenType::Punctuation(PunctuationType::Equals)
                    
                    => return,

                    _ => {
                        self.create_and_buffer_syntax_token(
                            self.num_buffered_input_tokens(),
                            SyntaxTokenType::Identifier,
                        );
                        return;
                    },
                }
            }
        }
    }


    /// Clears the buffer, without freeing the underlying memory
    fn clear_input_buffer(&mut self) { self.input_token_buffer.clear() }

    fn get_last_buffered_input_token(&self) -> &Token<'a> {
        self.input_token_buffer
            .back()
            .expect("Tried to access empty input buffer")
    }

    fn get_source_str(
        &self,
        offset: usize,
        number_of_tokens: usize,
    ) -> &'a str {
        let buffer = &self.input_token_buffer;
        debug_assert!(number_of_tokens > 0);
        // get end token data
        let end_token = buffer.get(number_of_tokens - 1).unwrap();
        let end_token_offset = end_token.data.get_source_location().offset;
        let end_token_length = end_token.data.len();
        let length = end_token_offset + end_token_length;
        let source_ref = &self.source_str[offset..length];
        source_ref
    }


    fn buffer_output_token(
        &mut self,
        token: SyntaxToken<'a>,
    ) {
        self.output_token_buffer.push_back(token)
    }

    fn get_buffered_output_token(&mut self) -> Option<SyntaxToken<'a>> {
        self.output_token_buffer.pop_front()
    }

    fn num_buffered_input_tokens(&self) -> usize { self.input_token_buffer.len() }

    /// This function will create a syntax token from the provided type.
    fn create_and_buffer_syntax_token(
        &mut self,
        number_of_tokens: usize,
        ty: SyntaxTokenType,
    ) {
        let offset = self
            .input_token_buffer
            .front()
            .unwrap()
            .data
            .get_source_location()
            .offset;

        let syntax_token = SyntaxToken {
            ty,
            data: TokenData::new(
                self.get_source_str(offset, number_of_tokens),
                SourceLocation::new(0, offset, None),
            ),
        };
        // remove input tokens from the buffer when they have been used
        for _ in 0..number_of_tokens {
            self.input_token_buffer.pop_front();
        }
        self.buffer_output_token(syntax_token);
    }
}

impl<'a> Iterator for SyntaxPassIter<'a> {
    type Item = SyntaxToken<'a>;

    #[rustfmt::skip]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Pull Syntax tokens from the internal buffer first before advancing the
            // underlying iterator
            if let Some(token) = self.get_buffered_output_token() {
                return Some(token);
            } else {
                if let Some(t) = self.peek_next_input_token() {
                    let ty = t.ty; // Copy the ty for the borrow checker
                    self.buffer_current_input_token();
                    match ty {
                        TokenType::Word           => self.word_pass(),
                        TokenType::Number         => self.number_pass(),
                        TokenType::Punctuation(p) => self.punctuation_pass(p),
                        TokenType::Control(c)     => self.control_pass(c),
                        TokenType::Whitespace     => self.whitespace_pass(),
                        TokenType::Unknown        => self.unknown_pass(),
                    }
                    self.clear_input_buffer();
                } else {
                    return None;
                }
            }
        }
    }
}

