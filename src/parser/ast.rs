//std
use std::{
    collections::VecDeque,
    fmt,
    iter::Peekable,
};

// external
use paste::paste;
use thiserror::Error;

use crate::parser::{
    syntax_pass::SyntaxPassIter,
    tokenizer::SourceCode,
    CONSOLE_PRINTER,
    ControlType,
    KeywordType,
    LiteralType,
    PunctuationType,
    SyntaxToken,
    SyntaxTokenType,
};

/*
    let x: i32 = 10;

    Scope {
        Decleration<var> {
            ident: "x"
        }
        Definition<var> {
            ident: "x"
            value: Constant {
                value: 10
            }
        }
    }

    fn do_thing() {}

    Scope {
        Decleration<func> {
            ident: "do thing"
            visibiltiy: private
            sig: FnSig {
                param: []
                return: ()
            },
        }
        Definition<func> {
            ident: "do thing"
            sig: FnSig {
                param: []
                return: ()
            },
        }
    }


*/

macro_rules! gen_create_parser_fn {
    ($TY:ty) => {
        paste! {
            pub fn [<run_ $TY:lower>] (&mut self) {
                $TY::new(self).parse();
            }
        }
    };
}

pub struct AstGenerator<'a> {
    token_iter: Peekable<SyntaxPassIter<'a>>,
    input_token_buffer: VecDeque<SyntaxToken<'a>>,
    output_node_buffer: VecDeque<AstNode>,
    source_str: &'a str,
}

impl<'a> AstGenerator<'a> {
    pub fn new(source: &'a SourceCode) -> Self {
        Self {
            token_iter: SyntaxPassIter::from(source).peekable(),
            input_token_buffer: VecDeque::with_capacity(16),
            output_node_buffer: VecDeque::with_capacity(4),
            source_str: source.get_str(),
        }
    }

    pub fn parse_keyword(
        &mut self,
        keyword: KeywordType,
    ) {
        match keyword {
            KeywordType::Let => self.run_letstatementparser(),
            KeywordType::Import => {},
            KeywordType::Fn => {},
            KeywordType::Interface => {},
            KeywordType::Pub => {},
            _ => {
                panic!("bad context for keyword")
            },
            _ => {},
        }
    }

    pub fn parse_identifier(&mut self) { panic!("Invalid Syntax") }

    pub fn parse_control(
        &mut self,
        cntl: ControlType,
    ) {
        // when we return from this iteration the buffer is cleared
        // This is a dirty way to consume tokens
        self.buffer_current_input_token();
    }

    pub fn parse_whitespace(&mut self) { self.consume_whitespace_token(true); }

    pub fn parse_unknown(&mut self) { panic!("Illegal Character") }

    pub fn parse_punctuation(
        &mut self,
        punc: PunctuationType,
    ) {
        panic!("Illegal Character")
    }

    #[rustfmt::skip]
    pub fn parse_literal (
        &mut self,
        keyword: LiteralType,
    ) {
        match keyword {
            LiteralType::String  => self.run_letstatementparser(),
            LiteralType::Char    => self.run_letstatementparser(),
            LiteralType::Float   => self.run_letstatementparser(),
            LiteralType::Integer => self.run_letstatementparser(),
            _ => {},
        }
    }

    gen_create_parser_fn!(LetStatementParser);

    // Private --------------------------------------------------------

    fn consume_whitespace_token(
        &mut self,
        err_if_none: bool,
    ) -> Result<usize, ParsingError> {
        if let Some(token) = self.peek_next_input_token() {
            match token.ty {
                SyntaxTokenType::Whitespace => {
                    self.consume_current_input_token();
                    Ok(1)
                },
                _ => {
                    if err_if_none {
                        Err(ParsingError::NoMatchFound)
                    } else {
                        Ok(0)
                    }
                },
            }
        } else {
            Err(ParsingError::EndOfTokens)
        }
    }

    /// Pushes the token into the buffer
    fn buffer_current_input_token(&mut self) {
        let token = self.token_iter.next().unwrap();
        self.input_token_buffer.push_back(token);
    }

    /// Clears the buffer, without freeing the underlying memory
    fn clear_input_buffer(&mut self) { self.input_token_buffer.clear() }

    fn get_last_buffered_input_token(&self) -> &SyntaxToken<'a> {
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

    fn peek_next_input_token(&mut self) -> Option<&SyntaxToken> { self.token_iter.peek() }
    fn consume_current_input_token(&mut self) -> Option<SyntaxToken> { self.token_iter.next() }

    fn buffer_output_Node(
        &mut self,
        node: AstNode,
    ) {
        self.output_node_buffer.push_back(node)
    }

    fn get_buffered_output_node(&mut self) -> Option<AstNode> {
        self.output_node_buffer.pop_front()
    }

    fn num_buffered_input_tokens(&self) -> usize { self.input_token_buffer.len() }

    /// This function will create a syntax token from the provided type.
    fn create_and_buffer_syntax_node(
        &mut self,
        number_of_tokens: usize,
        node: AstNode,
    ) {
        let offset = self
            .input_token_buffer
            .front()
            .unwrap()
            .data
            .get_source_location()
            .offset;

        // remove input tokens from the buffer when they have been used
        for _ in 0..number_of_tokens {
            self.input_token_buffer.pop_front();
        }

        self.buffer_output_Node(node);
    }
}

impl<'a> Iterator for AstGenerator<'a> {
    type Item = AstNode;

    #[rustfmt::skip]
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.token_iter.next();  
        let mut console_printer = CONSOLE_PRINTER.lock().unwrap();
        // Temp implimentation to get the console printer to work
        unsafe { 
            console_printer.print_syntax_token(token);
        }; 
        std::thread::sleep(std::time::Duration::from_millis(20));
        if token.is_some() {
            Some(AstNode {
                ty: AstNodeType::Scope,
                data: Box::new(ScopeNodeData {})
            })
        } else {
            None
        }

        // loop {
        //     // Pull Syntax tokens from the internal buffer first before advancing the
        //     // underlying iterator
        //     if let Some(node) = self.get_buffered_output_node() {
        //         return Some(node);
        //     } else {
        //         if let Some(t) = self.peek_next_input_token() {
        //             match t.ty {
        //                 SyntaxTokenType::Punctuation(p) => self.parse_punctuation(p),
        //                 SyntaxTokenType::Identifier => self.parse_identifier(),
        //                 SyntaxTokenType::Literal(l) => self.parse_literal(l),
        //                 SyntaxTokenType::Keyword(k) => self.parse_keyword(k),
        //                 SyntaxTokenType::Whitespace => self.parse_whitespace(),
        //                 SyntaxTokenType::Control(c) => self.parse_control(c),
        //                 SyntaxTokenType::Unknown    => self.parse_unknown(),
        //                 SyntaxTokenType::Null => {},
        //             }
        //             self.clear_input_buffer();
        //         } else {
        //             return None;
        //         }
        //     }
        // }
    }
}

pub struct Parser;
impl<'a> Parser {
    pub fn new() -> Self { Self }

    pub fn parse(
        &self,
        source: &str,
    ) -> Ast {
        let ast = Ast::new();
        let source_code = SourceCode::new(source);
        for _node in AstGenerator::new(&source_code) {
            //println!("{:#?}", node);
        }

        ast
    }

    // Private ----------------------------------------------------------------
}

#[derive(Debug)]
pub struct Ast {
    node: AstNode,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            node: AstNode {
                ty: AstNodeType::Scope,
                data: Box::new(ScopeNodeData {}),
            },
        }
    }

    // fn get_node(&self) -> &AstNode { &self.node }
    fn get_node_mut(&mut self) -> &mut AstNode { &mut self.node }
}

pub enum StatementType {
    Let(LetType),
}

pub struct AstNode {
    ty: AstNodeType,
    data: Box<dyn AstNodeData>,
}

impl fmt::Debug for AstNode {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        f.debug_struct("AstNode")
            .field("type", &"AstNodeType(..)")
            .field("data", &"data")
            .finish()
    }
}

#[derive(Debug)]
pub enum AstNodeType {
    Scope,
    Expression(ExpressionType),
}

#[derive(Debug)]
pub enum ExpressionType {
    Return,
    UnaryOperator,
    BinaryOperator(BinaryOperatorType),
}

#[derive(Debug)]
pub enum BinaryOperatorType {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    MemberAccess,
}

#[derive(Debug, Clone, Copy)]
pub struct ScopeNodeData;
impl AstNodeData for ScopeNodeData {}

#[derive(Debug, Clone, Copy)]
pub struct Expression;
impl AstNodeData for Expression {}

pub trait AstNodeData {}

pub struct LetAstNode<'a> {
    ty: LetType,
    ident: &'a str,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LetType {
    /// `let x: i32;`
    ForwardDeclarationWithType,
    /// `let x;`
    ForwardDeclarationWithoutType,
    /// `let x: i32 = 10;`
    DefinitionWithType,
    /// `let x = 10;`
    DefinitionWithoutType,
}

#[repr(transparent)]
pub struct LetStatementParser<'a, 'b> {
    parser: AstNodeGeneratorParser<'a, 'b>,
}
impl<'a, 'b> LetStatementParser<'a, 'b> {
    fn new(gen: &'b mut AstGenerator<'a>) -> Self {
        Self {
            parser: AstNodeGeneratorParser {
                gen,
            },
        }
    }

    fn parse(&mut self) {
        /*
            RULES.
                [] = Must have tokens;
                <> = Optional tokens
                !  = Must Have Whitespace
                ?  = Optional Whitespace

            Let Statement:
                1.  [Let!<_>Ident?<:?Ident>?=?Expr?;]
                2.  [Let!<_>Ident?<:?Ident>?;]

        */
        let mut count = 0;
        let mut has_type_info = false;
        let mut has_definition = false;

        let mut parse = || -> Result<(), ParsingError> {
            self.buffer_current_input_token();
            count += self.consume_whitespace_token(true)?;
            count += self.parse_identifier()?;
            count += self.consume_whitespace_token(false)?;

            self.if_next_token_is(
                SyntaxTokenType::Punctuation(PunctuationType::LBrace),
                |this| Ok(()),
            )?;

            self.if_next_token_is(
                SyntaxTokenType::Punctuation(PunctuationType::Colon),
                |this| {
                    count += this.parse_colon()?;
                    count += this.consume_whitespace_token(false)?;
                    count += this.parse_identifier()?;
                    count += this.consume_whitespace_token(false)?;
                    has_type_info = true;
                    Ok(())
                },
            )?;
            self.if_next_token_is(
                SyntaxTokenType::Punctuation(PunctuationType::Equals),
                |this| {
                    count += this.parse_equals()?;
                    count += this.consume_whitespace_token(false)?;
                    count += this.parse_string()?;
                    count += this.consume_whitespace_token(false)?;
                    has_definition = true;
                    Ok(())
                },
            )?;
            count += self.parse_semicolon()?;
            Ok(())
        };

        match parse() {
            Ok(_) => {
                let let_type = {
                    if has_type_info {
                        if has_definition {
                            LetType::DefinitionWithType
                        } else {
                            LetType::ForwardDeclarationWithType
                        }
                    } else {
                        if has_definition {
                            LetType::DefinitionWithoutType
                        } else {
                            LetType::ForwardDeclarationWithoutType
                        }
                    }
                };

                println!("{:#?}", let_type);
            },
            Err(e) => {
                println!("{:#?}", e);
            },
        }
    }
}

impl<'a, 'b> std::ops::Deref for LetStatementParser<'a, 'b> {
    type Target = AstNodeGeneratorParser<'a, 'b>;
    fn deref(&self) -> &Self::Target { &self.parser }
}

impl<'a, 'b> std::ops::DerefMut for LetStatementParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.parser }
}

#[repr(transparent)]
pub struct LiteralParser<'a, 'b> {
    parser: AstNodeGeneratorParser<'a, 'b>,
}

impl<'a, 'b> LiteralParser<'a, 'b> {
    fn new(gen: &'b mut AstGenerator<'a>) -> Self {
        Self {
            parser: AstNodeGeneratorParser {
                gen,
            },
        }
    }

    fn parse(&mut self) {}
}

impl<'a, 'b> std::ops::Deref for LiteralParser<'a, 'b> {
    type Target = AstNodeGeneratorParser<'a, 'b>;
    fn deref(&self) -> &Self::Target { &self.parser }
}

impl<'a, 'b> std::ops::DerefMut for LiteralParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.parser }
}

// ===============================================================================

macro_rules! define_gen_parse_fn {
    ($MATCH:path, $TY:ident) => {
        paste! {
            #[allow(dead_code)]
            fn [<parse_ $TY:lower >](&mut self) -> Result<usize, ParsingError> {
                if let Some(token) = self.gen.peek_next_input_token() {
                    match token.ty {
                        $MATCH => {
                            self.gen.buffer_current_input_token();
                            Ok(1)
                        },
                        _ => Err(ParsingError::NoMatchFound),
                    }
                } else {
                    Err(ParsingError::NoMatchFound)
                }
            }
        }
    };
}

macro_rules! gen_parse_fn {
    ($TOK:ident) => {
        define_gen_parse_fn!(SyntaxTokenType::$TOK, $TOK);
    };
    (Punctuation, $TOK:ident) => {
        define_gen_parse_fn!(SyntaxTokenType::Punctuation(PunctuationType::$TOK), $TOK);
    };
    (Keyword, $TOK:ident) => {
        define_gen_parse_fn!(SyntaxTokenType::Keyword(KeywordType::$TOK), $TOK);
    };
    (Literal, $TOK:ident) => {
        define_gen_parse_fn!(SyntaxTokenType::Literal(LiteralType::$TOK), $TOK);
    };
    (Control, $TOK:ident) => {
        define_gen_parse_fn!(SyntaxTokenType::Control(ControlType::$TOK), $TOK);
    };
}

pub struct AstNodeGeneratorParser<'a, 'b> {
    pub(self) gen: &'b mut AstGenerator<'a>,
}

impl<'a, 'b> AstNodeGeneratorParser<'a, 'b> {
    gen_parse_fn!(Identifier);
    gen_parse_fn!(Whitespace);
    gen_parse_fn!(Unknown);

    // Keywords
    gen_parse_fn!(Keyword, Let);
    gen_parse_fn!(Keyword, Import);
    gen_parse_fn!(Keyword, If);
    gen_parse_fn!(Keyword, Else);
    gen_parse_fn!(Keyword, Fn);
    gen_parse_fn!(Keyword, This);
    gen_parse_fn!(Keyword, Match);
    gen_parse_fn!(Keyword, Pub);
    gen_parse_fn!(Keyword, Class);
    gen_parse_fn!(Keyword, Interface);
    gen_parse_fn!(Keyword, Return);

    // Literal
    gen_parse_fn!(Literal, String);
    gen_parse_fn!(Literal, Char);
    gen_parse_fn!(Literal, Integer);
    gen_parse_fn!(Literal, Float);

    // Punctuation
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

    // Control
    gen_parse_fn!(Control, NewLine);
    gen_parse_fn!(Control, Tab);
    gen_parse_fn!(Control, Null);

    /// Calls the provided function if the next token is the provided
    /// [`SyntaxTokenType`]
    pub fn if_next_token_is<F: FnMut(&mut Self) -> Result<(), ParsingError>>(
        &mut self,
        token: SyntaxTokenType,
        mut f: F,
    ) -> Result<(), ParsingError> {
        if let Some(t) = self.peek_next_input_token() {
            if t.ty == token {
                return f(self);
            } else {
                return Ok(());
            }
        }
        Err(ParsingError::EndOfTokens)
    }
}

impl<'a, 'b> std::ops::Deref for AstNodeGeneratorParser<'a, 'b> {
    type Target = AstGenerator<'a>;
    fn deref(&self) -> &Self::Target { &self.gen }
}

impl<'a, 'b> std::ops::DerefMut for AstNodeGeneratorParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.gen }
}

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Token Stream Ended")]
    EndOfTokens,
    #[error("No match was found for token")]
    NoMatchFound,
}
