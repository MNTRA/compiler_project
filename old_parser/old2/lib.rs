mod errors;
mod expressions;
mod literals;
mod punctuation;

use std::todo;

use bitflags::bitflags;
use collections::stream::Dynamic;
use diagnostics::{
    Diagnostics,
    Reporter,
};
use lexer::{
    console_printer::CONSOLE_PRINTER,
    KeywordType,
    Span,
    SyntaxToken,
    SyntaxTokenStream,
    SyntaxTokenType,
};
use thiserror::Error;

pub fn parse_string(
    src: &String,
    reporter: Reporter,
) {
    pretty_print_tokens(src);
    //print_tokens(src);
    // let stream = SyntaxTokenStream::new(&src);
    // let out = ParseStream::from(stream).parse::<expressions::
    // Expression>(reporter); println!("{:#?}", out);

    let _ = parse(&src);
}

pub struct ParseStream<'src> {
    stream: Dynamic<SyntaxTokenStream<'src>>,
}

impl<'a> From<SyntaxTokenStream<'a>> for ParseStream<'a> {
    fn from(stream: SyntaxTokenStream<'a>) -> Self {
        Self {
            stream: Dynamic::new(stream),
        }
    }
}

impl<'src> ParseStream<'src> {
    #[inline]
    pub fn parse<T: Parse<'src>>(
        &mut self,
        reporter: Reporter,
    ) -> ParseResult<T::Output> {
        self.skip_whitespace_and_cc()?;
        self.parse_immediate::<T>(reporter)
    }

    #[inline]
    pub fn parse_immediate<T: Parse<'src>>(
        &mut self,
        reporter: Reporter,
    ) -> ParseResult<T::Output> {
        T::parse(&mut *self, reporter)
    }

    #[inline]
    fn skip_whitespace_and_cc(&mut self) -> ParseResult<()> {
        loop {
            let token = self.peek_immediate(0)?;
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(..) => {
                    self.stream.consume().unwrap();
                },
                _ => return Ok(()),
            }
        }
    }

    pub fn next_token(&mut self) -> ParseResult<SyntaxToken<'src>> {
        self.skip_whitespace_and_cc()?;
        self.peek_immediate(0)
    }

    /// Peeks the next token in the stream without passing over whitespace
    /// or cc tokens
    pub fn peek_immediate(
        &mut self,
        offset: usize,
    ) -> ParseResult<SyntaxToken<'src>> {
        match self.stream.peek(offset) {
            // TODO (George): Can we avoid the clone??
            // Does cloning this even matter??
            Ok(item) => Ok(item.clone()),
            Err(_) => Err(ParseError::EndOfTokenStream),
        }
    }

    pub fn peek_next(
        &mut self,
        mut offset: usize,
    ) -> Option<SyntaxToken<'src>> {
        while let Ok(token) = self.peek_immediate(offset) {
            match token.ty {
                SyntaxTokenType::Whitespace | SyntaxTokenType::Control(..) => {
                    offset += 1;
                },
                _ => return Some(token),
            }
        }
        None
    }

    pub fn consume(&mut self) { self.stream.consume().unwrap(); }
    pub fn consume_with_report(
        &mut self,
        reporter: &mut Reporter,
    ) {
        match self.stream.consume() {
            Ok(token) => {},
            Err(..) => unreachable!(),
        }
    }

    pub fn position(&self) -> usize { 0 }
}

pub trait Parse<'src> {
    type Output;
    fn parse(
        stream: &mut ParseStream<'src>,
        reporter: Reporter,
    ) -> ParseResult<Self::Output>;
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("NoTokens")]
    EndOfTokenStream,
    #[error("UnexpectedToken [{found}]")]
    UnexpectedToken {
        line: usize,
        span: Span,
        expected: &'static str,
        found: SyntaxTokenType,
    },
    /* #[error("UnexpectedToken [{0}]")]
     * UnexpectedToken(SyntaxTokenType), */
}

impl ParseError {
    pub fn code(&self) -> u32 {
        match self {
            ParseError::EndOfTokenStream => 0,
            ParseError::UnexpectedToken {
                ..
            } => 1,
        }
    }
}

fn pretty_print_tokens(src: &str) {
    let mut console = CONSOLE_PRINTER.lock().unwrap();
    let mut stream = SyntaxTokenStream::new(src);
    loop {
        let token = stream.next();
        unsafe { console.print_syntax_token(token) }
        if token.is_none() {
            break;
        }
    }
}

fn print_tokens(src: &str) {
    let stream = SyntaxTokenStream::new(src);
    for token in stream {
        println!("{:#?}", token.ty);
    }
}

#[derive(Default)]
pub struct Scope {
    imports: Vec<()>,
    functions: Vec<()>,
}

#[derive(Default)]
pub struct Ast {
    root: Scope,
}

pub struct Ctx {
    ast: Ast,
    stack: StateStack,
    offset: usize,
    token_complete: bool,
}

#[rustfmt::skip]
impl<'src> Ctx {
    fn new() -> Self {
        Self {
            ast: Ast::default(),
            stack: StateStack::new(),
            offset: 0,
            token_complete: false,
        }
    }

    pub fn get_allowed_parsers<'a>(&self) -> &'a [Parser] {
        self.stack.get().get_parsers()
    }

    fn whitespace_allowed(&self) -> bool { 
        self.stack.get().whitespace_allowed()
    }

    pub fn push_state (
        &mut self,
        state: State,
    ) {
        self.stack.push(state);
    }

    fn get_current_state_mut(&mut self) -> &mut State {
        self.stack.get_mut()
    }

    pub fn offset(&self) -> usize { self.offset }
}

use itertools::{
    peek_nth,
    PeekNth,
};
type TokenStream<'a> = PeekNth<SyntaxTokenStream<'a>>;

/*
Every parser needs to follow this pattern.

    1: What is it?
    2: Is it allowed?
    3: How does it modify the state.
    */

type Parser = fn(stream: &SyntaxToken<'_>, &mut Ctx) -> bool;
pub type ParseResult2<T> = Result<T, Error>;
pub struct Error;

fn parse(src: &String) -> ParseResult2<()> {
    let mut token_stream = peek_nth(SyntaxTokenStream::new(&src));
    let mut ctx = Ctx::new();
    loop {
        if ctx.whitespace_allowed() {
            skip_whitespace(&mut token_stream);
        }
        if let Some(token) = token_stream.peek() {
            for parser in ctx.get_allowed_parsers() {
                if parser(&token, &mut ctx) {
                    token_stream.next();
                    break;
                }
            }

            // TODO: Report UnexpectedToken here.

        } else {
            break;
        }
    }
    todo!()
}

fn skip_whitespace(stream: &mut TokenStream) {
    loop {
        if let Some(t) = stream.peek() {
            match t.ty {
                ST::Whitespace | ST::Control(..) => {
                    stream.next();
                },
                _ => {},
            }
        }
        return;
    }
}

fn Visibility_parser(
    stream: &mut TokenStream,
    ctx: &mut Ctx,
) -> ParseResult2<()> {
    if let Some(t) = stream.peek_nth(ctx.offset()) {
        match t.ty {
            ST::Keyword(KT::Pub) => {},
            _ => { /* Token wasnt 'pub' token */ },
        }
    }
    todo!()
}

type ST = SyntaxTokenType;
type KT = KeywordType;

fn parse_pub_keyword (
    token: &SyntaxToken,
    ctx: &mut Ctx,
) -> bool {
    match token.ty {
        ST::Keyword(KT::Pub) => {
            ctx.set_current_node();
            true
        },
        _ => {
            // Report Error
            false
        },
    }
}

fn parse_fn_token(
    token: &SyntaxToken,
    ctx: &mut Ctx,
) -> bool {
    match token.ty {
        ST::Keyword(KT::Fn) => {
            match ctx.get_current_state_mut() {
                State::ModuleRoot => {
                    ctx.push_state(State::FunctionItem {
                        vis: Visibility::Private,
                        ident: String::new(),
                        params: vec![],
                        ret: String::new(),
                    });
                    return true;
                },
                State::ModuleItem { kind, .. } => {
                    debug_assert!(*kind == MIKind::Unknown);
                    *kind = MIKind::Function;
                    return true;
                }
                _ => {}
            }
        },
        _ => {},
    }
    false
}

#[derive(Debug)]
pub enum Visibility {
    Public,
    Private,
}

impl Default for Visibility {
    fn default() -> Self { Visibility::Private }
}

#[derive(Debug)]
pub enum State {
    ModuleRoot,
    FunctionItem {
        vis: Visibility,
        ident: String,
        params: Vec<()>,
        ret: String,
    }
}

impl State {
    pub fn whitespace_allowed(&self) -> bool {
        match self {
            State::ModuleRoot |
            State::FunctionItem { .. } => true,
        }
    }

    pub fn get_parsers<'a>(&self) -> &'a [Parser] {
        match self {
            State::ModuleRoot => &[
                
            ],
            _ => &[]
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleItemKind {
    Unknown,
    Function,
}

#[rustfmt::skip]
bitflags! {
    struct ModuleScopeItemFlags: u16 {
        const PUBLIC    = (1 << 0);
        const FUNCTION  = (1 << 1);
        const STATIC    = (1 << 2);
        const CONST     = (1 << 3);
        const ATTRIBUTE = (1 << 4);
    }
}
pub type MIKind  = ModuleItemKind;
pub type MSFlags = ModuleScopeItemFlags;

bitflags! {
    struct StackDataFlags: u32 {
        const VISIBILITY = (1 << 0);
        const WHITESPACE_ALLOWED = (1 << 1);
    }
}

impl Default for StackDataFlags {
    fn default() -> Self { StackDataFlags::WHITESPACE_ALLOWED }
}

pub struct StateStack {
    stack: Vec<State>,
}

impl StateStack {
    pub fn new() -> Self {
        Self {
            stack: vec![State::ModuleRoot],
        }
    }

    pub fn push(
        &mut self,
        state: State,
    ) {
        self.stack.push(state);
    }

    pub fn pop(&mut self) { self.stack.pop(); }

    pub fn get(&self) -> &State { self.stack.last().expect("Stack should always have a root") }

    pub fn get_mut(&mut self) -> &mut State {
        self.stack
            .last_mut()
            .expect("Stack should always have a root")
    }
}
