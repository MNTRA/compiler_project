use std::str::Chars;

use macros::create_raw_tokenizer;

use crate::{
    cursor::Cursor,
    span::Span,
    token::{
        ControlType,
        PunctuationType,
        RawToken,
        RawTokenData,
        RawTokenType,
    },
};

// Raw Tokens =================================================================

pub struct RawTokenStream<'a> {
    source: &'a str,
    cursor: Cursor<Chars<'a>>,
}

impl<'a> RawTokenStream<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: Cursor::new(source.chars()),
        }
    }
}

impl<'a> Iterator for RawTokenStream<'a> {
    type Item = RawToken<'a>;

    #[rustfmt::skip]
    fn next(&mut self) -> Option<RawToken<'a>> {
        if let Ok(c) = self.cursor.peek(0) {
            let token = if is_whitespace(c) {
                WSTokenizer::new(&self.source, &mut self.cursor).tokenize()
            } else if is_control(c) {
                CCTokenizer::new(&self.source, &mut self.cursor).tokenize()
            } else if is_punctuation(c) {
                PuncTokenizer::new(&self.source, &mut self.cursor).tokenize()
            } else if is_alphabetic(c) {
                WordTokenizer::new(&self.source, &mut self.cursor).tokenize()
            } else if is_numeric(c) {
                NumTokenizer::new(&self.source, &mut self.cursor).tokenize()
            } else {
                UKnTokenizer::new(&self.source, &mut self.cursor).tokenize()
            };
            return Some(token);
        } else {
            return None;
        }
    }
}

create_raw_tokenizer! {
    WordTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        self.consume();
        loop {
            if let Ok(c) = self.cursor.peek(0) {
                if c.is_alphanumeric() {
                    self.consume();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return self.create_token(RawTokenType::Word);
    }
}

create_raw_tokenizer! {
    NumTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        self.consume();
        loop {
            if let Ok(c) = self.cursor.peek(0) {
                match c {
                    '0'..='9' => {
                        self.consume();
                    },
                    _ => {
                        break;
                    },
                }
            } else {
                break;
            }
        }
        return self.create_token(RawTokenType::Number);
    }
}

macro_rules! punctuation_token {
    ($SELF:ident, $IDENT:ident) => {{
        $SELF.consume();
        return $SELF.create_token(RawTokenType::Punctuation(PunctuationType::$IDENT));
    }};
}

create_raw_tokenizer! {
    PuncTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        match self.peek(0) {
            '+' => { punctuation_token!(self, Plus         )},
            '-' => { punctuation_token!(self, Hyphen       )},
            '_' => { punctuation_token!(self, UnderScore   )},
            '/' => { punctuation_token!(self, Slash        )},
            '\\'=> { punctuation_token!(self, BackSlash    )},
            '*' => { punctuation_token!(self, Asterisk     )},
            ')' => { punctuation_token!(self, RParen       )},
            '(' => { punctuation_token!(self, LParen       )},
            '>' => { punctuation_token!(self, RAngleBracket)},
            '<' => { punctuation_token!(self, LAngleBracket)},
            '}' => { punctuation_token!(self, RBrace       )},
            '{' => { punctuation_token!(self, LBrace       )},
            ']' => { punctuation_token!(self, RBracket     )},
            '[' => { punctuation_token!(self, LBracket     )},
            '=' => { punctuation_token!(self, Equals       )},
            '|' => { punctuation_token!(self, Pipe         )},
            '?' => { punctuation_token!(self, QuestionMark )},
            '!' => { punctuation_token!(self, Exclamation  )},
            '.' => { punctuation_token!(self, Period       )},
            '&' => { punctuation_token!(self, Ampersand    )},
            ':' => { punctuation_token!(self, Colon        )},
            ';' => { punctuation_token!(self, SemiColon    )},
            '"' => { punctuation_token!(self, Quote        )},
            '\''=> { punctuation_token!(self, SingleQuote  )},
            '%' => { punctuation_token!(self, Percent      )},
            '#' => { punctuation_token!(self, Hash         )},
            '@' => { punctuation_token!(self, At           )},
            '$' => { punctuation_token!(self, Dollar       )},
            '~' => { punctuation_token!(self, Tilde        )},
            '`' => { punctuation_token!(self, BackQuote    )},
            ',' => { punctuation_token!(self, Comma        )},
            _ => unreachable!(),
        }
    }
}

create_raw_tokenizer! {
    CCTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        match self.peek(0) {
            '\r' => {
                self.consume();
                match self.peek(0) {
                    '\n' => {
                        self.consume();
                        return self.create_token(
                            RawTokenType::Control(ControlType::NewLine)
                        );
                    },
                    _ => {
                        return self.create_token(
                            RawTokenType::Control(ControlType::NewLine)
                        );
                    }
                }
            }
            '\n' => {
                    self.consume();
                    return self.create_token(
                        RawTokenType::Control(ControlType::NewLine)
                    );
                },
            '\0' => {
                    self.consume();
                    return self.create_token(
                        RawTokenType::Control(ControlType::Null)
                    );
            }
            '\t' => {
                    self.consume();
                    return self.create_token(
                        RawTokenType::Control(ControlType::Tab)
                    );
                }
            c => { println!(" ur: {}", c);   unreachable!()},
        }
    }
}

create_raw_tokenizer! {
    WSTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        self.consume();
        loop {
            if let Ok(c) = self.cursor.peek(0) {
                match c {
                    ' ' => {
                        self.consume();
                    },
                    _ => {
                        break;
                    },
                }
            } else {
                break;
            }
        }
        return self.create_token(RawTokenType::Whitespace);
    }
}

create_raw_tokenizer! {
    UKnTokenizer,
    fn tokenize(&mut self) -> RawToken<'b> {
        self.consume();
        self.create_token(RawTokenType::Unknown)
    }
}

fn is_control(c: &char) -> bool {
    match c {
        '\r' | '\n' | '\t' | '\0' => true,
        _ => false,
    }
}

fn is_whitespace(c: &char) -> bool {
    match c {
        ' ' => true,
        _ => false,
    }
}

fn is_alphabetic(c: &char) -> bool { c.is_alphabetic() }

fn is_punctuation(c: &char) -> bool { c.is_ascii_punctuation() }

fn is_numeric(c: &char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

struct RawTokenizer<'a, 'b> {
    source: &'b str,
    length: usize,
    offset: usize,
    cursor: &'a mut Cursor<Chars<'b>>,
}

impl<'a, 'b> RawTokenizer<'a, 'b> {
    fn create_token(
        &mut self,
        ty: RawTokenType,
    ) -> RawToken<'b> {
        let offset = self.offset;
        let length = self.length - 1;
        let end_offset = offset + length;

        let mut span = Span::default();
        span.set_start(offset);
        span.set_end(end_offset);

        RawToken {
            ty,
            data: RawTokenData {
                span,
                src: &self.source[offset..=end_offset],
            },
        }
    }

    fn peek(
        &mut self,
        offset: usize,
    ) -> &char {
        self.cursor.peek(offset).unwrap()
    }

    fn consume(&mut self) {
        self.length += 1;
        let _ = self.cursor.next();
    }
}
