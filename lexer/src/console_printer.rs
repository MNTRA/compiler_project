use std::{
    borrow::Cow,
    sync::Mutex,
};

// external
use console::{
    self,
    set_colors_enabled,
    style,
    truncate_str,
    Term,
};
use lazy_static::lazy_static;

use crate::{
    token::{
        ControlType,
        LiteralType,
        SyntaxToken,
        TokenKind,
    },
    KeywordType,
};

lazy_static! {
    pub static ref CONSOLE_PRINTER: Mutex<ConsolePrinter> = {
        set_colors_enabled(true);
        let this = ConsolePrinter::new();
        this.term
            .clear_screen()
            .and_then(|_| this.print_titlebar())
            .unwrap();
        Mutex::new(this)
    };
}

type IoResult = std::io::Result<()>;

/// Console Printer
pub struct ConsolePrinter {
    width: usize,
    should_print_line_number: bool,
    term: Term,
}

impl ConsolePrinter {
    fn new() -> Self {
        Self {
            width: 70,
            should_print_line_number: true,
            term: Term::stdout(),
        }
    }

    // Private ---------------------------------------------------------------------

    //const BLACK:  u8 = 16;
    //const WHITE:  u8 = 15;
    const PURPLE: u8 = 177;
    const RED: u8 = 9;
    const YELLOW: u8 = 11;
    const BLUE: u8 = 33;
    const LIGHT_BLUE: u8 = 14;
    const ORANGE: u8 = 172;

    pub fn print_titlebar(&self) -> IoResult {
        let title_str = " Georges Complier ";
        let version_str = ": v0.0.1 ";
        let padding = self.width.saturating_sub(title_str.len());
        let text = format!(
            "{}{}",
            style(title_str).green().on_blue().bold(),
            style(self.pad_str(version_str, padding)).green().on_blue()
        );
        self.term.write_line(&self.trunc_str(&text))?;
        Ok(())
    }

    fn create_source_code_block_bounds_str(
        &self,
        text: &str,
    ) -> String {
        let text = self.pad_str(text, self.width);
        format!("{}", style(text).black().on_white())
    }

    fn pad_str<'a>(
        &self,
        text: &'a str,
        max_width: usize,
    ) -> Cow<'a, str> {
        if text.len() < max_width {
            let length_to_add = max_width - text.len();
            let mut out = String::with_capacity(text.len() + length_to_add);
            out.push_str(text);
            for _ in 0..length_to_add {
                out.push(' ');
            }
            Cow::Owned(out)
        } else {
            Cow::Borrowed(text)
        }
    }

    fn print_identifer_token(
        &self,
        src: &str,
    ) -> IoResult {
        self.term.write_str(&format!(
            "{}",
            style(src).color256(Self::LIGHT_BLUE).on_black()
        ))
    }
    fn print_print_punctuation_token(
        &self,
        src: &str,
    ) -> IoResult {
        self.term.write_str(&format!("{}", style(src).white()))
    }
    fn print_literal_token(
        &self,
        src: &str,
        kind: LiteralType,
    ) -> IoResult {
        match kind {
            LiteralType::String | LiteralType::Char => self
                .term
                .write_str(&format!("{}", style(src).color256(Self::ORANGE).on_black())),
            LiteralType::Integer | LiteralType::Float => self
                .term
                .write_str(&format!("{}", style(src).color256(Self::YELLOW).on_black())),
        }
    }
    fn print_keyword_token(
        &self,
        src: &str,
        kind: KeywordType,
    ) -> IoResult {
        match kind {
            KeywordType::Fn => self
                .term
                .write_str(&format!("{}", style(src).color256(Self::PURPLE).on_black())),
            _ => self
                .term
                .write_str(&format!("{}", style(src).color256(Self::BLUE).on_black())),
        }
    }
    fn print_whitespace_token(
        &self,
        src: &str,
    ) -> IoResult {
        self.term.write_str(src)
    }
    fn print_control_token(
        &mut self,
        kind: ControlType,
    ) -> IoResult {
        match kind {
            ControlType::NewLine => {
                self.term.write_line(" ")?;
                self.should_print_line_number = true;
                Ok(())
            },
            ControlType::Tab => self.term.write_str("   "),
            ControlType::Null => self.term.write_str(&format!(
                "{}",
                style("NULL").black().color256(Self::PURPLE).bold()
            )),
        }
    }
    fn print_unknown_token(
        &self,
        src: &str,
    ) -> IoResult {
        self.term.write_str(&format!(
            "{}",
            style(src).black().on_color256(Self::RED).bold()
        ))
    }

    fn print_syntax_token_impl_(
        &mut self,
        src: &String,
        token: &SyntaxToken,
    ) {
        let src = token.get_str(src);
        let result = match token.ty {
            TokenKind::Punctuation(_) => self.print_print_punctuation_token(src),
            TokenKind::Identifier => self.print_identifer_token(src),
            TokenKind::Literal(l) => self.print_literal_token(src, l),
            TokenKind::Keyword(k) => self.print_keyword_token(src, k),
            TokenKind::Whitespace => self.print_whitespace_token(src),
            TokenKind::Control(c) => self.print_control_token(c),
            TokenKind::Unknown => self.print_unknown_token(src),
            TokenKind::Null => unreachable!(),
        };
        match result {
            Ok(_) => {},
            Err(e) => panic!("{}", e),
        }
    }

    pub unsafe fn print_syntax_token(
        &mut self,
        src: &String,
        token: Option<SyntaxToken>,
    ) {
        static PRINT_TOP: std::sync::Once = std::sync::Once::new();
        PRINT_TOP.call_once(|| {
            let top = self.create_source_code_block_bounds_str(" SOURCE");
            self.term.write_line(&self.trunc_str(&top)).unwrap();
        });
        static mut TEMP_LINE: usize = 1;
        if token.is_some() {
            // Safety: safe as long as only 1 thread calls this function at a time
            if self.should_print_line_number {
                self.print_line_number(TEMP_LINE).unwrap();
                TEMP_LINE += 1;
                self.should_print_line_number = false;
            }
            self.print_syntax_token_impl_(src, &token.unwrap());
        } else {
            // Safety: safe as long as only 1 thread calls this function at a time
            if self.should_print_line_number {
                self.print_line_number(TEMP_LINE).unwrap();
                TEMP_LINE += 1;
                self.should_print_line_number = false;
            }
            self.term.write_line("").unwrap();
            let bottom = self.create_source_code_block_bounds_str(" ");
            self.term.write_line(&self.trunc_str(&bottom)).unwrap();
        }
    }

    fn print_line_number(
        &mut self,
        line: usize,
    ) -> IoResult {
        let char_buffer: &mut [char; 4] = &mut [' '; 4];
        for (i, c) in line.to_string().chars().enumerate() {
            match i {
                0 => char_buffer[3] = c,
                1 => {
                    char_buffer.rotate_left(1);
                    char_buffer[3] = c;
                },
                2 => {
                    char_buffer.rotate_left(2);
                    char_buffer[3] = c;
                },
                4 => {
                    char_buffer.rotate_left(3);
                    char_buffer[3] = c;
                },
                _ => {
                    panic!("files over 9999 lines are not yet supported.")
                },
            }
        }
        let out = format!(
            "{}{}{}{}| ",
            char_buffer[0], char_buffer[1], char_buffer[2], char_buffer[3],
        );
        self.term.write_str(&out)
    }

    fn trunc_str<'a>(
        &self,
        text: &'a str,
    ) -> Cow<'a, str> {
        truncate_str(text, self.width, "")
    }
}
