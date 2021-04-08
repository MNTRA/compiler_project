// // external
// use console::{
//     self,
//     set_colors_enabled,
//     style,
//     truncate_str,
//     Term,
// };

use console::{
    set_colors_enabled,
    Style,
    Term,
};
use lexer::{
    ControlType,
    Span,
    SyntaxToken,
    SyntaxTokenStream,
    SyntaxTokenType,
};

use crate::ParseError;

pub struct Diagnostics {
    errors: Vec<Reporter>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            errors: vec![],
        }
    }

    pub fn report(
        &mut self,
        reporter: Reporter,
    ) {
        match reporter.kind.unwrap() {
            ReportKind::Error => {
                self.errors.push(reporter);
            },
            _ => todo!(),
        }
    }

    pub fn reporter(&mut self) -> Reporter {
        Reporter {
            parse_region: None,
            reported_region: None,
            kind: None,
            msg: String::new(),
            code: 0,
        }
    }

    pub fn print_errors(
        &mut self,
        src: &str,
    ) {
        set_colors_enabled(true);
        let term = Term::stdout();
        for error in &mut self.errors {
            Self::print_error_title(&term, error);
            Self::print_code_region(src, error);
        }
    }

    fn error_prefix(code: u32) -> String {
        let red_style = Style::new().bold().red().on_black();
        let cyan_style = Style::new().cyan().on_black();
        format!(
            "{}{:04}{}",
            red_style.apply_to("[Error: "),
            cyan_style.apply_to(&format!("E{}", code)),
            red_style.apply_to("]"),
        )
    }

    fn print_error_title(
        term: &Term,
        error: &Reporter,
    ) {
        let prefix = Self::error_prefix(error.code);
        let _ = term.write_line(&format!("{} {}", prefix, error.msg));
    }

    fn print_code_region(
        src: &str,
        error: &Reporter,
    ) {
        let (parse_line, parse_region) = error.parse_region.unwrap();
        let (_, report_region) = error.reported_region.unwrap();
        let local_start = report_region.start() - parse_region.start();
        let context_region = &src[parse_region.start()..=parse_region.end()];
        let marker = (0..report_region.len()).map(|_|{"^"}).collect::<String>();


        let yellow_style = Style::new().color256(Self::YELLOW).on_black();
        let red_style = Style::new().color256(Self::RED).on_black();

        let code_line_prefix = yellow_style.apply_to(
            format!(
                "{2:>1$}\n{3:>0$} | ",
                Self::INDENT,
                Self::INDENT + 2,
                "|",
                parse_line + 1,
            )
        );
        let code_line = format!(
            "{}{}",
            code_line_prefix,
            context_region,
        );

        let marker_line_prefix = yellow_style.apply_to(format!(
            "{1:>0$} ",
            Self::INDENT + 2,
            "|",
        ));
        let marker_line = format!(
            "{}{}",
            marker_line_prefix,
            red_style.apply_to(marker),
        );

        println!("\n{}\n{}\n", code_line, marker_line);
        
    }

    const INDENT: usize = 4;

    const PURPLE: u8 = 177;
    const RED: u8 = 9;
    const YELLOW: u8 = 11;
    const BLUE: u8 = 33;
    const LIGHT_BLUE: u8 = 14;
    const ORANGE: u8 = 172;
}

#[derive(Debug)]
pub struct Reporter {
    parse_region: Option<(usize, Span)>,
    reported_region: Option<(usize, Span)>,
    kind: Option<ReportKind>,
    msg: String,
    code: u32,
}

impl Reporter {
    pub fn append_parse_region_span(
        &mut self,
        line: usize,
        span: Span,
    ) {
        if let Some((_, region)) = &mut self.parse_region {
            region.set_end(span.end());
        } else {
            self.parse_region = Some((line, span));
        }
    }

    pub fn report_parse_error(
        &mut self,
        error: ParseError,
    ) {
        if let ParseError::UnexpectedToken {
            line,
            span,
            expected,
            found,
        } = error {
            self.append_parse_region_span(line, span);
            self.reported_region = Some((line, span));
            self.kind = Some(ReportKind::Error);
            self.msg = format!("Expected {}, found {}", expected, found,);
            self.code = error.code();
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportKind {
    Warning,
    Error,
    Hint,
}

/*

1 | { 1 + 2 }
  | ^^^^^^^^^


2 |     do_thing();
  |     ^^^^^^^^^^^

    Error: do_thing has error

*/

#[derive(Debug)]
pub struct DiagnosticsError<'a> {
    pub kind: ErrorKind,
    pub tokens: Vec<SyntaxToken<'a>>,
}

#[derive(Debug)]
pub enum ErrorKind {
    InvalidSyntax,
}
