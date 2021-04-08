#![feature(format_args_capture)]

pub mod span;

mod builder;

use console::{Style, Term, set_colors_enabled};
use crossbeam::queue::SegQueue;
use lazy_static::lazy_static;

pub use builder::DiagnosticBuilder;
use span::Span;

#[rustfmt::skip]
lazy_static! {
    static ref HINT_CHANNEL:    DiagnosticQueue = DiagnosticQueue::new();
    static ref WARNING_CHANNEL: DiagnosticQueue = DiagnosticQueue::new();
    static ref ERROR_CHANNEL:   DiagnosticQueue = DiagnosticQueue::new();
}
pub type DiagnosticQueue = SegQueue<Diagnostic>;

pub struct Diagnostic {
    level: Level,
    title: String,
    label: String,
    hint: String,
    marked_span: Option<Span>,
}

impl From<DiagnosticBuilder> for Diagnostic {
    fn from(builder: DiagnosticBuilder) -> Self {
        Self {
            level: builder.level.expect("Level cant be None"),
            title: builder.title,
            label: builder.label,
            hint: builder.hint,
            marked_span: builder.marked_span,
        }
    }
}

pub enum Level {
    Hint,
    Warning,
    Error,
}

impl std::fmt::Display for Level {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Level::Hint => f.write_str("hint"),
            Level::Warning => f.write_str("warning"),
            Level::Error => f.write_str("error"),
        }
    }
}

pub fn report_diagnostic(diagnostic: impl Into<Diagnostic>) {
    let diagnostic = diagnostic.into();
    match diagnostic.level {
        Level::Hint => HINT_CHANNEL.push(diagnostic),
        Level::Warning => WARNING_CHANNEL.push(diagnostic),
        Level::Error => ERROR_CHANNEL.push(diagnostic),
    }
}

pub fn print_diagnostics(src: &String) {
    set_colors_enabled(true);
    let term = Term::stderr();
    let seperator = format!("{:->60}", "\n");

    for diagnostic in iter_diagnostics() {
        let out_str = match diagnostic.level {
            Level::Error => format_error(diagnostic, src),
            Level::Hint => todo!(),
            Level::Warning => todo!(),
        };

        let _ = term.write_line(&format!("{seperator}{out_str}"));
    }

    let _ = term.write_line(&seperator);
}

fn iter_diagnostics() -> impl Iterator<Item = Diagnostic> {
    let errors = std::iter::from_fn(|| ERROR_CHANNEL.pop());
    let warnings = std::iter::from_fn(|| WARNING_CHANNEL.pop());
    errors.chain(warnings)
}

const TAB: &str = "  ";

pub fn format_error(
    mut diagnostic: Diagnostic,
    src: &str,
) -> String {
    let red = Style::new().red().on_black();
    let white = Style::new().white().on_black();
    let cyan = Style::new().cyan().on_black();

    let level = red.apply_to(format!("{}(0001)", diagnostic.level));
    let title = white.apply_to(std::mem::take(&mut diagnostic.title));
    let label = white.apply_to(format!("{TAB}>>>{TAB}{}", diagnostic.label));

    let hint = if !diagnostic.hint.is_empty() {
        let prefix = cyan.apply_to("Hint");
        white
            .apply_to(format!("{TAB}{prefix}:{TAB}{}", diagnostic.hint))
            .to_string()
    } else {
        String::new()
    };
    if let Some(span) = diagnostic.marked_span {
        let src_block = format_code_region(span, src);
        format!("{level}: {title}\n{src_block}\n{label}\n{hint}")
    } else {
        format!("{level}: {title}\n{label}\n{hint}")
    }
}

pub fn format_code_region(span: Span, src: &str) -> String {
    let code_region = SpanMarker::new(span, src);

    let yellow = Style::new().color256(11).on_black();
    let red = Style::new().red().on_black();

    let cr_prefix = yellow.apply_to(
        format!(
            "{2:>1$}\n{3:>0$} | ",
            TAB.len(),
            TAB.len() + 2,
            "|",
            span.line() + 1,
        )
    );
    let code_line = format!(
        "{}{}",
        cr_prefix,
        code_region.src,
    );

    let ml_prefix = yellow.apply_to(format!(
        "{1:>0$} ",
        TAB.len() + 2,
        "|",
    ));
    let marker_line = format!(
        "{}{}",
        ml_prefix,
        red.apply_to(code_region.marker),
    );

    format!("{code_line}\n{marker_line}\n")
}

struct SpanMarker<'a> {
    src: &'a str,
    marker: String,
}

impl<'a> SpanMarker<'a> {
    pub fn new(
        span: Span,
        src: &'a str,
    ) -> Self {
        let (line_start, line_end) = find_line_containing_span(span, src);
        let marker = (0..span.len()).map(|_|{"^"}).collect::<String>();
        let (src_region, removed_chars) = get_src_line(src, line_start, line_end);
        let marker_offset = span.start() - line_start - removed_chars;
        let offset_str = (0..marker_offset).map(|_|{" "}).collect::<String>();
        Self {
            src: src_region,
            // If the marker offset is wrong look here first
            marker: format!("{offset_str}{marker}"),
        }
    }
}

fn find_line_containing_span(
    span: Span,
    src: &str,
) -> (usize, Option<usize>) {
    const NEWLINE_CHARS: &[char] = &['\n', '\r'];
    let start_pos = src
        .get(..=span.start())
        .unwrap()
        .rfind(NEWLINE_CHARS)
        .unwrap_or(0);

    let end_pos = src.get(span.start()..).unwrap().find(NEWLINE_CHARS);
    (start_pos, end_pos)
}

fn get_src_line(src: &str, mut start: usize, end: Option<usize>) -> (&str, usize) {
    let temp = &src[start..];

    let mut removed_chars = 0;
    for (i, _) in temp.char_indices().take_while(|c| c.1.is_whitespace() ) {
        removed_chars = i + 1;
    }
    
    start = start + removed_chars;
    let out_str = &src[start..];
    
    if let Some(end) = end {
        (&out_str[..end].trim_end(), removed_chars)
    } else {
        (&out_str.trim_end(), removed_chars)
    }
}