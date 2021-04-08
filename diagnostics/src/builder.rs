use crate::{Level, span::Span};

#[derive(Default)]
pub struct DiagnosticBuilder {
    pub(crate) level: Option<Level>,
    pub(crate) title: String,
    pub(crate) label: String,
    pub(crate) hint: String,
    pub(crate) marked_span: Option<Span>,
}   

impl DiagnosticBuilder {
    pub fn with_level (mut self, level: Level) -> Self {
        self.level = Some(level);
        self
    }

    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.title = title.into();
        self
    }

    pub fn with_label (mut self, label: impl Into<String>) -> Self {
        self.label = label.into();
        self
    }

    pub fn set_hint (&mut self, hint: impl Into<String>) -> &mut Self {
        self.hint = hint.into();
        self
    }

    pub fn with_hint (mut self, hint: impl Into<String>) -> Self {
        self.hint = hint.into();
        self
    }

    pub fn set_marked_span (&mut self, span: Span) -> &mut Self {
        self.marked_span = Some(span);
        self
    }
    pub fn get_marked_span (&mut self) -> Option<Span> {
        self.marked_span
    }
    pub fn with_marked_span (mut self, span: Span) -> Self {
        self.marked_span = Some(span);
        self
    }
    
    pub fn report(self) {
        crate::report_diagnostic(self);
    }
}

