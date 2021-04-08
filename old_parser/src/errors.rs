use diagnostics::{CompileError, ErrorCode, Info};
use lexer::Span;

pub struct InvalidSyntax {
    err_span: Span,
    ctx_span: Span,
    message: String,
}

impl InvalidSyntax {
    pub fn new(
        err_span: Span,
        ctx_span: Span,
        message: impl Into<String>,
    ) -> Self {
        Self {
            err_span,
            ctx_span,
            message: message.into(),
        }
    }
}

impl CompileError for InvalidSyntax {
    fn name(&self) -> &'static str { "Invalid Syntax" }
    fn code(&self) -> ErrorCode { ErrorCode::new('P', 1) }
    fn info(&self) -> Info { 
        Info {
            ctx_span: self.ctx_span,
            src_span: self.err_span,
            message: self.message.clone(),
        }
    }
}
