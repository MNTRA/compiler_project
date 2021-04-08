
pub const NULL_SPAN: Span = Span {
    start: 0,
    end: 0,
    line: 0,
};

/// A `Span` repesents a continuous range of source code.
///
/// `Span` should be used in error reporting and printing.
#[derive(Default, Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
    line: usize,
}

impl Span {
    pub fn new(
        start: usize,
        end: usize,
        line: usize,
    ) -> Self {
        Self {
            start,
            end,
            line,
        }
    }

    pub fn start(&self) -> usize { self.start }
    pub fn end(&self) -> usize { self.end }

    pub fn set_start(
        &mut self,
        start: usize,
    ) {
        self.start = start;
    }
    pub fn set_end(
        &mut self,
        end: usize,
    ) {
        self.end = end;
    }

    pub fn add_to_end(
        &mut self,
        amt: usize,
    ) {
        self.end += amt;
    }

    pub fn add_to_start(
        &mut self,
        amt: usize,
    ) {
        self.start += amt;
    }

    pub fn offset_by(
        &mut self,
        amt: usize,
    ) {
        self.start += amt;
        self.end += amt;
    }

    pub fn set_line(&mut self, line: usize) {
        self.line = line;
    }

    pub fn line(&self) -> usize { self.line }

    pub fn len(&self) -> usize { self.end - self.start + 1 }
}

impl std::ops::AddAssign<Span> for Span {
    fn add_assign(
        &mut self,
        other: Self,
    ) {
        *self = Self {
            start: self.start,
            end: self.end + other.len(), //other.end - other.start,
            line: self.line,
        }
    }
}