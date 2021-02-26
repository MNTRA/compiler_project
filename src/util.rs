/// Advances an iterator by calling `next()` on it n number of times
///
/// If n == 0 the function does nothing
pub fn advance_iter_to<'a, T>(
    iter: &'a mut dyn Iterator<Item = T>,
    n: usize,
) {
    if n > 0 {
        for _ in 0..n {
            iter.next();
        }
    }
}

/// Finds the next occournce of val in the interator starting where the
/// interator currently is
///
/// Returns false if the end of the iterator is reached before a value is found
pub fn iterator_forward_to_next<'a, T>(
    iter: &'a mut dyn Iterator<Item = T>,
    val: T,
) -> bool
where
    T: Eq,
{
    let mut peekable = iter.peekable();
    let mut count = 0;
    loop {
        if let Some(it) = peekable.peek() {
            if *it == val {
                break;
            } else {
                count += 1;
                peekable.next();
            }
        } else {
            return false;
        }
    }
    advance_iter_to(iter, count);
    true
}

pub mod stack {
    pub struct Stack<T> {
        stack: Vec<T>,
    }

    impl<T> Stack<T> {
        pub fn new() -> Self {
            Self {
                stack: Vec::new(),
            }
        }

        pub fn push(
            &mut self,
            val: T,
        ) {
            self.stack.push(val)
        }

        pub fn pop(&mut self) -> Option<T> {
            if !self.is_empty() {
                let last = self.stack.len() - 1;
                Some(self.stack.remove(last))
            } else {
                None
            }
        }

        pub fn get_top(&self) -> Option<&T> {
            if !self.is_empty() {
                let last = self.stack.len() - 1;
                self.stack.get(last)
            } else {
                None
            }
        }

        pub fn get_top_mut(&mut self) -> Option<&mut T> {
            if !self.is_empty() {
                let last = self.stack.len() - 1;
                self.stack.get_mut(last)
            } else {
                None
            }
        }

        pub fn is_empty(&self) -> bool { self.stack.is_empty() }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: usize,
    pub offset: usize,
    pub filename: Option<&'static str>,
}

impl SourceLocation {
    pub fn new(
        line: usize,
        offset: usize,
        filename: impl Into<Option<&'static str>>,
    ) -> Self {
        SourceLocation {
            line,
            offset,
            filename: filename.into(),
        }
    }
}

/// Keeping the panic to the scope of 1 function keeps the asm clean apparently
pub fn panic_here<'a>(text: impl Into<Option<&'a str>>) -> ! {
    if let Some(s) = text.into() {
        let s: &'a str = s; // <- for type inference
        panic!("{}", s);
    } else {
        panic!();
    }
}
