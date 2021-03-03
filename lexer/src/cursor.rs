/// External
use collections::stream::{ Dynamic, DynamicError };
use thiserror::Error;

type CursorResult<T> = Result<T, CursorError>;

pub struct Cursor<I: Iterator> {
    offset: usize,
    stream: Dynamic<I>
}

impl<I: Iterator> Cursor<I> {
    pub fn new(source: I) -> Self {
        Self {
            offset: 0,
            stream: Dynamic::new(source)
        }
    }

    pub fn peek (&mut self, offset: usize) -> CursorResult<&I::Item> {
        self.stream.peek(offset).map_err(|_|{
            CursorError::EndOfLine
        })
    }
    
    pub fn next (&mut self) -> CursorResult<I::Item> {
        let out = self.stream.consume().map_err(|_|{
            CursorError::EndOfLine
        });
        self.offset += 1;
        out
    }

    pub fn clear_buffer(&mut self) {
        let len = self.stream.num_buffered_items() - 1;
        for _ in 0..len {
            self.stream.next();
        }
        self.stream.clear_buffer();
    }

    pub fn advance(&mut self, mut amt: usize) {
        while amt - 1  > 0 {
            let _ = self.next();
            amt -= 1;
        }
    }

    pub fn offset(&self) -> usize { self.offset }
}

#[derive(Error, Debug)]
pub enum CursorError {
    #[error("End Of Stream")]
    EndOfLine,
    #[error(transparent)]
    PeekableError(DynamicError)
}
