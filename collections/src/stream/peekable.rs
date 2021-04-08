// std
use std::collections::VecDeque;

// External
use thiserror::Error;

const DEBUG: bool = cfg!(debug_assertions);

// Dynamic ======================================================================

pub type PeekableResult<T> = std::result::Result<T, DynamicError>;

/// Similar in functionality to `std::iter::Peekable` but allows
/// users to peek any number of items into the stream.
///
/// Internally `Dynamic` holds an unbounded ring buffer that stores items seen
/// by the underlying iterator, returning items from the buffer when consumed.
pub struct Dynamic<T: Iterator> {
    stream: T,
    buffer: VecDeque<T::Item>,
}

impl<T: Iterator> Dynamic<T> {
    pub fn new(stream: T) -> Self {
        Self {
            stream,
            buffer: VecDeque::with_capacity(4),
        }
    }

    /// Analagous to `Iterator::next()` but returns a
    /// `PeekableResult<T::Item>` instead
    pub fn consume(&mut self) -> PeekableResult<T::Item> {
        match Self::get_next_item(self) {
            Some(item) => Ok(item),
            None => Err(DynamicError::EndOfStream),
        }
    }

    /// Returns a reference to the item `offset` number of items ahead of
    /// the iterator
    pub fn peek(
        &mut self,
        offset: usize,
    ) -> PeekableResult<&T::Item> {
        // TODO (George): This could be implimented better?
        let mut count = 0;
        while let None = self.buffer.get(offset) {
            if let Some(item) = self.stream.next() {
                count += 1;
                self.buffer.push_back(item);
            } else {
                if count == 0 {
                    return Err(DynamicError::EndOfStream);
                } else {
                    return Err(DynamicError::OffsetOutOfRange(count));
                }
            }
        }
        return Ok(self.buffer.get(offset).unwrap());
    }

    pub fn num_buffered_items(&self) -> usize { self.buffer.len() }

    pub fn clear_buffer(&mut self) { self.buffer.clear(); }

    // Private -------------------------------------------------------------

    /// Analagous to `Iterator::next()`
    ///  
    /// Gets the next item from the stream by first returning elements from
    /// the buffer and only advancing the underlying iterator if the
    /// buffer is empty
    fn get_next_item(&mut self) -> Option<T::Item> {
        if self.buffer.is_empty() {
            T::next(&mut self.stream)
        } else {
            if DEBUG {
                match self.buffer.pop_front() {
                    Some(item) => Some(item),
                    None => panic!("Buffer was empty"),
                }
            } else {
                self.buffer.pop_front()
            }
        }
    }
}

impl<T: Iterator> Iterator for Dynamic<T> {
    type Item = T::Item;
    fn next(&mut self) -> Option<T::Item> {
        match self.consume() {
            Ok(item) => Some(item),
            Err(_) => None,
        }
    }
}

#[derive(Error, Debug)]
pub enum DynamicError {
    /// Reached the end of the stream
    #[error("End Of Stream")]
    EndOfStream,
    /// Peeked into the end of the stream before
    /// `BufferedStream::peek(offset: usize)` could be called
    /// `offset` number of times.
    ///
    /// usize: number of items peeked before erroring
    #[error("Reached end of stream before count")]
    OffsetOutOfRange(usize),
}

// Tests ===================================================================

#[cfg(test)]
mod test {
    use std::str::Chars;

    use super::*;
    #[test]
    fn buf_stream_consume() {
        let (mut stream, s) = setup("Consume Test String");
        let mut string = String::new();
        while let Ok(c) = stream.consume() {
            string.push(c);
        }
        assert_eq!(string, s);
    }
    /// Does peeking at 0 before consuming any items work
    #[test]
    fn buf_stream_peek_t1() {
        let (mut stream, s) = setup("Peek0 Test String");
        assert_eq!(stream.peek(0).unwrap(), &'P');
    }

    /// Does peeking at an offset before consuming any items work
    #[test]
    fn buf_stream_peek_t2() {
        let (mut stream, s) = setup("Peek0 Test String");
        assert_eq!(stream.peek(6).unwrap(), &'T');
        assert_eq!(stream.peek(8).unwrap(), &'s');
    }

    #[test]
    fn buf_stream_consume_peek() {
        let (mut stream, s) = setup("Peek0 Test String");
        stream.consume();
        stream.consume();
        stream.consume();
        stream.consume();
        assert_eq!(stream.peek(0).unwrap(), &'0');
    }

    #[test]
    fn buf_stream_peek_consume() {
        let (mut stream, s) = setup("Peek0 Test String");
        stream.peek(0);
        stream.peek(0);
        stream.peek(0);
        assert_eq!(stream.peek(0).unwrap(), &'P');
        assert_eq!(stream.consume().unwrap(), 'P');
    }

    fn setup<'a>(s: &'a str) -> (Dynamic<std::str::Chars<'a>>, &'a str) {
        let stream = Dynamic::new(s.chars());
        (stream, s)
    }
}
