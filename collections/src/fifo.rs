use std::mem::{
    ManuallyDrop,
    MaybeUninit,
};

use thiserror::Error;

/// A stack based const sized [fifo](https://en.wikipedia.org/wiki/FIFO_(computing_and_electronics))
pub struct StackFifo<T, const N: usize> {
    read: usize,
    len: usize,
    buffer: MaybeUninit<[ManuallyDrop<T>; N]>,
}

impl<T, const N: usize> StackFifo<T, N> {
    pub fn new() -> Self {
        Self {
            read: 0,
            len: 0,
            buffer: MaybeUninit::uninit(),
        }
    }

    pub fn push_back(
        &mut self,
        val: T,
    ) -> Result<(), StackFifoError> {
        if self.len() < self.capacity() {
            let write = self.write_pos();
            unsafe {
                self.access_buffer_mut()[write] = ManuallyDrop::new(val);
            }
            self.len += 1;
            Ok(())
        } else {
            Err(StackFifoError::Error)
        }
    }

    pub fn pop_front(&mut self) -> Result<T, StackFifoError>
    where
        T: Clone,
    {
        if self.len() > 0 {
            eprintln!("{}", self.read);
            // TODO (George): make this not require clone?
            let item = unsafe { self.access_buffer_mut()[self.read].clone() };
            self.len -= 1;
            self.read = (self.read + 1) % N;
            Ok(ManuallyDrop::into_inner(item))
        } else {
            Err(StackFifoError::Error)
        }
    }

    pub fn iter(&self) -> StackFifoIter<T, N> { StackFifoIter::from(self) }

    pub fn iter_mut(&mut self) -> StackFifoIterMut<T, N> { StackFifoIterMut::from(self) }

    pub fn len(&self) -> usize { self.len }

    pub const fn capacity(&self) -> usize { N }

    pub fn read_pos(&self) -> usize { self.read }

    pub fn write_pos(&self) -> usize { (self.read + self.len) % N }

    pub fn get<'a>(
        &self,
        pos: usize,
    ) -> Result<&'a T, StackFifoError> {
        if self.len() - pos > 0 {
            let true_pos = (self.read + pos) % N;
            // SAFETY: the api ensures safe access
            let item_ref = unsafe { self.access_buffer().get(true_pos) };
            Ok(item_ref.unwrap())
        } else {
            Err(StackFifoError::Error)
        }
    }

    pub fn get_mut<'a>(
        &mut self,
        pos: usize,
    ) -> Result<&'a mut T, StackFifoError> {
        if self.len() - pos > 0 {
            let true_pos = (self.read + pos) % N;
            // SAFETY: the api ensures safe access
            let item_ref = unsafe { self.access_buffer_mut().get_mut(true_pos) };
            Ok(item_ref.unwrap())
        } else {
            Err(StackFifoError::Error)
        }
    }

    /// The returned slice might be in an invalid state.s
    /// Its on the caller to make sure that the data is used in a
    /// safe way.
    unsafe fn access_buffer_mut<'a>(&mut self) -> &'a mut [ManuallyDrop<T>; N] {
        &mut *self.buffer.as_mut_ptr()
    }
    /// The returned slice might be in an invalid state.
    /// Its on the caller to make sure that the data is used in a
    /// safe way.
    unsafe fn access_buffer<'a>(&self) -> &'a [ManuallyDrop<T>; N] { &*self.buffer.as_ptr() }
}

impl<T, const N: usize> Drop for StackFifo<T, N> {
    fn drop(&mut self) {
        if std::mem::needs_drop::<T>() {
            let len = self.len();
            if len > 0 {
                for offset in 0..len {
                    unsafe {
                        let buf = self.access_buffer_mut();
                        let item = buf.get_mut(self.read_pos() + offset).unwrap();
                        ManuallyDrop::drop(item)
                    }
                }
            }
        }
    }
}

// StackFifoIter ===========================================================

pub struct StackFifoIter<'a, T, const N: usize> {
    pos: usize,
    inner: &'a StackFifo<T, N>,
}

impl<'a, T, const N: usize> Iterator for StackFifoIter<'a, T, N> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.inner.len() {
            match self.inner.get(self.pos) {
                Ok(item) => {
                    self.pos += 1;
                    return Some(item);
                },
                Err(_) => {},
            }
        }
        None
    }
}

impl<'a, T, const N: usize> From<&'a StackFifo<T, N>> for StackFifoIter<'a, T, N> {
    fn from(c: &'a StackFifo<T, N>) -> Self {
        Self {
            pos: 0,
            inner: c,
        }
    }
}

// StackFifoIterMut =======================================================

pub struct StackFifoIterMut<'a, T, const N: usize> {
    pos: usize,
    inner: &'a mut StackFifo<T, N>,
}

impl<'a, T, const N: usize> Iterator for StackFifoIterMut<'a, T, N> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.inner.len() {
            match self.inner.get_mut(self.pos) {
                Ok(item) => {
                    self.pos += 1;
                    return Some(item);
                },
                Err(_) => {},
            }
        }
        None
    }
}

impl<'a, T, const N: usize> From<&'a mut StackFifo<T, N>> for StackFifoIterMut<'a, T, N> {
    fn from(c: &'a mut StackFifo<T, N>) -> Self {
        Self {
            pos: 0,
            inner: c,
        }
    }
}

#[derive(Error, Debug)]
pub enum StackFifoError {
    #[error("StackFifoError")]
    Error,
}

// Tests ===================================================================

#[cfg(test)]
mod tests {
    use console::{
        self,
        set_colors_enabled_stderr,
        style,
        Term,
    };
    use pretty_assertions::assert_eq;

    use super::*;
    use util::*;

    lazy_static::lazy_static! {
        /// static console handle ensures `set_colors_enabled(true)`
        /// is called before printing
        pub static ref ERROR_PRINTER: Term = {
            set_colors_enabled_stderr(true);
            Term::stderr()
        };
    }
    // Push Back Tests =====================================================
    #[test]
    fn push_back_once() -> TestResult {
        catch_err("push_back_once", || {
            // Does the push_back() fn work if called once
            let mut fifo: StackFifo<i32, 2> = StackFifo::new();
            fifo.push_back(1).boxed()?;
            Ok(())
        })
    }
    
    #[test]
    fn push_back_multiple_wrapping() -> TestResult {
        catch_err("push_back_multiple_wrapping", || {
            // Does the push_back() fn work if called multiple times and wraps
            let mut fifo: StackFifo<i32, 4> = StackFifo::new();
            
            fifo.push_back(1).boxed()?;
            fifo.push_back(2).boxed()?;
            fifo.push_back(3).boxed()?;
            fifo.push_back(4).boxed()?;
            
            let _ = fifo.pop_front();
            let _ = fifo.pop_front();
            
            fifo.push_back(5).boxed()?;
            fifo.push_back(6).boxed()?;
            Ok(())
        })
    }
    
    #[test]
    fn push_back_multiple_wrapping_correctness() -> TestResult {
        catch_err("push_back_multiple_wrapping_correctness", || {
            // Does the push_back() fn correctly insert values if
            // called multiple times and wraps
            let mut fifo: StackFifo<i32, 5> = StackFifo::new();
            
            // [1, 2, 3, 4, 5]
            fifo.push_back(1).boxed()?;
            fifo.push_back(2).boxed()?;
            fifo.push_back(3).boxed()?;
            fifo.push_back(4).boxed()?;
            fifo.push_back(5).boxed()?;
            
            // [ _, _, _, 4, 5]
            assert_eq!(fifo.pop_front().boxed()?, 1);
            assert_eq!(fifo.pop_front().boxed()?, 2);
            assert_eq!(fifo.pop_front().boxed()?, 3);
            
            // [ 6, 7, _, 4, 5]
            fifo.push_back(6).boxed()?;
            fifo.push_back(7).boxed()?;
            
            let check = [4, 5, 6, 7];
            
            for (i, item) in fifo.iter().enumerate() {
                Equals::test(item, &check[i]).boxed()?;
            }
            Ok(())
        })
    }

    #[test]
    fn push_back_multiple_non_wrapping() -> TestResult {
        catch_err("push_back_multiple_non_wrapping", || {
            // Does the push_back() fn work if called multiple times but doesnt wrap
            let mut fifo: StackFifo<i32, 4> = StackFifo::new();
            fifo.push_back(1).boxed()?;
            fifo.push_back(2).boxed()?;
            fifo.push_back(3).boxed()?;
            fifo.push_back(4).boxed()?;
            
            assert_eq!(fifo.pop_front().boxed()?, 1);
            assert_eq!(fifo.pop_front().boxed()?, 2);
            assert_eq!(fifo.pop_front().boxed()?, 3);
            assert_eq!(fifo.pop_front().boxed()?, 4);
            
            Ok(())
        })
    }
    
    // Push Back Tests =====================================================
    

    #[test]
    fn fifo_push_back_overflow_err() -> TestResult {
        catch_ok("fifo_push_back_overflow_err", || {
            // This should return Err() because the capacity
            // was 2 and we tried to insert 3 items
            let mut fifo: StackFifo<i32, 2> = StackFifo::new();
            fifo.push_back(1).boxed()?;
            fifo.push_back(2).boxed()?;
            fifo.push_back(3).boxed()?; // <- Should cause error here
            Ok(())
        })
    }

    // TODO (George): Move this to its own file
    pub mod util {
        #![allow(dead_code)]

        use super::*;
        use std::marker::PhantomData;

        pub type TestResult = Result<(), TestError>;

        #[derive(Error)]
        pub struct Equals<L, R = L> 
        where
            L: std::fmt::Display + PartialEq<R>,
            R: std::fmt::Display
        {
            msg: String,
            /// fn() -> R because we dont own any L or R but we need to
            /// use the type param in the struct def.
            /// Generic is used in return type to maintain covariance
            _marker: PhantomData<(
                fn() -> L,
                fn() -> R,
            )>
        }

        // TODO (George): Pretty printing??
        impl<L, R> std::fmt::Display for Equals<L, R>
        where
            L: std::fmt::Display + PartialEq<R>,
            R: std::fmt::Display
        {
            fn fmt(
                &self,
                fmt: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                fmt.write_str(&self.msg)
            }
        }

        impl<L, R> std::fmt::Debug for Equals<L, R>
        where
            L: std::fmt::Display + PartialEq<R>,
            R: std::fmt::Display
        {
            fn fmt(
                &self,
                fmt: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                std::fmt::Display::fmt(&self, fmt)
            }
        }

        impl<L, R> Equals<L, R>
        where
            L: std::fmt::Display + PartialEq<R>,
            R: std::fmt::Display
        {
            /// Return `Err(Self)` 
            /// if ```left != right ``` is true
            ///
            /// else return `Ok(())`
            pub fn test<'a> (
                left: &L,
                right: &R,
            ) -> Result<(), Self>{
                if left != right {
                    Err( Self { 
                        msg: format!("{} != {}", left, right),
                        _marker: PhantomData
                    })
                } else {
                    Ok(())
                }
            }
        }

        pub fn catch_err<T>(
            name: &str,
            func: impl FnOnce() -> Result<T, Box<dyn std::error::Error>>,
        ) -> TestResult {
            match func() {
                Ok(_) => Ok(()),
                Err(e) => {
                    let out = format!(
                        "{}[{}]: {}\n\tsource: fn {}()\n",
                        style("")
                            .white()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        style("Error")
                            .red()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        style(&format!("{}", &e))
                            .yellow()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        name
                    );
                    let _ = ERROR_PRINTER.write_line(&out);
                    Err(TestError::TestReturnedError)
                },
            }
        }

        pub fn catch_ok<T>(
            name: &str,
            func: impl FnOnce() -> Result<T, Box<dyn std::error::Error>>,
        ) -> TestResult {
            match func() {
                Ok(_) => {
                    let out = format!(
                        "{}[{}]: {}\n\tsource: {}",
                        style("")
                            .white()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        style("Error")
                            .red()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        style("Test Returned Ok()")
                            .yellow()
                            .on_black()
                            .for_stderr()
                            .force_styling(true),
                        name
                    );
                    let _ = ERROR_PRINTER.write_line(&out);
                    Err(TestError::TestReturnedOk)
                },
                Err(_) => Ok(()),
            }
        }

        pub trait BoxedErr<T, E>
        where
            E: std::error::Error
        {
            fn boxed(self) -> Result<T, Box<E>>;
        }
        
        impl<T, E> BoxedErr<T, E> for Result<T, E>
        where
            E: std::error::Error
        {
            fn boxed(self) -> Result<T, Box<E>> {
                match self {
                    Ok(t) => Ok(t),
                    Err(e) => Err(Box::new(e)),
                }
            }
        }

        #[derive(Error, Debug)]
        pub enum TestError {
            #[error("Test Returned Error")]
            TestReturnedError,
            #[error("Test Shouldn't have returned Ok()")]
            TestReturnedOk,
            #[error(transparent)]
            IoError(std::io::Error),
        }
    }
}
