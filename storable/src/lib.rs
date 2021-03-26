#![allow(unknown_lints)]
#![warn(clippy::pedantic)]
#![deny(unused_must_use)]
// FIXME: remove this once 'upper_case_acronyms' is in stable
#![allow(clippy::unknown_clippy_lints)]

//! Low-level serialization and deserialization for Perl's `Storable` data format.
//!
//! This crate also allows manipulation of the deserialized data.
//!
//! Unless you need low-level access to the data structures, you probably want to use the `serde_storable` crate instead
//! to deserialize the data into a more convenient format. But this module still may be useful if you wish to
//! work with data that does not map easily (or at all) into Rust constructs.
//!
#[cfg(test)]
mod tests;

mod error;
mod markers;
mod value;
mod thaw_reader;
mod thaw_settings;
mod vstring;

pub use crate::error::ThawError;
pub use crate::value::{FlagHashValue, Value, ValueRc};
pub use crate::thaw_reader::ThawReader;
pub use crate::thaw_settings::ThawSettings;
pub use crate::vstring::VString;

use std::io::{self, Read};

mod constants {
    pub const MAGICSTR: [u8; 4] = [b'p', b's', b't', b'0'];

    pub const STORABLE_BIN_MAJOR: u8 = 2;
    pub const STORABLE_BIN_MINOR: u8 = 11;

    pub const SHV_RESTRICTED: u8 = 0x01;

    pub const SHV_K_UTF8: u8 = 0x01;
    pub const SHV_K_WASUTF8: u8 = 0x02;
    pub const SHV_K_LOCKED: u8 = 0x04;
    pub const SHV_K_ISSV: u8 = 0x08;
    pub const SHV_K_PLACHOLDER: u8 = 0x10;
}

struct ByteSliceThawReader<'a> {
    slice: &'a [u8],
    cur: usize,
}
impl<'a> ThawReader<&'a str, &'a [u8]> for ByteSliceThawReader<'a> {
    fn read_single_byte(&mut self) -> Result<u8, ThawError> {
        if self.cur < self.slice.len() {
            self.cur += 1;
            Ok(self.slice[self.cur - 1])
        } else {
            Err(io::Error::new(io::ErrorKind::UnexpectedEof, "ran out of buffer").into())
        }
    }

    fn read_short_exact(&mut self, buf: &mut [u8]) -> Result<(), ThawError> {
        let n = buf.len();
        if self.cur + n <= self.slice.len() {
            buf.clone_from_slice(&self.slice[self.cur..self.cur + n]);
            self.cur += n;
            Ok(())
        } else {
            Err(io::Error::new(io::ErrorKind::UnexpectedEof, "ran out of buffer").into())
        }
    }

    fn possibly_clone_st(st: &&'a str) -> &'a str {
        *st
    }

    fn st_into_bt(st: &'a str) -> &'a [u8] {
        st.as_bytes()
    }

    fn bytes_from_utf8(&mut self, bytes: &'a [u8]) -> Result<&'a str, &'a [u8]> {
        std::str::from_utf8(bytes).map_err(|_| bytes)
    }

    fn read_bytes_unchecked(&mut self, n: usize) -> Result<&'a [u8], ThawError> {
        if self.cur + n <= self.slice.len() {
            let rv = &self.slice[self.cur..self.cur + n];
            self.cur += n;
            Ok(rv)
        } else {
            Err(io::Error::new(io::ErrorKind::UnexpectedEof, "ran out of buffer").into())
        }
    }
}
// TODO: during development
#[allow(clippy::missing_errors_doc)]
pub fn thaw_from_bytes<'a>(
    slice: &'a [u8],
    settings: &ThawSettings,
) -> Result<ValueRc<&'a str, &'a [u8]>, ThawError> {
    let mut reader = ByteSliceThawReader { slice, cur: 0 };
    reader.thaw(settings)
}

struct IoThawReader<'a, T: Read> {
    reader: &'a mut T,
}
impl<'a, T: Read> ThawReader<String, Vec<u8>> for IoThawReader<'a, T> {
    fn read_single_byte(&mut self) -> Result<u8, ThawError> {
        let mut buf = [0_u8];
        self.reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    fn read_short_exact(&mut self, buf: &mut [u8]) -> Result<(), ThawError> {
        self.reader.read_exact(buf).map_err(|e| e.into())
    }

    fn possibly_clone_st(st: &String) -> String {
        st.clone()
    }

    fn st_into_bt(st: String) -> Vec<u8> {
        st.into_bytes()
    }

    fn bytes_from_utf8(&mut self, bytes: Vec<u8>) -> Result<String, Vec<u8>> {
        String::from_utf8(bytes).map_err(std::string::FromUtf8Error::into_bytes)
    }

    fn read_bytes_unchecked(&mut self, n: usize) -> Result<Vec<u8>, ThawError> {
        let mut out = Vec::with_capacity(n);
        unsafe {
            out.set_len(n);
        } // SAFETY: u8 is a primitive type, and read_exact will initialize the entire buffer
        self.reader.read_exact(&mut out)?;
        Ok(out)
    }
}
// TODO: during development
#[allow(clippy::missing_errors_doc)]
pub fn thaw_from_io<T: Read>(
    reader: &mut T,
    settings: &ThawSettings,
) -> Result<ValueRc<String, Vec<u8>>, ThawError> {
    let mut reader = IoThawReader { reader };
    reader.thaw(settings)
}
