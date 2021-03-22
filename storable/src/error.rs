use crate::markers::InvalidMarkerError;
use core::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::io;

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum ThawError {
    Io(io::Error),
    OldFormatUnsupported,
    NativeByteOrderUnsupported,
    MoreRecent(u8, u8),
    InvalidMarker(u8),
    UnsupportedMarker(&'static str),
    GotNonNetOrderMarker(&'static str),
    InvalidUtf8,
    InvalidHash,
    SvKeysUnsupported,
    WouldLeak,
    ObjectOutOfRange,
    CorruptedLength,
    OverLongUnsupported,
    DeserializeSizeExceeded,
    UnitSizeExceeded,
    BlessedDisabled,
    TiedDisabled,
    InvalidTied,
    MaxDepthReached,
}
impl From<io::Error> for ThawError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<InvalidMarkerError> for ThawError {
    fn from(err: InvalidMarkerError) -> Self {
        Self::InvalidMarker(err.0)
    }
}

impl<BT: AsRef<[u8]> + PartialEq + Eq + Hash> From<InternalError<BT>> for ThawError {
    fn from(err: InternalError<BT>) -> Self {
        match err {
            InternalError::Public(e) => e,
            InternalError::InvalidUtf8(_) => Self::InvalidUtf8,
        }
    }
}

impl fmt::Display for ThawError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for ThawError {}

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum InternalError<BT: AsRef<[u8]> + PartialEq + Eq + Hash> {
    Public(ThawError),
    InvalidUtf8(BT),
}
impl<BT: AsRef<[u8]> + PartialEq + Eq + Hash> From<ThawError> for InternalError<BT> {
    fn from(err: ThawError) -> Self {
        Self::Public(err)
    }
}

impl<BT: AsRef<[u8]> + PartialEq + Eq + Hash> fmt::Display for InternalError<BT> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            InternalError::Public(err) => write!(f, "Public Error [{:?}]", err),
            InternalError::InvalidUtf8(bytes) => write!(f, "InvalidUtf[{:?}]", bytes.as_ref()),
        }
    }
}
impl<BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug> std::error::Error for InternalError<BT> {}
