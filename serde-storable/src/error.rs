use crate::PathElement;
use serde::{de, ser};
use std::fmt::{self, Display};
use std::num::{ParseFloatError, ParseIntError, TryFromIntError};
use std::rc::Rc;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub struct Error {
    code: ErrorCode,
    path: Option<Rc<PathElement>>,
}
impl Error {
    pub(crate) fn new(code: ErrorCode, path: &Rc<PathElement>) -> Self {
        Error {
            code,
            path: Some(path.clone()),
        }
    }

    #[inline]
    pub(crate) fn with_path(self, path: &Rc<PathElement>) -> Self {
        if self.path.is_some() {
            self
        } else {
            Error {
                code: self.code,
                path: Some(path.clone()),
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ErrorCode {
    IntegerOverflow,
    ParseInt,
    ParseFloat,
    Message(String),
    CannotBorrow,
    UnexpectedPerlValue,
    InternalException(&'static str),
}

impl From<ErrorCode> for Error {
    fn from(code: ErrorCode) -> Self {
        Error { code, path: None }
    }
}
impl From<TryFromIntError> for Error {
    fn from(_: TryFromIntError) -> Self {
        Error {
            code: ErrorCode::IntegerOverflow,
            path: None,
        }
    }
}
impl From<ParseIntError> for Error {
    fn from(_: ParseIntError) -> Self {
        Error {
            code: ErrorCode::ParseInt,
            path: None,
        }
    }
}
impl From<ParseFloatError> for Error {
    fn from(_: ParseFloatError) -> Self {
        Error {
            code: ErrorCode::ParseFloat,
            path: None,
        }
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self {
            code: ErrorCode::Message(msg.to_string()),
            path: None,
        }
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Self {
            code: ErrorCode::Message(msg.to_string()),
            path: None,
        }
    }
}

impl ser::Error for ErrorCode {
    fn custom<T: Display>(msg: T) -> Self {
        ErrorCode::Message(msg.to_string())
    }
}

impl de::Error for ErrorCode {
    fn custom<T: Display>(msg: T) -> Self {
        ErrorCode::Message(msg.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if let Some(path) = self.path.as_ref() {
            write!(formatter, "got {} at {}", self.code, path)
        } else {
            write!(formatter, "got {} at <unknown>", self.code)
        }
    }
}

impl Display for ErrorCode {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorCode::Message(msg) => formatter.write_str(msg),
            ErrorCode::IntegerOverflow => formatter.write_str("integer value overflowed"),
            ErrorCode::ParseInt => formatter.write_str("integer parse failed"),
            ErrorCode::ParseFloat => formatter.write_str("float parse failed"),
            ErrorCode::CannotBorrow => formatter.write_str("cannot borrow"),
            ErrorCode::UnexpectedPerlValue => formatter.write_str("unexpected perl value"),
            ErrorCode::InternalException(str) => write!(formatter, "internal exception: {}", *str),
            /* and so forth */
        }
    }
}

impl std::error::Error for Error {}
impl std::error::Error for ErrorCode {}
