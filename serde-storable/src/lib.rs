#![allow(unknown_lints)]
#![warn(clippy::pedantic)]
#![deny(unused_must_use)]

use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

pub mod de;
pub mod error;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum PathElement {
    Root,
    Key(Rc<PathElement>, String),
    Index(Rc<PathElement>, usize),
}
impl PathElement {
    fn fmt_parent(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PathElement::Root => write!(f, "<root>->"),
            PathElement::Key(pre, cur) => {
                pre.fmt_parent(f)?;
                write!(f, "{{{}}}", cur)
            }
            PathElement::Index(pre, idx) => {
                pre.fmt_parent(f)?;
                write!(f, "[{}]", idx)
            }
        }
    }
}
impl Display for PathElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PathElement::Root => write!(f, "<root>"),
            o => o.fmt_parent(f),
        }
    }
}
