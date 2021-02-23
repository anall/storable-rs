use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;

/// A wrapped `PerlValue`
pub type PerlValueRc = Rc<RefCell<PerlValue>>;

#[derive(Debug,PartialEq,Clone)]
pub enum FlagHashValue {
    Value(PerlValueRc),
    ReadOnly(PerlValueRc),
    Placeholder
}

#[derive(Debug,Clone)]
#[allow(clippy::upper_case_acronyms)]
/// An enum representing any possible `PerlValue`
pub enum PerlValue {
    /// Placeholder for internal use only, should never be seen in final results
    Dummy,

    Undef,
    Blessed(PerlValueRc,String),

    Scalar(String),
    ScalarBytes(Vec<u8>),
    Array(Vec<PerlValueRc>),

    Hash(HashMap<String,PerlValueRc>),
    FlagHash(HashMap<String,FlagHashValue>),

    // in case we have something with weird non-utf8 high-ascii keys
    HashByte(HashMap<Vec<u8>,PerlValueRc>),
    FlagHashByte(HashMap<Vec<u8>,FlagHashValue>),

    Ref(PerlValueRc),
    WeakRef(Weak<RefCell<PerlValue>>),

    TiedScalar(PerlValueRc,String),
    TiedArray(PerlValueRc,String),
    TiedArrayIdx(PerlValueRc,String,usize),
    TiedHash(PerlValueRc,String),
    TiedHashKey(PerlValueRc,String,PerlValueRc),

    IV(i64),
}
impl PerlValue {
    /// Calls the closure `c` after simplifying any weak or special refs to `Ref`
    fn simplify_with<T, F: Fn(&Self) -> T>(&self, c : F) -> T {
        match self {
            // Ref can be passed through as is
            Self::WeakRef(weak) => {
                c( &weak.upgrade().map_or(Self::Dummy, Self::Ref ) )
            }
            o => c(o)
        }
    }

    /// Wrap a `PerlValue` inside the `Rc<RefCell<...>>`
    ///
    /// ```
    /// use storable::PerlValue;
    /// let wrapped = PerlValue::wrap(PerlValue::IV(5));
    /// ```
    #[must_use]
    pub fn wrap(pv : PerlValue) -> PerlValueRc {
        Rc::new(RefCell::new( pv ))
    }

    #[must_use]
    pub(crate) fn scalar_or_bytes(bytes : Vec<u8>, flagged : bool, upgrade_unflagged_utf8 : bool ) -> PerlValue {
        if upgrade_unflagged_utf8 || flagged || bytes.iter().find(|&&ch| ch >= 128).is_none() {
            match String::from_utf8(bytes) {
                Ok(str) => Self::Scalar(str),
                Err(err) => Self::ScalarBytes(err.into_bytes())
            }
        } else {
            Self::ScalarBytes(bytes)
        }
    }
}
impl PartialEq for PerlValue {
    fn eq(&self, other: &Self) -> bool {
        #[allow(clippy::enum_glob_use)]
        use PerlValue::*;
        self.simplify_with(|left| other.simplify_with(|right|
            match (left,right) { // Never add Dummy to this
                (Undef, Undef) => true,

                (Scalar(a),Scalar(b)) => a == b,
                (ScalarBytes(a),ScalarBytes(b)) => a == b,
                (Ref(a),Ref(b)) => a == b,
                (Blessed(a,i),Blessed(b,j)) => a == b && i == j,
                (Array(a),Array(b)) => a == b,

                (Hash(a),Hash(b)) => a == b,
                (FlagHash(a),FlagHash(b)) => a == b,

                (HashByte(a),HashByte(b)) => a == b,
                (FlagHashByte(a),FlagHashByte(b)) => a == b,

                (TiedScalar(a1,a2), TiedScalar(b1,b2)) |
                    (TiedArray(a1,a2), TiedArray(b1,b2)) |
                    (TiedHash(a1,a2), TiedHash(b1,b2)) => (a1,a2) == (b1,b2),

                (TiedArrayIdx(a1,a2,a3), TiedArrayIdx(b1,b2,b3)) => (a1,a2,a3) == (b1,b2,b3),
                (TiedHashKey(a1,a2,a3), TiedHashKey(b1,b2,b3)) => (a1,a2,a3) == (b1,b2,b3),

                (IV(a),IV(b)) => a == b,
                _ => false
            }
        ))
    }
}