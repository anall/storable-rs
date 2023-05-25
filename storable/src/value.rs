use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::{Rc, Weak};

use crate::vstring;

/// A `PerlValue` wrapped inside `Rc<RefCell<...>>`
#[allow(clippy::module_name_repetitions)]
pub type ValueRc<ST, BT> = Rc<RefCell<Value<ST, BT>>>;

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::module_name_repetitions)]
pub enum FlagHashValue<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
> {
    Value(ValueRc<ST, BT>),
    ReadOnly(ValueRc<ST, BT>),
    Placeholder,
}

#[derive(Clone)]
#[allow(clippy::upper_case_acronyms)]
/// An enum representing any possible `PerlValue`
pub enum Value<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
> {
    /// Placeholder for internal use only, should never be seen in final results
    Dummy,

    /// An `undef` value, which may or may not be the immortal `undef` (specified by the `bool`)
    Undef(bool),

    /// The immortal `true` value (`PL_sv_yes`)
    Yes,
    /// The immortal `false` value (`PL_sv_no`)
    No,
    /// A blessed value, storing both the internal value and the class
    Blessed(ValueRc<ST, BT>, ST),

    /// A string with valid UTF8 (which may or may not be flagged)
    String(ST, bool),
    /// A string with invalid UTF8 (possibly binary data)
    Bytes(BT),
    /// An array
    Array(Vec<ValueRc<ST, BT>>),
    /// A v-string (version string)
    VString(vstring::VString<ST, BT>),

    /// A hash
    Hash(HashMap<ST, ValueRc<ST, BT>>),
    /// A hash with flags on the hash values
    FlagHash(HashMap<ST, FlagHashValue<ST, BT>>),

    /// A hash with non-utf8 high-ascii hash keys
    HashByte(HashMap<BT, ValueRc<ST, BT>>),
    /// A hash with non-utf8 high-ascii hash keys and with flags on the hash values
    FlagHashByte(HashMap<BT, FlagHashValue<ST, BT>>),

    /// A ref to another `Value`
    Ref(ValueRc<ST, BT>),
    /// A weak reference to another `Value`
    WeakRef(Weak<RefCell<Value<ST, BT>>>),

    /// A ref to another `Value`, with an overload involved
    Overload(ValueRc<ST, BT>),

    /// A tied scalar value, storing both the internal value and the tie class.
    ///
    /// This does not actually allow accessing the value through the tie without re-implementing the
    /// Perl semantics.
    TiedScalar(ValueRc<ST, BT>, ST),
    /// A tied array, storing both the internal value and the tie class.
    ///
    /// This does not actually allow accessing the value through the tie without re-implementing the
    /// Perl semantics.
    TiedArray(ValueRc<ST, BT>, ST),
    /// A tied array index access, storing the internal value, the tie class, and the index.
    ///
    /// This does not actually allow accessing the value through the tie without re-implementing the
    /// Perl semantics.
    TiedArrayIdx(ValueRc<ST, BT>, ST, usize),
    /// A tied hash, storing both the internal value and the tie class.
    ///
    /// This does not actually allow accessing the value through the tie without re-implementing the
    /// Perl semantics.
    TiedHash(ValueRc<ST, BT>, ST),
    /// A tied hash key access, storing the internal value, the tie class, and the key.
    ///
    /// This does not actually allow accessing the value through the tie without re-implementing the
    /// Perl semantics.
    TiedHashKey(ValueRc<ST, BT>, ST, ValueRc<ST, BT>),

    /// Signed integer value.
    IV(i64),

    /// Unsigned integer value.
    ///
    /// This is currently inaccessible through the Storable bytestream but keeping for future
    /// compatibility without breaking the API. This will currently be stored out as either a
    /// signed value (if it fits) or a string.
    UV(u64),
}
impl<
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
    > Value<ST, BT>
{
    /// Calls the closure `c` after simplifying any weak or special refs to `Ref`
    fn simplify_with<T, F: Fn(&Self) -> T>(&self, c: F) -> T {
        match self {
            // Ref can be passed through as is
            Self::WeakRef(weak) => c(&weak.upgrade().map_or(Self::Dummy, Self::Ref)),
            o => c(o),
        }
    }

    /// Wrap a `PerlValue` inside the `Rc<RefCell<...>>`
    ///
    /// ```
    /// # use storable::Value;
    /// let wrapped = Value::<&str,&[u8]>::wrap(Value::IV(5));
    /// ```
    #[must_use]
    pub fn wrap(pv: Value<ST, BT>) -> ValueRc<ST, BT> {
        Rc::new(RefCell::new(pv))
    }
}
impl<
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
    > PartialEq for Value<ST, BT>
{
    fn eq(&self, other: &Self) -> bool {
        #[allow(clippy::enum_glob_use)]
        use Value::*;
        self.simplify_with(|left| {
            other.simplify_with(|right| match (left, right) {
                // Never add Dummy to this
                (Undef(_), Undef(_)) | (Yes, Yes) | (No, No) => true,

                (String(a, _), String(b, _)) => a == b, // we don't care if the flag is the same
                (Bytes(a), Bytes(b)) => a == b,
                (Ref(a), Ref(b)) => a == b,
                (Blessed(a, i), Blessed(b, j)) => a == b && i == j,
                (Array(a), Array(b)) => a == b,

                (Hash(a), Hash(b)) => a.eq(b),
                (FlagHash(a), FlagHash(b)) => a.eq(b),

                (HashByte(a), HashByte(b)) => a == b,
                (FlagHashByte(a), FlagHashByte(b)) => a == b,

                (TiedScalar(a1, a2), TiedScalar(b1, b2))
                | (TiedArray(a1, a2), TiedArray(b1, b2))
                | (TiedHash(a1, a2), TiedHash(b1, b2)) => (a1, a2) == (b1, b2),

                (TiedArrayIdx(a1, a2, a3), TiedArrayIdx(b1, b2, b3)) => {
                    (a1, a2, a3) == (b1, b2, b3)
                }
                (TiedHashKey(a1, a2, a3), TiedHashKey(b1, b2, b3)) => (a1, a2, a3) == (b1, b2, b3),

                (VString(a), VString(b)) => a == b,

                (IV(a), IV(b)) => a == b,
                (UV(a), UV(b)) => a == b,

                _ => false,
            })
        })
    }
}

impl<
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
    > Debug for Value<ST, BT>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dummy => write!(f, "Dummy"),
            Self::Undef(immortal) => f.debug_struct(&"Undef").field("immortal", immortal).finish(),
            Self::Yes => write!(f, "Yes"),
            Self::No => write!(f, "No"),
            Self::Blessed(val, class) => f
                .debug_tuple(&format!("Blessed[{:?}]", class))
                .field(&val.borrow())
                .finish(),
            Self::String(val, flagged) => {
                if *flagged {
                    write!(f, "String(utf8)[{:?}]", val)
                } else {
                    write!(f, "String[{:?}]", val)
                }
            }
            Self::Bytes(val) => write!(f, "Bytes[{:?}]", val),
            Self::Array(val) => {
                let mut list = f.debug_list();
                for v in val {
                    list.entry(&v.borrow());
                }
                list.finish()
            }
            Self::VString(val) => f.debug_tuple("VString").field(val).finish(),
            Self::Hash(arg0) => f.debug_tuple("Hash").field(arg0).finish(),
            Self::FlagHash(arg0) => f.debug_tuple("FlagHash").field(arg0).finish(),
            Self::HashByte(arg0) => f.debug_tuple("HashByte").field(arg0).finish(),
            Self::FlagHashByte(arg0) => f.debug_tuple("FlagHashByte").field(arg0).finish(),
            Self::Ref(val) => f.debug_tuple("Ref").field(&val.borrow()).finish(),
            Self::WeakRef(arg0) => f.debug_tuple("WeakRef").field(arg0).finish(),
            Self::Overload(arg0) => f.debug_tuple("Overload").field(&arg0.borrow()).finish(),
            Self::TiedScalar(val, class) => f
                .debug_tuple("TiedScalar")
                .field(&val.borrow())
                .field(class)
                .finish(),
            Self::TiedArray(val, class) => f
                .debug_tuple("TiedArray")
                .field(&val.borrow())
                .field(class)
                .finish(),
            Self::TiedArrayIdx(val, class, idx) => f
                .debug_tuple("TiedArrayIdx")
                .field(&val.borrow())
                .field(class)
                .field(idx)
                .finish(),
            Self::TiedHash(val, class) => f
                .debug_tuple("TiedHash")
                .field(&val.borrow())
                .field(class)
                .finish(),
            Self::TiedHashKey(val, class, key) => f
                .debug_tuple("TiedHashKey")
                .field(&val.borrow())
                .field(class)
                .field(key)
                .finish(),
            Self::IV(val) => write!(f, "IV[{}]", val),
            Self::UV(val) => write!(f, "UV[{}]", val),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use std::rc::Rc;

    #[test]
    fn test_simplify_with() {
        let val = Value::<&str, &[u8]>::wrap(Value::IV(5));
        let rv = Value::wrap(Value::<&str, &[u8]>::Ref(val.clone()));
        let weak = Value::wrap(Value::<&str, &[u8]>::WeakRef(Rc::downgrade(&val)));
        let weak_b = weak.borrow();

        weak_b.simplify_with(|pv| match pv {
            Value::Ref(inner) => {
                assert!(Rc::ptr_eq(&val, inner))
            }
            _ => panic!("got invalid value {:?}", pv),
        });

        assert_eq!(rv, weak);
    }

    #[test]
    fn test_various_pv_variants() {
        let hello = "hello world".to_string();
        let _ = Value::wrap(Value::<&str, &[u8]>::String(&hello, false));
        let _ = Value::wrap(Value::<&str, &[u8]>::Bytes(&[1]));

        let _ = Value::wrap(Value::<String, Vec<u8>>::String(hello.clone(), false));
        let _ = Value::wrap(Value::<String, Vec<u8>>::Bytes(vec![1]));
    }
}
