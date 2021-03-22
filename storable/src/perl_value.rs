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
pub enum FlagHashValue<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
> {
    Value(ValueRc<ST, BT>),
    ReadOnly(ValueRc<ST, BT>),
    Placeholder,
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
/// An enum representing any possible `PerlValue`
pub enum Value<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
> {
    /// Placeholder for internal use only, should never be seen in final results
    Dummy,

    /// An undef value, which may or may not be the immortal undef (`PL_sv_undef`).
    ///
    /// Open a ticket if you need to specify which undef gets serialized or know which
    /// gets deserialized.
    Undef,

    /// The immortal `true` value (`PL_sv_yes`)
    Yes,
    /// The immortal `false` value (`PL_sv_no`)
    No,
    /// A blessed value
    Blessed(ValueRc<ST, BT>, ST),

    String(ST),
    VString(vstring::VString<ST, BT>),
    Bytes(BT),
    Array(Vec<ValueRc<ST, BT>>),

    Hash(HashMap<ST, ValueRc<ST, BT>>),
    FlagHash(HashMap<ST, FlagHashValue<ST, BT>>),

    // in case we have something with weird non-utf8 high-ascii keys
    HashByte(HashMap<BT, ValueRc<ST, BT>>),
    FlagHashByte(HashMap<BT, FlagHashValue<ST, BT>>),

    Ref(ValueRc<ST, BT>),
    WeakRef(Weak<RefCell<Value<ST, BT>>>),

    TiedScalar(ValueRc<ST, BT>, ST),
    TiedArray(ValueRc<ST, BT>, ST),
    TiedArrayIdx(ValueRc<ST, BT>, ST, usize),
    TiedHash(ValueRc<ST, BT>, ST),
    TiedHashKey(ValueRc<ST, BT>, ST, ValueRc<ST, BT>),

    /// Signed integer value.
    IV(i64),

    /// Unsigned integer value.
    ///
    /// This is currently inaccessible through the Storable bytestream but keeping for future
    /// compatibility without breaking the API.
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
                (Undef, Undef) | (Yes, Yes) | (No, No) => true,

                (String(a), String(b)) => a == b,
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

#[cfg(test)]
mod tests {
    use crate::perl_value::Value;
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
        let _ = Value::wrap(Value::<&str, &[u8]>::String(&hello));
        let _ = Value::wrap(Value::<&str, &[u8]>::Bytes(&[1]));

        let _ = Value::wrap(Value::<String, Vec<u8>>::String(hello.clone()));
        let _ = Value::wrap(Value::<String, Vec<u8>>::Bytes(vec![1]));
    }
}
