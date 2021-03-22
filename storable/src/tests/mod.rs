use crate::perl_value::FlagHashValue;
use crate::{ThawError, ThawSettings, VString, Value, ValueRc};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Cursor;
use std::rc::Rc;

fn wrap<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
>(
    pv: Value<ST, BT>,
) -> ValueRc<ST, BT> {
    Rc::new(RefCell::new(pv))
}

fn wrap_iv<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
>(
    i: i64,
) -> ValueRc<ST, BT> {
    wrap(Value::IV(i))
}

#[test]
fn thaw_scalar() {
    static DATA: [u8; 7] = [0x05, 0x0b, 0x0a, 0x03, 0x66, 0x6f, 0x6f];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic())
            .map_err(|e| format!("{:?}", e))
            .expect("got error"),
        wrap(Value::String("foo"))
    );
}

#[test]
fn thaw_scalar_io() {
    static DATA: [u8; 7] = [0x05, 0x0b, 0x0a, 0x03, 0x66, 0x6f, 0x6f];

    assert_eq!(
        crate::thaw_from_io(&mut Cursor::new(DATA), &ThawSettings::without_magic())
            .map_err(|e| format!("{:?}", e))
            .expect("got error"),
        wrap(Value::String("foo".to_string()))
    );
}

#[test]
fn thaw_unflagged_scalar() {
    static DATA: [u8; 6] = [0x05, 0x0b, 0x0a, 0x02, 0xc3, 0xa6];

    assert_eq!(
        crate::thaw_from_io(&mut Cursor::new(DATA), &ThawSettings::without_magic())
            .expect("got error"),
        wrap(Value::Bytes(vec![0xc3, 0xa6]))
    );

    assert_eq!(
        crate::thaw_from_io(
            &mut Cursor::new(DATA),
            &ThawSettings::without_magic().and_upgrade_unflagged_utf8()
        )
        .expect("got error"),
        wrap(Value::String("æ".to_string()))
    );
}

#[test]
fn thaw_unflagged_scalar_io() {
    static DATA: [u8; 6] = [0x05, 0x0b, 0x0a, 0x02, 0xc3, 0xa6];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error"),
        wrap(Value::<_, &[u8]>::Bytes(&[0xc3, 0xa6]))
    );

    assert_eq!(
        crate::thaw_from_bytes(
            &DATA,
            &ThawSettings::without_magic().and_upgrade_unflagged_utf8()
        )
        .expect("got error"),
        wrap(Value::String("æ"))
    );
}

#[test]
fn thaw_flagged_scalar() {
    static DATA: [u8; 6] = [0x05, 0x0b, 0x17, 0x02, 0xc3, 0xa6];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error"),
        wrap(Value::String("æ"))
    );
}

#[test]
fn thaw_long_scalar() {
    static DATA: [u8; 263] = [
        0x05, 0x0b, 0x01, 0x00, 0x00, 0x01, 0x00, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
        0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33,
        0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32,
        0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31,
        0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
        0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33,
        0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32,
        0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31,
        0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
        0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33,
        0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32,
        0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31,
        0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
        0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33,
        0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32,
        0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31,
        0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
        0x31, 0x32, 0x33, 0x34, 0x31, 0x32, 0x33, 0x34,
    ];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic() ).expect("got error"),
        wrap( Value::String( "1234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234" ) )
    );
}

#[test]
fn thaw_flagged_long_scalar() {
    static DATA: [u8; 265] = [
        0x05, 0x0b, 0x18, 0x00, 0x00, 0x01, 0x02, 0xc3, 0xa6, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65,
        0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62,
        0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64,
        0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61,
        0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65,
        0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64,
        0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66,
        0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65,
        0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65,
        0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62,
        0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64,
        0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61,
        0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65,
        0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64,
        0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66,
        0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65,
        0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65,
        0x65, 0x66, 0x64, 0x65, 0x61, 0x64, 0x62, 0x65, 0x65, 0x66,
    ];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic() ).expect("got error"),
        wrap( Value::String( "ædeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef" ) )
    );
}

#[test]
fn thaw_array() {
    static DATA: [u8; 12] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x03, 0x66, 0x6f, 0x6f,
    ];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error"),
        wrap(Value::Array(vec![wrap(Value::String("foo"))]))
    );
}

#[test]
fn thaw_net_integer() {
    static DATA: [u8; 7] = [0x05, 0x0b, 0x09, 0x00, 0x00, 0xff, 0xff];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error"),
        wrap(Value::IV(65535))
    );
}

// add hand-crafted tests for SxInteger / SxDouble
#[test]
fn thaw_hash() {
    static DATA: [u8; 24] = [
        0x05, 0x0b, 0x03, 0x00, 0x00, 0x00, 0x02, 0x08, 0x81, 0x00, 0x00, 0x00, 0x02, 0x68, 0x69,
        0x08, 0x82, 0x00, 0x00, 0x00, 0x03, 0x62, 0x79, 0x65,
    ];

    let mut hash: HashMap<&str, ValueRc<&str, &[u8]>> = HashMap::new();
    hash.insert("hi", wrap(Value::IV(1)));
    hash.insert("bye", wrap(Value::IV(2)));

    let parsed = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(parsed, wrap(Value::Hash(hash)));
}

#[test]
fn thaw_hash_unflagged_utf8_key() {
    static DATA: [u8; 16] = [
        0x05, 0x0b, 0x03, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x01, 0x61, 0x00, 0x00, 0x00, 0x02, 0xc3,
        0xa6,
    ];

    let err = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect_err("got error");
    if let ThawError::InvalidUtf8 = err {
        // good
    } else {
        panic!("got a different error {:?}", err);
    }

    let settings = ThawSettings::without_magic().and_with_byte_hashes();
    let result = crate::thaw_from_bytes(&DATA, &settings).expect("got error");
    let mut expected: HashMap<&[u8], ValueRc<&str, &[u8]>> = HashMap::new();
    expected.insert(&[0xc3, 0xa6], wrap(Value::String("a")));
    assert_eq!(result, wrap(Value::HashByte(expected)));

    let settings = ThawSettings::without_magic().and_upgrade_unflagged_utf8();
    let result = crate::thaw_from_bytes(&DATA, &settings).expect("got error");
    let mut expected: HashMap<&str, ValueRc<&str, &[u8]>> = HashMap::new();
    expected.insert("æ", wrap(Value::String("a")));
    assert_eq!(result, wrap(Value::Hash(expected)));
}

#[test]
fn thaw_hash_unflagged_utf8_key_io() {
    static DATA: [u8; 16] = [
        0x05, 0x0b, 0x03, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x01, 0x61, 0x00, 0x00, 0x00, 0x02, 0xc3,
        0xa6,
    ];

    let err = crate::thaw_from_io(&mut Cursor::new(DATA), &ThawSettings::without_magic())
        .expect_err("got error");
    if let ThawError::InvalidUtf8 = err {
        // good
    } else {
        panic!("got a different error {:?}", err);
    }

    let settings = ThawSettings::without_magic().and_with_byte_hashes();
    let result = crate::thaw_from_io(&mut Cursor::new(DATA), &settings).expect("got error");
    let mut expected: HashMap<Vec<u8>, ValueRc<String, Vec<u8>>> = HashMap::new();
    expected.insert(vec![0xc3, 0xa6], wrap(Value::String("a".to_string())));
    assert_eq!(result, wrap(Value::HashByte(expected)));

    let settings = ThawSettings::without_magic().and_upgrade_unflagged_utf8();
    let result = crate::thaw_from_io(&mut Cursor::new(DATA), &settings).expect("got error");
    let mut expected: HashMap<String, ValueRc<String, Vec<u8>>> = HashMap::new();
    expected.insert("æ".to_string(), wrap(Value::String("a".to_string())));
    assert_eq!(result, wrap(Value::Hash(expected)));
}

#[test]
fn thaw_hash_flagged_utf8_key() {
    static DATA: [u8; 18] = [
        0x05, 0x0b, 0x19, 0x00, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x01, 0x61, 0x02, 0x00, 0x00, 0x00,
        0x02, 0xc3, 0xa6,
    ];

    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    let mut expected: HashMap<&str, ValueRc<&str, &[u8]>> = HashMap::new();
    expected.insert("æ", wrap(Value::String("a")));
    assert_eq!(result, wrap(Value::Hash(expected)));
}

#[test]
fn thaw_hash_bad_utf8_key() {
    static DATA: [u8; 15] = [
        0x05, 0x0b, 0x03, 0x00, 0x00, 0x00, 0x01, 0x08, 0x81, 0x00, 0x00, 0x00, 0x02, 0xa6, 0xc3,
    ];

    let err = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect_err("got error");
    if let ThawError::InvalidUtf8 = err {
        // good
    } else {
        panic!("got a different error {:?}", err);
    }

    let settings = ThawSettings::without_magic().and_with_byte_hashes();
    let result = crate::thaw_from_bytes(&DATA, &settings).expect("got error");
    let mut expected: HashMap<&[u8], ValueRc<&str, &[u8]>> = HashMap::new();
    expected.insert(&[166, 195], wrap(Value::IV(1)));
    assert_eq!(result, wrap(Value::HashByte(expected)));
}

#[test]
fn thaw_restricted_hash() {
    static DATA: [u8; 37] = [
        0x05, 0x0b, 0x19, 0x01, 0x00, 0x00, 0x00, 0x03, 0x08, 0x81, 0x00, 0x00, 0x00, 0x00, 0x03,
        0x66, 0x6f, 0x6f, 0x0e, 0x14, 0x00, 0x00, 0x00, 0x03, 0x62, 0x61, 0x7a, 0x08, 0x82, 0x04,
        0x00, 0x00, 0x00, 0x03, 0x62, 0x61, 0x72,
    ];

    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    let mut expected: HashMap<&str, FlagHashValue<&str, &[u8]>> = HashMap::new();
    expected.insert("foo", FlagHashValue::Value(wrap(Value::IV(1))));
    expected.insert("bar", FlagHashValue::ReadOnly(wrap(Value::IV(2))));
    expected.insert("baz", FlagHashValue::Placeholder);
    assert_eq!(result, wrap(Value::FlagHash(expected)));
}

#[test]
fn thaw_array_with_ref() {
    static DATA: [u8; 13] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x01, 0x04, 0x0a, 0x03, 0x66, 0x6f, 0x6f,
    ];

    assert_eq!(
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error"),
        wrap(Value::Array(vec![wrap(Value::Ref(wrap(Value::String(
            "foo"
        ))))]))
    );
}

#[test]
fn thaw_array_with_double_ref() {
    static DATA: [u8; 19] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x02, 0x04, 0x0a, 0x03, 0x66, 0x6f, 0x6f, 0x04, 0x00,
        0x00, 0x00, 0x00, 0x02,
    ];

    let foo = wrap(Value::Ref(wrap(Value::String("foo"))));
    let res = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    let res_b = res.borrow();
    let arr = if let Value::Array(arr) = &*res_b {
        arr
    } else {
        panic!("didn't get array")
    };

    assert_eq!(*arr, vec![foo.clone(), foo.clone()]);

    let arr0_b = arr[0].borrow();
    let ref0_ptr = if let Value::Ref(rv) = &*arr0_b {
        rv
    } else {
        panic!("next slot wasn't a ref")
    };

    let arr1_b = arr[1].borrow();
    let ref1_ptr = if let Value::Ref(rv) = &*arr1_b {
        rv
    } else {
        panic!("next slot wasn't a ref")
    };

    assert!(Rc::ptr_eq(&ref0_ptr, &ref1_ptr));
}

#[test]
fn thaw_array_with_ref_to_self() {
    static DATA: [u8; 15] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x02, 0x08, 0x81, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    let err =
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect_err("got success");
    if let ThawError::WouldLeak = err {
        // good
    } else {
        panic!("got a different error {:?}", err);
    }
}

#[test]
fn thaw_array_with_weak_ref_to_self() {
    static DATA: [u8; 15] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x02, 0x08, 0x81, 0x1b, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    let res = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    let res_b = res.borrow();
    let arr = if let Value::Array(arr) = &*res_b {
        arr
    } else {
        panic!("didn't get array")
    };

    assert_eq!(arr.len(), 2);
    assert_eq!(arr[0], wrap(Value::IV(1)));

    let arr1_b = arr[1].borrow();
    let ref_ptr = if let Value::WeakRef(rv) = &*arr1_b {
        rv
    } else {
        panic!("next slot wasn't a weak ref")
    };
    assert!(Rc::ptr_eq(
        &res,
        &ref_ptr.upgrade().expect("weak ref missing")
    ));
}

#[test]
fn thaw_tied_scalar() {
    static DATA: [u8; 21] = [
        0x05, 0x0b, 0x0d, 0x04, 0x11, 0x0e, 0x54, 0x69, 0x65, 0x3a, 0x3a, 0x53, 0x74, 0x64, 0x53,
        0x63, 0x61, 0x6c, 0x61, 0x72, 0x05,
    ];

    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::TiedScalar(wrap(Value::Undef), "Tie::StdScalar"))
    );
}

#[test]
fn thaw_tied_array() {
    static DATA: [u8; 24] = [
        0x05, 0x0b, 0x0b, 0x04, 0x11, 0x0d, 0x54, 0x69, 0x65, 0x3a, 0x3a, 0x53, 0x74, 0x64, 0x41,
        0x72, 0x72, 0x61, 0x79, 0x02, 0x00, 0x00, 0x00, 0x00,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::TiedArray(
            wrap(Value::Array(Vec::new())),
            "Tie::StdArray"
        ))
    );
}

#[test]
fn thaw_tied_array_element() {
    static DATA: [u8; 28] = [
        0x05, 0x0b, 0x16, 0x04, 0x11, 0x0d, 0x54, 0x69, 0x65, 0x3a, 0x3a, 0x53, 0x74, 0x64, 0x41,
        0x72, 0x72, 0x61, 0x79, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::TiedArrayIdx(
            wrap(Value::Array(Vec::new())),
            "Tie::StdArray",
            0
        ))
    );
}

#[test]
fn thaw_tied_hash() {
    static DATA: [u8; 23] = [
        0x05, 0x0b, 0x0c, 0x04, 0x11, 0x0c, 0x54, 0x69, 0x65, 0x3a, 0x3a, 0x53, 0x74, 0x64, 0x48,
        0x61, 0x73, 0x68, 0x03, 0x00, 0x00, 0x00, 0x00,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::TiedHash(
            wrap(Value::Hash(HashMap::new())),
            "Tie::StdHash"
        ))
    );
}

#[test]
fn thaw_tied_hash_key() {
    static DATA: [u8; 28] = [
        0x05, 0x0b, 0x15, 0x04, 0x11, 0x0c, 0x54, 0x69, 0x65, 0x3a, 0x3a, 0x53, 0x74, 0x64, 0x48,
        0x61, 0x73, 0x68, 0x03, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x03, 0x66, 0x6f, 0x6f,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::TiedHashKey(
            wrap(Value::Hash(HashMap::new())),
            "Tie::StdHash",
            wrap(Value::String("foo"))
        ))
    );
}

#[test]
fn thaw_two_blessed() {
    static DATA: [u8; 30] = [
        0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x02, 0x04, 0x11, 0x03, 0x46, 0x6f, 0x6f, 0x02, 0x00,
        0x00, 0x00, 0x01, 0x08, 0x81, 0x04, 0x12, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x08, 0x82,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::Array(vec![
            wrap(Value::Ref(wrap(Value::Blessed(
                wrap(Value::Array(vec![wrap_iv(1)])),
                "Foo"
            )),)),
            wrap(Value::Ref(wrap(Value::Blessed(
                wrap(Value::Array(vec![wrap_iv(2)])),
                "Foo"
            )),))
        ]))
    )
}

#[test]
#[ignore]
fn thaw_regex() {
    static DATA: [u8; 17] = [
        0x05, 0x0b, 0x11, 0x06, 0x52, 0x65, 0x67, 0x65, 0x78, 0x70, 0x20, 0x00, 0x03, 0x66, 0x6f,
        0x6f, 0x00,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(result, wrap(Value::Undef));
}

#[test]
#[ignore]
fn thaw_sv_undef_elem() {
    // handcrafted, can't figure out how to get Perl to spit this out
    static DATA: [u8; 10] = [0x05, 0x0b, 0x02, 0x00, 0x00, 0x00, 0x02, 0x1f, 0x08, 0x81];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(result, wrap(Value::Undef));
}

#[test]
#[ignore]
fn thaw_code() {
    static DATA: [u8; 15] = [
        0x05, 0x0b, 0x1a, 0x0a, 0x0a, 0x7b, 0x0a, 0x20, 0x20, 0x20, 0x20, 0x31, 0x3b, 0x0a, 0x7d,
    ];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(result, wrap(Value::Undef));
}

#[test]
fn thaw_vstring() {
    static DATA: [u8; 14] = [
        0x05, 0x0b, 0x1d, 0x06, 0x76, 0x33, 0x2e, 0x31, 0x32, 0x37, 0x0a, 0x02, 0x03, 0x7f,
    ];
    static VSTRING: [u8; 2] = [0x03, 0x7f];
    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(
        result,
        wrap(Value::VString(
            VString::from_raw_bits("v.127", &VSTRING[..]).unwrap()
        ))
    );
}

#[test]
fn thaw_yes() {
    // handcrafted, as I can't figure out how to make Perl give this to us
    static DATA: [u8; 3] = [0x05, 0x0b, 0x0f];

    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(result, wrap(Value::Yes))
}

#[test]
fn thaw_no() {
    // handcrafted, as I can't figure out how to make Perl give this to us
    static DATA: [u8; 3] = [0x05, 0x0b, 0x10];

    let result = crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect("got error");
    assert_eq!(result, wrap(Value::No))
}

#[test]
fn thaw_native_byte_order() {
    static DATA: [u8; 24] = [
        0x04, 0x0a, 0x08, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x04, 0x08, 0x08, 0x08,
        0x06, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    let err =
        crate::thaw_from_bytes(&DATA, &ThawSettings::without_magic()).expect_err("got success");
    if let ThawError::NativeByteOrderUnsupported = err {
        // good
    } else {
        panic!("got a different error {:?}", err);
    }
}
