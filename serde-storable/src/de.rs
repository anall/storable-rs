use crate::error;
pub use crate::error::Error;
use crate::error::ErrorCode;
use crate::PathElement;

use serde::de::{DeserializeSeed, Unexpected, Visitor};
use serde::{de, Deserialize};
use std::cell::Ref;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use storable::{ThawSettings, Value, ValueRc};

trait STHelper<'de> {
    fn st_visit_str<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error>;
    fn st_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error>;

    fn eq_str(&self, s: &str) -> bool;
}
impl<'de> STHelper<'de> for String {
    fn st_visit_str<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_str(self)
    }

    fn st_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_bytes(self.as_bytes())
    }
    fn eq_str(&self, s: &str) -> bool {
        s == self
    }
}
impl<'de> STHelper<'de> for &'de str {
    fn st_visit_str<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_borrowed_str(*self)
    }

    fn st_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_borrowed_bytes(self.as_bytes())
    }

    fn eq_str(&self, s: &str) -> bool {
        &s == self
    }
}

trait BTHelper<'de> {
    fn bt_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error>;
}
impl<'de> BTHelper<'de> for Vec<u8> {
    fn bt_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_bytes(self)
    }
}
impl<'de> BTHelper<'de> for &'de [u8] {
    fn bt_visit_bytes<V: Visitor<'de>>(
        &self,
        v: V,
    ) -> Result<<V as Visitor<'de>>::Value, error::Error> {
        v.visit_borrowed_bytes(self)
    }
}

fn make_unexpected<
    'de,
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
>(
    pv: &Value<ST, BT>,
) -> Unexpected {
    match pv {
        Value::Dummy => Unexpected::Other("internal dummy"),
        Value::Undef(_) => Unexpected::Other("undef"),
        Value::Yes => Unexpected::Other("immortal yes"),
        Value::No => Unexpected::Other("immortal no"),
        Value::Blessed(_, _) => Unexpected::Other("blessed value"),
        Value::String(st, _) => Unexpected::Str(st.as_ref()),
        Value::VString(_) => Unexpected::Other("perl vstring"),
        Value::Bytes(bt) => Unexpected::Bytes(bt.as_ref()),
        Value::Array(_) => Unexpected::Seq,
        Value::Hash(_) => Unexpected::Map,
        Value::FlagHash(_) => Unexpected::Other("restricted hash"),
        Value::HashByte(_) => Unexpected::Other("byte-keyed hash"),
        Value::FlagHashByte(_) => Unexpected::Other("restricted byte-keyed hash"),
        Value::Ref(_) => Unexpected::Other("ref"),
        Value::WeakRef(_) => Unexpected::Other("weak ref"),
        Value::Overload(_) => Unexpected::Other("overloaded value"),
        Value::TiedScalar(_, _) => Unexpected::Other("tied scalar"),
        Value::TiedArray(_, _) => Unexpected::Other("tied array"),
        Value::TiedArrayIdx(_, _, _) => Unexpected::Other("tied array index"),
        Value::TiedHash(_, _) => Unexpected::Other("tied hash"),
        Value::TiedHashKey(_, _, _) => Unexpected::Other("tied hash value"),
        Value::IV(v) => Unexpected::Signed(*v),
        Value::UV(v) => Unexpected::Unsigned(*v),
    }
}
struct SeqAccess<
    'a,
    'de,
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
> {
    arr: &'a Vec<ValueRc<ST, BT>>,
    i: usize,
    de: &'a Deserializer<'de, ST, BT>,
}
impl<
        'a,
        'de,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > SeqAccess<'a, 'de, ST, BT>
{
    fn new(de: &'a Deserializer<'de, ST, BT>, arr: &'a Vec<ValueRc<ST, BT>>) -> Self {
        Self { arr, i: 0, de }
    }
}
impl<
        'a,
        'de,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > de::SeqAccess<'de> for SeqAccess<'a, 'de, ST, BT>
{
    type Error = error::Error;

    fn next_element_seed<T>(
        &mut self,
        seed: T,
    ) -> Result<Option<<T as DeserializeSeed<'de>>::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.i >= self.arr.len() {
            return Ok(None);
        }

        let mut de = self.de.next_index(self.i, &self.arr[self.i]);
        let res = seed.deserialize(&mut de);
        self.i += 1;
        de.fix_error(res).map(|v| Some(v))
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.arr.len() - self.i)
    }
}

struct MapAccess<
    'a,
    'de,
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
> {
    iter: std::collections::hash_map::Iter<'a, ST, ValueRc<ST, BT>>,
    cur_de: Option<Deserializer<'de, ST, BT>>,
    rem: usize,
    de: &'a Deserializer<'de, ST, BT>,
}
impl<
        'a,
        'de,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > MapAccess<'a, 'de, ST, BT>
{
    fn new(de: &'a Deserializer<'de, ST, BT>, map: &'a HashMap<ST, ValueRc<ST, BT>>) -> Self {
        Self {
            iter: map.iter(),
            rem: map.len(),
            cur_de: None,
            de,
        }
    }
}
impl<
        'a,
        'de,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > de::MapAccess<'de> for MapAccess<'a, 'de, ST, BT>
{
    type Error = error::Error;

    fn next_key_seed<K>(
        &mut self,
        seed: K,
    ) -> Result<Option<<K as DeserializeSeed<'de>>::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((key, value)) = self.iter.next() {
            let cur_de = self.de.next_key(key.as_ref(), &value);
            let rv = cur_de.fix_error(
                seed.deserialize(&mut KeyDeserializer(key, &cur_de.path, PhantomData))
                    .map(|v| Some(v)),
            );
            self.cur_de = Some(cur_de);
            rv
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(
        &mut self,
        seed: V,
    ) -> Result<<V as DeserializeSeed<'de>>::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let mut cur_de = self.cur_de.take().expect("called out of order");
        let res = seed.deserialize(&mut cur_de);
        cur_de.fix_error(res)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.rem)
    }
}

macro_rules! deserialize_key_invalid {
    ($method:ident) => {
        fn $method<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            Err( error::Error::new( de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor), self.1 ) )
        }
    };
    ($method:ident, $( $rest:ident ),+) => {
        deserialize_key_invalid!($method);
        deserialize_key_invalid!( $($rest),+ );
    };
}

struct KeyDeserializer<'a, 'de, ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>>(
    &'a ST,
    &'a Rc<PathElement>,
    PhantomData<&'de str>,
);
impl<'de, 'a, ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>> de::Deserializer<'de>
    for &'a mut KeyDeserializer<'a, 'de, ST>
{
    type Error = error::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0
            .st_visit_str(visitor)
            .map_err(|e| e.with_path(self.1))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0
            .st_visit_str(visitor)
            .map_err(|e| e.with_path(self.1))
    }

    fn deserialize_identifier<V>(
        self,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0
            .st_visit_str(visitor)
            .map_err(|e| e.with_path(self.1))
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0
            .st_visit_str(visitor)
            .map_err(|e| e.with_path(self.1))
    }

    // and all of these should error out
    deserialize_key_invalid!(
        deserialize_bool,
        deserialize_i8,
        deserialize_i16,
        deserialize_i32,
        deserialize_i64,
        deserialize_u8,
        deserialize_u16,
        deserialize_u32,
        deserialize_u64,
        deserialize_f32,
        deserialize_f64,
        deserialize_char,
        deserialize_bytes,
        deserialize_byte_buf,
        deserialize_option,
        deserialize_unit,
        deserialize_seq,
        deserialize_map,
        deserialize_ignored_any
    );

    // these can't be in the above macro due to extra arguments
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }

    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(error::Error::new(
            de::Error::invalid_type(Unexpected::Str(self.0.as_ref()), &visitor),
            self.1,
        ))
    }
}

macro_rules! tri_code {
    ($self:ident, $e:expr) => {
        match $e {
            Ok(val) => Ok(val),
            Err(err) => $self.fix_error(Err(error::Error::from(err))),
        }
    };
}

struct Deserializer<
    'de,
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
> {
    cur: ValueRc<ST, BT>,
    path: Rc<PathElement>,
    phantom: PhantomData<&'de str>,
}

impl<
        'de,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > Deserializer<'de, ST, BT>
{
    fn next_index<'a>(&self, idx: usize, pv: &ValueRc<ST, BT>) -> Deserializer<'de, ST, BT> {
        Deserializer {
            cur: pv.clone(),
            path: Rc::new(PathElement::Index(self.path.clone(), idx)),
            phantom: PhantomData,
        }
    }
    fn next_key<'a>(&self, key: &str, pv: &ValueRc<ST, BT>) -> Deserializer<'de, ST, BT> {
        Deserializer {
            cur: pv.clone(),
            path: Rc::new(PathElement::Key(self.path.clone(), key.to_string())),
            phantom: PhantomData,
        }
    }

    fn borrow_cur(&self) -> error::Result<Ref<Value<ST, BT>>> {
        let v = self.cur.borrow();
        match v.deref() {
            Value::Dummy => Err(error::Error::new(
                ErrorCode::InternalException("got internal temporary marker"),
                &self.path,
            )),
            _ => Ok(v),
        }
    }

    fn deserialize_number<V>(&self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => visitor.visit_i64(tri_code!(self, str::parse(v.as_ref()))?),
            Value::Yes => visitor.visit_i64(1),
            Value::No => visitor.visit_i64(0),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn fix_error<T>(&self, rv: error::Result<T>) -> error::Result<T> {
        match rv {
            Ok(val) => Ok(val),
            Err(err) => Err(err.with_path(&self.path)),
        }
    }
}

macro_rules! deserialize_number {
    ($method:ident) => {
        fn $method<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            self.deserialize_number(visitor)
        }
    };
    ($method:ident, $( $rest:ident ),+) => {
        deserialize_number!($method);
        deserialize_number!( $($rest),+ );
    };
}

impl<
        'de,
        'a,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug + STHelper<'de>,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug + BTHelper<'de>,
    > de::Deserializer<'de> for &'a mut Deserializer<'de, ST, BT>
{
    type Error = error::Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    deserialize_number!(
        deserialize_i8,
        deserialize_i16,
        deserialize_i32,
        deserialize_i64
    );
    deserialize_number!(
        deserialize_u8,
        deserialize_u16,
        deserialize_u32,
        deserialize_u64
    );

    fn deserialize_f32<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => visitor.visit_f32(tri_code!(self, str::parse(v.as_ref()))?),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => visitor.visit_f64(tri_code!(self, str::parse(v.as_ref()))?),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::String(v, _) => v.st_visit_str(visitor),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => visitor.visit_string(v.as_ref().to_string()),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => v.st_visit_bytes(visitor),
            Value::Bytes(v) => v.bt_visit_bytes(visitor),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::IV(v) => visitor.visit_i64(*v),
            Value::UV(v) => visitor.visit_u64(*v),
            Value::String(v, _) => v.st_visit_bytes(visitor),
            Value::Bytes(v) => v.bt_visit_bytes(visitor),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.borrow_cur()?.deref() {
            Value::Undef(_) => {
                return self.fix_error(visitor.visit_none());
            }
            Value::String(v, _) if v.eq_str("") || v.eq_str("0") => {
                return self.fix_error(visitor.visit_none());
            }
            Value::IV(v) if *v == 0 => {
                return self.fix_error(visitor.visit_none());
            }
            _ => {}
        };

        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(visitor.visit_unit())
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::Array(arr) => visitor.visit_seq(SeqAccess::new(&self, arr)),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::Hash(map) => visitor.visit_map(MapAccess::new(&self, map)),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(match self.borrow_cur()?.deref() {
            Value::Array(arr) => visitor.visit_seq(SeqAccess::new(&self, &arr)),
            Value::Hash(map) => visitor.visit_map(MapAccess::new(&self, map)),
            v => Err(de::Error::invalid_type(make_unexpected(v), &visitor)),
        })
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_identifier<V>(
        self,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(
        self,
        visitor: V,
    ) -> Result<<V as Visitor<'de>>::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.fix_error(visitor.visit_unit())
    }
}

pub fn from_bytes<'a, T: Deserialize<'a>>(data: &'a [u8]) -> error::Result<T> {
    let mut dezerializer = Deserializer {
        cur: storable::thaw_from_bytes(
            data,
            &ThawSettings::without_magic()
                .and_strip_refs()
                .and_downgrade_restricted_hashes(),
        )
        .expect("got error"),
        path: Rc::new(PathElement::Root),
        phantom: PhantomData,
    };
    T::deserialize(&mut dezerializer)
}

#[cfg(test)]
mod tests {
    use crate::de::from_bytes;
    use serde_derive::Deserialize;

    #[test]
    fn test_number() {
        assert_eq!(from_bytes::<i8>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<i8>(&[0x05, 0x0b, 0x0a, 0x01, 0x35]), Ok(5));

        assert_eq!(from_bytes::<i16>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<i32>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<i64>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));

        assert_eq!(from_bytes::<u8>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<u16>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<u32>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
        assert_eq!(from_bytes::<u64>(&[0x05, 0x0b, 0x08, 0x85]), Ok(5));
    }

    #[derive(Deserialize, Debug, Eq, PartialEq)]
    struct Triple {
        v: (u8, u16, u32),
    }
    #[test]
    fn test_seq() {
        const DATA: [u8; 24] = [
            0x05, 0x0b, 0x03, 0x00, 0x00, 0x00, 0x01, 0x04, 0x02, 0x00, 0x00, 0x00, 0x03, 0x08,
            0x81, 0x08, 0x82, 0x08, 0x83, 0x00, 0x00, 0x00, 0x01, 0x76,
        ];
        let v: Option<Triple> = from_bytes(&DATA).unwrap();
        assert_eq!(Some(Triple { v: (1, 2, 3) }), v);
    }
}
