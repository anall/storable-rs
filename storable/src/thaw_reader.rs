use std::hash::Hash;

use crate::error::{InternalError, ThawError};
use crate::markers::TypeMarkers;
use crate::perl_value::{FlagHashValue, Value, ValueRc};
use crate::thaw_settings::ThawSettings;
use crate::{constants, vstring};

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::rc::Rc;

/// Helper to read and thaw either owned or borrowed data.
///
/// * `ST` typically being string-like such as `&'a str` or `String`
/// * `BT` typically being byte-like such as `&'a [u8]` or `Vec<u8>`
pub trait ThawReader<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
>
{
    /// Thaw the value using this `ThawReader` and the provided `ThawSettings`.
    ///
    /// This should not be overridden, and *can not* be usefully overridden.
    ///
    /// # Errors
    /// Returns error if:
    ///   * IO fails
    ///   * Any of the specified size limits are exceeded
    ///   * Invalid or unsupported Storable data encountered
    ///   * The specified Storable data would cause a Rust-side reference leak.
    fn thaw(&mut self, settings: &ThawSettings) -> Result<ValueRc<ST, BT>, ThawError>
    where
        Self: Sized,
    {
        let mut ctx = ThawContext::create_context(self, settings)?;
        ctx.retrieve(None, false)
    }

    /// Read and return a single byte as a `u8`
    ///
    /// # Errors
    /// Returns error if IO fails
    fn read_single_byte(&mut self) -> Result<u8, ThawError>;

    /// Read a short number of bytes (given by the length of `buf`) and return it inside `buf`
    ///
    /// # Errors
    /// Returns error if IO fails
    fn read_short_exact(&mut self, buf: &mut [u8]) -> Result<(), ThawError>;

    /// return a `st` or a clone of `st` depending on if `st` is owned.
    fn possibly_clone_st(st: &ST) -> ST;

    /// convert a `ST` into a `BT`
    fn st_into_bt(st: ST) -> BT;

    /// check  if `BT` contains only low-ASCII (values under 0x80)
    ///
    /// This can be overridden if your `BT` type has a better method of doing this.
    fn low_ascii_only(&mut self, bytes: &BT) -> bool {
        bytes.as_ref().iter().find(|&&ch| ch >= 128).is_none()
    }

    /// Treat `BT` as UTF-8 encoded bytes, and attempt to convert it into a `ST`
    ///
    /// # Errors
    /// Returns the original input bytes back if the provided value isn't valid UTF-8.
    fn bytes_from_utf8(&mut self, bytes: BT) -> Result<ST, BT>;

    /// Read a non-trivial amount of bytes, and return them as a `BT`
    /// This is unchecked as this is after the length restriction checks.
    ///
    /// # Errors
    /// Returns error if IO fails
    fn read_bytes_unchecked(&mut self, n: usize) -> Result<BT, ThawError>;
}

#[allow(dead_code)]
#[allow(clippy::struct_excessive_bools)]
struct ThawContext<
    'a,
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
    RDR: ThawReader<ST, BT>,
> {
    reader: &'a mut RDR,

    network_order: bool,
    version_major: u8,
    version_minor: u8,

    seen: Vec<ValueRc<ST, BT>>,
    seen_bless: Vec<ST>,

    allowed_remaining_string_bytes: usize,
    allowed_remaining_array_elements: usize,
    allowed_remaining_hash_keys: usize,

    max_unit_string_bytes: usize,
    max_unit_array_elements: usize,
    max_unit_hash_keys: usize,

    remaining_depth: usize,

    allow_byte_hashes: bool,
    upgrade_unflagged_utf8: bool,
    allow_blessed: bool,
    allow_tied: bool,
    downgrade_restricted_hashes: bool,
    strip_refs: bool,
}
impl<
        'a,
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
        RDR: ThawReader<ST, BT>,
    > ThawContext<'a, ST, BT, RDR>
{
    #[allow(clippy::unit_arg)]
    #[allow(clippy::unnecessary_wraps)]
    fn wrap_and_stash_ok(&mut self, to_stash: Value<ST, BT>) -> Result<ValueRc<ST, BT>, ThawError> {
        let pv_result = Value::wrap(to_stash);
        self.stash(&pv_result);
        Ok(pv_result)
    }

    #[allow(clippy::unit_arg)]
    fn stash(&mut self, to_stash: &ValueRc<ST, BT>) {
        self.seen.push(to_stash.clone())
    }

    #[doc(hidden)]
    fn read_i32(&mut self) -> Result<i32, ThawError> {
        let mut arr = [0_u8; 4];
        self.reader.read_short_exact(&mut arr)?;

        if self.network_order {
            Ok(i32::from_be_bytes(arr))
        } else {
            Ok(i32::from_ne_bytes(arr))
        }
    }

    #[doc(hidden)]
    fn read_u32(&mut self) -> Result<u32, ThawError> {
        let mut arr = [0_u8; 4];
        self.reader.read_short_exact(&mut arr)?;

        if self.network_order {
            Ok(u32::from_be_bytes(arr))
        } else {
            Ok(u32::from_ne_bytes(arr))
        }
    }

    #[doc(hidden)]
    fn read_bytes(&mut self, n: usize) -> Result<BT, ThawError> {
        if n > self.max_unit_string_bytes {
            return Err(ThawError::UnitSizeExceeded);
        }

        if let Some(v) = self.allowed_remaining_string_bytes.checked_sub(n) {
            self.allowed_remaining_string_bytes = v;
        } else {
            return Err(ThawError::DeserializeSizeExceeded);
        }

        self.reader.read_bytes_unchecked(n)
    }

    #[doc(hidden)]
    fn read_u32_and_bytes(&mut self) -> Result<BT, ThawError> {
        let len = self.read_u32()?;
        self.read_bytes(len as usize)
    }

    #[doc(hidden)]
    fn read_utf8_string(&mut self, n: usize) -> Result<ST, ThawError> {
        let buf = self.read_bytes(n)?;
        self.reader
            .bytes_from_utf8(buf)
            .map_err(|_| ThawError::InvalidUtf8)
    }

    #[doc(hidden)]
    fn scalar_or_bytes(&mut self, flagged: bool, buf: BT) -> Value<ST, BT> {
        if self.upgrade_unflagged_utf8 || flagged || self.reader.low_ascii_only(&buf) {
            match self.reader.bytes_from_utf8(buf) {
                Ok(str) => Value::String(str),
                Err(err) => Value::Bytes(err),
            }
        } else {
            Value::Bytes(buf)
        }
    }

    #[doc(hidden)]
    fn create_context(
        reader: &'a mut RDR,
        settings: &ThawSettings,
    ) -> Result<ThawContext<'a, ST, BT, RDR>, ThawError> {
        let network_order_byte: u8;

        if settings.with_magic {
            let mut buffer = [0_u8; constants::MAGICSTR.len() + 1];
            reader.read_short_exact(&mut buffer)?;

            if buffer[0..constants::MAGICSTR.len()] == constants::MAGICSTR {
                println!("got new magic?");
                network_order_byte = buffer[constants::MAGICSTR.len()]; // we want the character right after the magic str
            } else {
                return Err(ThawError::OldFormatUnsupported);
            }
        } else {
            network_order_byte = reader.read_single_byte()?;
        }

        let version_major = network_order_byte >> 1;
        if version_major == 0 {
            return Err(ThawError::OldFormatUnsupported);
        }

        let version_minor = if version_major > 1 {
            reader.read_single_byte()?
        } else {
            0_u8
        };

        // FIXME: handle "accept_future_minor"
        if version_major > constants::STORABLE_BIN_MAJOR
            || (version_major == constants::STORABLE_BIN_MAJOR
                && version_minor > constants::STORABLE_BIN_MINOR)
        {
            return Err(ThawError::MoreRecent(version_major, version_minor));
        }

        let network_order = network_order_byte & 0x1 == 0x1;

        if !network_order {
            return Err(ThawError::NativeByteOrderUnsupported);
        }

        Ok(ThawContext {
            reader,
            network_order,
            version_major,
            version_minor,

            seen: Vec::new(),
            seen_bless: Vec::new(),

            allowed_remaining_string_bytes: settings.allowed_remaining_string_bytes,
            allowed_remaining_array_elements: settings.allowed_remaining_array_elements,
            allowed_remaining_hash_keys: settings.allowed_remaining_hash_keys,

            max_unit_string_bytes: settings.max_unit_string_bytes,
            max_unit_array_elements: settings.max_unit_array_elements,
            max_unit_hash_keys: settings.max_unit_hash_keys,

            remaining_depth: settings.max_depth,

            allow_byte_hashes: settings.allow_byte_hashes,
            upgrade_unflagged_utf8: settings.upgrade_unflagged_utf8,
            allow_blessed: true,
            allow_tied: true,
            downgrade_restricted_hashes: settings.downgrade_restricted_hashes,
            strip_refs: settings.strip_refs,
        })
    }

    #[doc(hidden)]
    fn unwrap_ref_blessed<
        E: FnOnce() -> ThawError,
        R,
        F: FnOnce(ValueRc<ST, BT>, ST) -> Result<R, ThawError>,
    >(
        in_value: &ValueRc<ST, BT>,
        err: E,
        f: F,
    ) -> Result<R, ThawError> {
        let in_value_b = in_value.borrow();
        if let Value::Ref(ref_value) = &*in_value_b {
            let ref_value_b = ref_value.borrow();
            if let Value::Blessed(inner, class) = &*ref_value_b {
                f(inner.clone(), RDR::possibly_clone_st(class))
            } else {
                Err(err())
            }
        } else {
            Err(err())
        }
    }

    #[doc(hidden)]
    fn read_hash_key(&mut self, flagged: bool) -> Result<ST, InternalError<BT>> {
        let bytes = self.read_u32_and_bytes()?;

        if self.upgrade_unflagged_utf8 || flagged || self.reader.low_ascii_only(&bytes) {
            match self.reader.bytes_from_utf8(bytes) {
                Ok(str) => Ok(str),
                Err(err) => Err(InternalError::InvalidUtf8(err)),
            }
        } else {
            Err(InternalError::InvalidUtf8(bytes))
        }
    }

    #[doc(hidden)]
    fn retrieve_hash(&mut self, is_flag_hash: bool) -> Result<ValueRc<ST, BT>, ThawError> {
        let hash_flag = if is_flag_hash {
            self.reader.read_single_byte()?
        } else {
            0
        };
        let length = self.read_u32()? as usize;

        if let Some(v) = self.allowed_remaining_hash_keys.checked_sub(length) {
            self.allowed_remaining_hash_keys = v;
        } else {
            return Err(ThawError::DeserializeSizeExceeded);
        }

        if length > self.max_unit_hash_keys {
            return Err(ThawError::DeserializeSizeExceeded);
        }

        let pv_result = Value::wrap(Value::Dummy);
        self.stash(&pv_result);

        if hash_flag & constants::SHV_RESTRICTED == constants::SHV_RESTRICTED
            && !self.downgrade_restricted_hashes
        {
            self.retrieve_restricted_hash(length, is_flag_hash, pv_result)
        } else {
            self.retrieve_normal_hash(length, is_flag_hash, pv_result)
        }
    }

    #[doc(hidden)]
    fn retrieve_normal_hash(
        &mut self,
        length: usize,
        is_flag_hash: bool,
        pv_result: ValueRc<ST, BT>,
    ) -> Result<ValueRc<ST, BT>, ThawError> {
        let mut result: HashMap<ST, ValueRc<ST, BT>> = HashMap::with_capacity(length);
        let mut downgraded_at: Option<(usize, BT, ValueRc<ST, BT>)> = None;
        for i in 0..length {
            let obj = self.retrieve(None, false)?;

            let flag = if is_flag_hash {
                self.reader.read_single_byte()?
            } else {
                0
            };
            if flag & constants::SHV_K_ISSV == constants::SHV_K_ISSV {
                return Err(ThawError::SvKeysUnsupported);
            }
            if flag & (constants::SHV_K_LOCKED | constants::SHV_K_PLACHOLDER) != 0
                && !self.downgrade_restricted_hashes
            {
                return Err(ThawError::InvalidHash);
            }
            let flagged_key = flag & (constants::SHV_K_UTF8 | constants::SHV_K_WASUTF8) != 0;
            match self.read_hash_key(flagged_key) {
                Ok(key) => result.insert(key, obj),
                Err(InternalError::InvalidUtf8(bytes))
                    if self.allow_byte_hashes && !flagged_key =>
                {
                    downgraded_at = Some((i, bytes, obj));
                    break;
                }
                Err(InternalError::InvalidUtf8(_)) => return Err(ThawError::InvalidUtf8),
                Err(InternalError::Public(err)) => return Err(err),
            };
        }
        if let Some((pos, key, value)) = downgraded_at {
            let mut result: HashMap<BT, ValueRc<ST, BT>> = result
                .into_iter()
                .map(|(k, v)| (RDR::st_into_bt(k), v))
                .collect();
            result.insert(key, value);
            for _ in pos + 1..length {
                let obj = self.retrieve(None, false)?;

                let flag = if is_flag_hash {
                    self.reader.read_single_byte()?
                } else {
                    0
                };
                if flag & constants::SHV_K_ISSV == constants::SHV_K_ISSV {
                    return Err(ThawError::SvKeysUnsupported);
                }
                if flag & (constants::SHV_K_LOCKED | constants::SHV_K_PLACHOLDER) != 0 {
                    return Err(ThawError::InvalidHash);
                }
                result.insert(self.read_u32_and_bytes()?, obj);
            }

            pv_result.replace(Value::HashByte(result));
        } else {
            pv_result.replace(Value::Hash(result));
        }
        Ok(pv_result)
    }

    #[doc(hidden)]
    fn retrieve_restricted_hash(
        &mut self,
        length: usize,
        is_flag_hash: bool,
        pv_result: ValueRc<ST, BT>,
    ) -> Result<ValueRc<ST, BT>, ThawError> {
        let mut result: HashMap<ST, FlagHashValue<ST, BT>> = HashMap::with_capacity(length);
        let mut downgraded_at: Option<(usize, BT, FlagHashValue<ST, BT>)> = None;
        for i in 0..length {
            let obj = self.retrieve(None, false)?;

            let flag = if is_flag_hash {
                self.reader.read_single_byte()?
            } else {
                0
            };
            if flag & constants::SHV_K_ISSV == constants::SHV_K_ISSV {
                return Err(ThawError::SvKeysUnsupported);
            }

            let restrict = flag & (constants::SHV_K_LOCKED | constants::SHV_K_PLACHOLDER);
            let obj = if restrict == constants::SHV_K_LOCKED {
                FlagHashValue::ReadOnly(obj)
            } else if restrict & constants::SHV_K_PLACHOLDER == constants::SHV_K_PLACHOLDER {
                FlagHashValue::Placeholder
            } else {
                FlagHashValue::Value(obj)
            };

            let flagged_key = flag & (constants::SHV_K_UTF8 | constants::SHV_K_WASUTF8) != 0;
            match self.read_hash_key(flagged_key) {
                Ok(key) => result.insert(key, obj),
                Err(InternalError::InvalidUtf8(bytes))
                    if self.allow_byte_hashes && !flagged_key =>
                {
                    downgraded_at = Some((i, bytes, obj));
                    break;
                }
                Err(InternalError::InvalidUtf8(_)) => return Err(ThawError::InvalidUtf8),
                Err(InternalError::Public(err)) => return Err(err),
            };
        }
        if let Some((pos, key, value)) = downgraded_at {
            let mut result: HashMap<BT, FlagHashValue<ST, BT>> = result
                .into_iter()
                .map(|(k, v)| (RDR::st_into_bt(k), v))
                .collect();
            result.insert(key, value);
            for _ in pos + 1..length {
                let obj = self.retrieve(None, false)?;

                let flag = if is_flag_hash {
                    self.reader.read_single_byte()?
                } else {
                    0
                };
                if flag & constants::SHV_K_ISSV == constants::SHV_K_ISSV {
                    return Err(ThawError::SvKeysUnsupported);
                }

                let restrict = flag & (constants::SHV_K_LOCKED | constants::SHV_K_PLACHOLDER);
                let obj = if restrict == constants::SHV_K_LOCKED {
                    FlagHashValue::ReadOnly(obj)
                } else if restrict & constants::SHV_K_PLACHOLDER == constants::SHV_K_PLACHOLDER {
                    FlagHashValue::Placeholder
                } else {
                    FlagHashValue::Value(obj)
                };

                result.insert(self.read_u32_and_bytes()?, obj);
            }

            pv_result.replace(Value::FlagHashByte(result));
        } else {
            pv_result.replace(Value::FlagHash(result));
        }
        Ok(pv_result)
    }

    fn vstring_with_buf(&mut self, str_buf: ST) -> Result<ValueRc<ST, BT>, ThawError> {
        let inner_type = TypeMarkers::try_from(self.reader.read_single_byte()?)?;
        let result = match inner_type {
            TypeMarkers::SxScalar | TypeMarkers::SxUtf8Str => {
                let length = self.reader.read_single_byte()?;

                self.read_bytes(length as usize)
            }

            TypeMarkers::SxLScalar | TypeMarkers::SxLUtf8Str => self.read_u32_and_bytes(),

            _ => Err(ThawError::UnsupportedMarker(
                "got unknown marker in vstring",
            )),
        }?;

        self.wrap_and_stash_ok(Value::VString(vstring::VString::from_raw_bits(
            str_buf, result,
        )?))
    }

    #[allow(clippy::unit_arg)]
    #[allow(clippy::too_many_lines)]
    #[doc(hidden)]
    fn retrieve(
        &mut self,
        mut cname: Option<ST>,
        leak_ok: bool,
    ) -> Result<ValueRc<ST, BT>, ThawError> {
        let marker: TypeMarkers = TypeMarkers::try_from(self.reader.read_single_byte()?)?;

        if let Some(res) = self.remaining_depth.checked_sub(1) {
            self.remaining_depth = res;
        } else {
            return Err(ThawError::MaxDepthReached);
        }

        let rv_or_err = match marker {
            TypeMarkers::SxUndef | TypeMarkers::SxSvUndef => {
                // TODO: If we ever care if Undef is immortal
                let v = self.wrap_and_stash_ok(Value::Undef)?;

                #[allow(clippy::option_if_let_else)]
                if let Some(class) = cname.take() {
                    Ok(Value::wrap(Value::Blessed(v, class)))
                } else {
                    Ok(v)
                }
            }
            TypeMarkers::SxSvYes => self.wrap_and_stash_ok(Value::Yes),
            TypeMarkers::SxSvNo => self.wrap_and_stash_ok(Value::No),

            TypeMarkers::SxScalar => {
                let length = self.reader.read_single_byte()?;

                let buf = self.read_bytes(length as usize)?;
                let rv = self.scalar_or_bytes(false, buf);
                self.wrap_and_stash_ok(rv)
            }
            TypeMarkers::SxUtf8Str => {
                let length = self.reader.read_single_byte()?;

                let buf = self.read_bytes(length as usize)?;
                let rv = self.scalar_or_bytes(true, buf);
                self.wrap_and_stash_ok(rv)
            }

            TypeMarkers::SxLScalar => {
                let buf = self.read_u32_and_bytes()?;
                let rv = self.scalar_or_bytes(false, buf);
                self.wrap_and_stash_ok(rv)
            }
            TypeMarkers::SxLUtf8Str => {
                let buf = self.read_u32_and_bytes()?;
                let rv = self.scalar_or_bytes(true, buf);
                self.wrap_and_stash_ok(rv)
            }

            TypeMarkers::SxByte => {
                let pv = Value::IV(i64::from(self.reader.read_single_byte()?) - 128);
                self.wrap_and_stash_ok(pv)
            }
            TypeMarkers::SxNetInt => {
                let pv = Value::IV(i64::from(self.read_i32()?));
                self.wrap_and_stash_ok(pv)
            }

            TypeMarkers::SxArray => {
                let length = self.read_u32()? as usize;
                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);

                if length as usize > self.max_unit_array_elements {
                    return Err(ThawError::UnitSizeExceeded);
                }

                if let Some(v) = self.allowed_remaining_array_elements.checked_sub(length) {
                    self.allowed_remaining_array_elements = v;
                } else {
                    return Err(ThawError::DeserializeSizeExceeded);
                }

                let mut result: Vec<ValueRc<ST, BT>> = Vec::with_capacity(length);
                for _ in 0..length {
                    result.push(self.retrieve(None, false)?);
                }

                pv_result.replace(Value::Array(result));

                Ok(pv_result)
            }

            TypeMarkers::SxHash => self.retrieve_hash(false),
            TypeMarkers::SxFlagHash => self.retrieve_hash(true),

            TypeMarkers::SxRef => {
                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);

                let v = self.retrieve(None, false)?;

                if self.strip_refs {
                    pv_result.replace(Value::Ref(v.clone())); // We still need to stash the ref inside pv_result
                    Ok(v)
                } else {
                    pv_result.replace(Value::Ref(v));
                    Ok(pv_result)
                }
            }
            TypeMarkers::SxWeakRef => {
                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);

                let result = self.retrieve(cname.take(), true)?;
                pv_result.replace(Value::WeakRef(Rc::downgrade(&result)));

                Ok(pv_result)
            }

            TypeMarkers::SxObject => {
                let tag = self.read_i32()?;
                let v = self
                    .seen
                    .get(usize::try_from(tag).map_err(|_| ThawError::ObjectOutOfRange)?)
                    .ok_or(ThawError::ObjectOutOfRange)?
                    .clone();
                if !leak_ok {
                    let v_b = v.borrow(); // we need to borrow this for a second, if we can
                    if let Value::Dummy = &*v_b {
                        return Err(ThawError::WouldLeak);
                    }
                }
                Ok(v)
            }
            TypeMarkers::SxLObject => Err(ThawError::OverLongUnsupported),

            TypeMarkers::SxBless => {
                if !self.allow_blessed {
                    return Err(ThawError::BlessedDisabled);
                }

                let len = self.reader.read_single_byte()?;
                let class = if len & 0x80 == 0x80 {
                    let long_len = usize::try_from(self.read_i32()?)
                        .map_err(|_| ThawError::CorruptedLength)?;
                    self.read_utf8_string(long_len)?
                } else {
                    self.read_utf8_string(len as usize)?
                };

                self.seen_bless.push(RDR::possibly_clone_st(&class));

                self.retrieve(Some(class), false)
            }
            TypeMarkers::SxIxBless => {
                let short = self.reader.read_single_byte()?;
                let idx = if short & 0x80 == 0x80 {
                    usize::try_from(self.read_i32()?).map_err(|_| ThawError::CorruptedLength)?
                } else {
                    short as usize
                };
                let class = RDR::possibly_clone_st(
                    self.seen_bless
                        .get(idx)
                        .ok_or(ThawError::ObjectOutOfRange)?,
                );

                self.retrieve(Some(class), false)
            }
            TypeMarkers::SxTiedScalar => {
                if !self.allow_tied {
                    return Err(ThawError::TiedDisabled);
                }

                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);
                let sv = self.retrieve(None, false)?;

                let rv = Self::unwrap_ref_blessed(
                    &sv,
                    || ThawError::InvalidTied,
                    |v, class| Ok(Value::TiedScalar(v, class)),
                )?;
                pv_result.replace(rv);
                Ok(pv_result)
            }
            TypeMarkers::SxTiedArray => {
                if !self.allow_tied {
                    return Err(ThawError::TiedDisabled);
                }

                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);
                let sv = self.retrieve(None, false)?;

                let rv = Self::unwrap_ref_blessed(
                    &sv,
                    || ThawError::InvalidTied,
                    |v, class| Ok(Value::TiedArray(v, class)),
                )?;
                pv_result.replace(rv);
                Ok(pv_result)
            }
            TypeMarkers::SxTiedHash => {
                if !self.allow_tied {
                    return Err(ThawError::TiedDisabled);
                }

                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);
                let sv = self.retrieve(None, false)?;

                let rv = Self::unwrap_ref_blessed(
                    &sv,
                    || ThawError::InvalidTied,
                    |v, class| Ok(Value::TiedHash(v, class)),
                )?;
                pv_result.replace(rv);
                Ok(pv_result)
            }

            TypeMarkers::SxTiedIdx => {
                if !self.allow_tied {
                    return Err(ThawError::TiedDisabled);
                }

                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);
                let sv = self.retrieve(None, false)?;

                let idx =
                    usize::try_from(self.read_i32()?).map_err(|_| ThawError::CorruptedLength)?;

                let rv = Self::unwrap_ref_blessed(
                    &sv,
                    || ThawError::InvalidTied,
                    |v, class| Ok(Value::TiedArrayIdx(v, class, idx)),
                )?;
                pv_result.replace(rv);

                Ok(pv_result)
            }

            TypeMarkers::SxTiedKey => {
                if !self.allow_tied {
                    return Err(ThawError::TiedDisabled);
                }

                let pv_result = Value::wrap(Value::Dummy);
                self.stash(&pv_result);
                let sv = self.retrieve(None, false)?;

                let key = self.retrieve(None, false)?;

                let rv = Self::unwrap_ref_blessed(
                    &sv,
                    || ThawError::InvalidTied,
                    |v, class| Ok(Value::TiedHashKey(v, class, key)),
                )?;
                pv_result.replace(rv);

                Ok(pv_result)
            }

            TypeMarkers::SxSvUndefElem => Err(ThawError::UnsupportedMarker("SvUndefElem")),
            TypeMarkers::SxRegexp => Err(ThawError::UnsupportedMarker("Regexp")),
            TypeMarkers::SxCode => Err(ThawError::UnsupportedMarker("Code")),

            TypeMarkers::SxVString => {
                let length = self.reader.read_single_byte()?;

                let buf = self.read_utf8_string(length as usize)?;
                self.vstring_with_buf(buf)
            }
            TypeMarkers::SxLVString => Err(ThawError::UnsupportedMarker("LVString")),

            // Storable 3.15 will output these as strings
            TypeMarkers::SxInteger => Err(ThawError::GotNonNetOrderMarker("Integer")),
            TypeMarkers::SxDouble => Err(ThawError::GotNonNetOrderMarker("Double")),

            // These have not been supported yet (or can't get Perl to spit one out)
            TypeMarkers::SxHook => Err(ThawError::UnsupportedMarker("Hook")),
            TypeMarkers::SxOverload => Err(ThawError::UnsupportedMarker("Overload")),
            TypeMarkers::SxWeakOverload => Err(ThawError::UnsupportedMarker("WeakOverload")),
        };

        self.remaining_depth += 1;

        rv_or_err.map(|rv| {
            #[allow(clippy::option_if_let_else)]
            if let Some(class) = cname.take() {
                Value::wrap(Value::Blessed(rv, class))
            } else {
                rv
            }
        })
    }
}
