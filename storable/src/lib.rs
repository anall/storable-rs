#[warn(clippy::pedantic)]
#[deny(unused_must_use)]
mod markers;
mod perl_value;

#[macro_use]
extern crate num_derive;

use std::{io, cmp, fmt};
use std::io::Error;
use crate::markers::{TypeMarkers,InvalidMarkerError};
pub use crate::perl_value::{PerlValue,PerlValueRc};

use std::convert::TryFrom;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::perl_value::FlagHashValue;
use std::ops::Deref;

const MAGICSTR: [u8; 4] = [b'p',b's',b't',b'0'];

const STORABLE_BIN_MAJOR : u8 = 2;
const STORABLE_BIN_MINOR : u8 = 11;

#[derive(Debug)]
pub enum ThawError {
    Io(io::Error),
    OldFormatUnsupported,
    NativeByteOrderUnsupported,
    MoreRecent(u8,u8),
    InvalidMarker(u8),
    UnsupportedMarker(String),
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
    fn from(err: Error) -> Self {
        Self::Io(err)
    }
}

impl From<InvalidMarkerError> for ThawError {
    fn from(err: InvalidMarkerError) -> Self {
        Self::InvalidMarker(err.0)
    }
}

impl From<InternalError> for ThawError {
    fn from(err: InternalError) -> Self {
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
enum InternalError {
    Public(ThawError),
    InvalidUtf8(Vec<u8>),
}
impl From<ThawError> for InternalError {
    fn from(err: ThawError) -> Self {
        Self::Public(err)
    }
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for InternalError {}

fn read_single_byte<T : io::Read>(reader : &mut T) -> io::Result<u8> {
    let mut buffer = [0_u8; 1];
    reader.read_exact(&mut buffer)?;
    Ok( buffer[0] )
}

pub struct ThawSettings {
    with_magic : bool,

    allowed_remaining_scalar_bytes : usize,
    allowed_remaining_array_elements : usize,
    allowed_remaining_hash_keys : usize,

    max_unit_scalar_bytes : usize,
    max_unit_array_elements : usize,
    max_unit_hash_keys : usize,

    max_depth : usize,

    allow_byte_hashes : bool,
    upgrade_unflagged_utf8: bool,

    strip_refs : bool,
}
impl Default for ThawSettings {
    fn default() -> Self {
        Self {
            with_magic : false,

            allowed_remaining_scalar_bytes : usize::MAX,
            allowed_remaining_array_elements : usize::MAX,
            allowed_remaining_hash_keys : usize::MAX,

            max_unit_scalar_bytes : i32::MAX as usize,
            max_unit_array_elements : i32::MAX as usize,
            max_unit_hash_keys : i32::MAX as usize,

            max_depth : 1024,

            allow_byte_hashes : false,
            upgrade_unflagged_utf8 : false,
            strip_refs : false,
        }
    }
}
impl ThawSettings {
    pub fn with_magic() -> Self {
        Self {
            with_magic: true,
            ..Default::default()
        }
    }
    pub fn without_magic() -> Self {
        Self {
            with_magic: false,
            ..Default::default()
        }
    }

    pub fn and_allowed_scalar_bytes(mut self, val : usize) -> Self {
        self.allowed_remaining_scalar_bytes = val;
        self
    }
    pub fn and_allowed_array_elements(mut self, val : usize) -> Self {
        self.allowed_remaining_array_elements = val;
        self
    }
    pub fn and_allowed_hash_keys(mut self, val : usize) -> Self {
        self.allowed_remaining_hash_keys = val;
        self
    }


    pub fn and_max_unit_scalar_bytes(mut self, val : usize) -> Self {
        self.max_unit_scalar_bytes = val;
        self
    }
    pub fn and_max_unit_array_elements(mut self, val : usize) -> Self {
        self.max_unit_array_elements = val;
        self
    }
    pub fn and_max_unit_hash_keys(mut self, val : usize) -> Self {
        self.max_unit_hash_keys = val;
        self
    }

    // ...
    pub fn and_with_byte_hashes(mut self) -> Self {
        self.allow_byte_hashes = true;
        self
    }

    pub fn and_upgrade_unflagged_utf8(mut self) -> Self {
        self.upgrade_unflagged_utf8 = true;
        self
    }

    pub fn and_strip_refs(mut self) -> Self {
        self.strip_refs = true;
        self
    }
}

const SHV_RESTRICTED : u8 = 0x01;

const SHV_K_UTF8 : u8 = 0x01;
const SHV_K_WASUTF8 : u8 = 0x02;
const SHV_K_LOCKED : u8 = 0x04;
const SHV_K_ISSV : u8 = 0x08;
const SHV_K_PLACHOLDER : u8 = 0x10;

#[allow(dead_code)]
struct ThawContext<T : io::Read> {
    network_order : bool,
    version_major : u8,
    version_minor : u8,

    reader : T,

    seen : Vec<Rc<RefCell<PerlValue>>>,

    allowed_remaining_scalar_bytes : usize,
    allowed_remaining_array_elements : usize,
    allowed_remaining_hash_keys : usize,

    max_unit_scalar_bytes : usize,
    max_unit_array_elements : usize,
    max_unit_hash_keys : usize,

    remaining_depth : usize,

    allow_byte_hashes : bool,
    upgrade_unflagged_utf8: bool,
    allow_blessed : bool,
    allow_tied : bool,
    strip_refs : bool,
}
impl<T : io::Read> ThawContext<T> {

    fn new(mut reader : T, settings : &ThawSettings) -> Result<ThawContext<T>,ThawError> {
        let mut buffer = [0_u8; (MAGICSTR.len()+255)];
        let network_order_byte : u8;

        if settings.with_magic {
            reader.read_exact(&mut buffer[0..MAGICSTR.len()])?;

            if buffer[0..MAGICSTR.len()] == MAGICSTR {
                println!("got new magic?");
                network_order_byte = buffer[MAGICSTR.len()]; // we want the character right after the magic str
            } else {
                return Err( ThawError::OldFormatUnsupported );
            }
        } else {
            network_order_byte = read_single_byte(&mut reader)?;
        }

        let version_major = network_order_byte >> 1;
        if version_major == 0 {
            return Err( ThawError::OldFormatUnsupported );
        }

        let version_minor = if version_major > 1 {
            read_single_byte(&mut reader)?
        } else { 0_u8 };

        // FIXME: handle "accept_future_minor"
        if version_major > STORABLE_BIN_MAJOR || ( version_major == STORABLE_BIN_MAJOR && version_minor > STORABLE_BIN_MINOR ) {
            return Err( ThawError::MoreRecent(version_major,version_minor) )
        }

        let network_order = network_order_byte & 0x1 == 0x1;

        if ! network_order {
            return Err( ThawError::NativeByteOrderUnsupported);
        }

        Ok( Self {
            network_order,
            version_major, version_minor,
            reader,

            seen: Vec::new(),

            allowed_remaining_scalar_bytes : settings.allowed_remaining_scalar_bytes,
            allowed_remaining_array_elements : settings.allowed_remaining_array_elements,
            allowed_remaining_hash_keys : settings.allowed_remaining_hash_keys,

            max_unit_scalar_bytes : settings.max_unit_scalar_bytes,
            max_unit_array_elements : settings.max_unit_array_elements,
            max_unit_hash_keys : settings.max_unit_hash_keys,

            remaining_depth : settings.max_depth,

            allow_byte_hashes : settings.allow_byte_hashes,
            upgrade_unflagged_utf8: settings.upgrade_unflagged_utf8,
            allow_blessed: true,
            allow_tied: true,
            strip_refs : settings.strip_refs,
        } )
    }

    // FIXME: Make generic
    fn read_i32(&mut self) -> io::Result<i32> {
        let mut buf = [0; 4];
        self.reader.read_exact(&mut buf)?;

        if self.network_order {
            Ok( i32::from_be_bytes(buf) )
        } else {
            Ok( i32::from_ne_bytes(buf) )
        }
    }

    fn read_u32(&mut self) -> io::Result<u32> {
        let mut buf = [0; 4];
        self.reader.read_exact(&mut buf)?;

        if self.network_order {
            Ok( u32::from_be_bytes(buf) )
        } else {
            Ok( u32::from_ne_bytes(buf) )
        }
    }

    #[allow(clippy::unit_arg)]
    #[allow(clippy::unnecessary_wraps)]
    fn wrap_and_stash_ok(&mut self, to_stash : PerlValue) -> Result<PerlValueRc,ThawError> {
        let pv_result = PerlValue::wrap(to_stash);
        self.stash(&pv_result);
        Ok( pv_result )
    }

    #[allow(clippy::unit_arg)]
    fn stash(&mut self, to_stash : &PerlValueRc) {
        self.seen.push(to_stash.clone())
    }

    fn read_bytes(&mut self, mut length : usize) -> Result<Vec<u8>,ThawError> {
        if length > self.max_unit_scalar_bytes {
            return Err(ThawError::UnitSizeExceeded);
        }

        if let Some(v) = self.allowed_remaining_scalar_bytes.checked_sub(length) {
            self.allowed_remaining_scalar_bytes = v;
        } else {
            return Err(ThawError::DeserializeSizeExceeded)
        }

        let mut out : Vec<u8> = Vec::with_capacity(length);
        while length > 0 {
            let read_len = cmp::min(1024,length);
            let mut buf: Vec<u8> = vec![0_u8; read_len];

            self.reader.read_exact(&mut buf)?;
            out.append(&mut buf);
            length -= read_len;
        };

        Ok( out )
    }

    fn read_u32_and_bytes(&mut self) -> Result<Vec<u8>,ThawError> {
        let length = self.read_u32()? as usize;
        self.read_bytes( length )
    }

    fn read_hash_key(&mut self, flagged : bool) -> Result<String,InternalError> {
        let bytes = self.read_u32_and_bytes()?;
        if self.upgrade_unflagged_utf8 || flagged || bytes.iter().find(|&&ch| ch >= 128).is_none() {
            String::from_utf8( bytes ).map_err(|e| InternalError::InvalidUtf8( e.into_bytes() ) )
        } else {
            Err( InternalError::InvalidUtf8( bytes ) )
        }
    }

    fn retrieve_hash(&mut self, is_flag_hash : bool) -> Result<PerlValueRc,ThawError> {
        let hash_flag = if is_flag_hash { read_single_byte(&mut self.reader)? } else { 0 };
        let length = self.read_u32()? as usize;

        if let Some(v) = self.allowed_remaining_hash_keys.checked_sub(length) {
            self.allowed_remaining_hash_keys = v;
        } else {
            return Err(ThawError::DeserializeSizeExceeded)
        }

        let pv_result = PerlValue::wrap(PerlValue::Dummy);
        self.stash(&pv_result);

        if hash_flag&SHV_RESTRICTED == SHV_RESTRICTED {
            self.retrieve_restricted_hash(length, is_flag_hash,pv_result)
        } else {
            self.retrieve_normal_hash(length, is_flag_hash,pv_result)
        }
    }

    fn retrieve_normal_hash(&mut self, length : usize, is_flag_hash : bool, pv_result : PerlValueRc) -> Result<PerlValueRc,ThawError> {
        let mut result : HashMap<String,PerlValueRc> = HashMap::with_capacity(length);
        let mut downgraded_at: Option<(usize,Vec<u8>,PerlValueRc)> = None;
        for i in 0 .. length {
            let obj = self.retrieve( None, false )?;

            let flag = if is_flag_hash { read_single_byte(&mut self.reader)? } else { 0 };
            if flag & SHV_K_ISSV == SHV_K_ISSV {
                return Err( ThawError::SvKeysUnsupported );
            }
            if flag & (SHV_K_LOCKED|SHV_K_PLACHOLDER) != 0 {
                return Err( ThawError::InvalidHash );
            }
            let flagged_key = flag & (SHV_K_UTF8|SHV_K_WASUTF8) != 0;
            match self.read_hash_key( flagged_key ) {
                Ok(key) => result.insert(key ,obj),
                Err( InternalError::InvalidUtf8(bytes ) ) if self.allow_byte_hashes && ! flagged_key => {
                    downgraded_at = Some( (i,bytes,obj) );
                    break;
                },
                Err( InternalError::InvalidUtf8(_) ) => return Err( ThawError::InvalidUtf8),
                Err( InternalError::Public(err) ) => return Err( err )
            };
        }
        if let Some( (pos,key,value) ) = downgraded_at {
            let mut result : HashMap<Vec<u8>,PerlValueRc> = result.into_iter().map(|(k,v)| (k.into_bytes(),v) ).collect();
            result.insert( key, value );
            for _ in pos+1 .. length {
                let obj = self.retrieve( None, false )?;

                let flag = if is_flag_hash { read_single_byte(&mut self.reader)? } else { 0 };
                if flag & SHV_K_ISSV == SHV_K_ISSV {
                    return Err( ThawError::SvKeysUnsupported );
                }
                if flag & (SHV_K_LOCKED|SHV_K_PLACHOLDER) != 0 {
                    return Err( ThawError::InvalidHash );
                }
                result.insert( self.read_u32_and_bytes()?, obj );
            }

            pv_result.replace( PerlValue::HashByte(result) );
        } else {
            pv_result.replace(PerlValue::Hash(result) );
        }
        Ok(pv_result)
    }

    fn retrieve_restricted_hash(&mut self, length : usize, is_flag_hash : bool, pv_result : PerlValueRc) -> Result<PerlValueRc,ThawError> {
        let mut result : HashMap<String,FlagHashValue> = HashMap::with_capacity(length);
        let mut downgraded_at: Option<(usize,Vec<u8>,FlagHashValue)> = None;
        for i in 0 .. length {
            let obj = self.retrieve( None, false )?;

            let flag = if is_flag_hash { read_single_byte(&mut self.reader)? } else { 0 };
            if flag & SHV_K_ISSV == SHV_K_ISSV {
                return Err( ThawError::SvKeysUnsupported );
            }

            let restrict = flag & (SHV_K_LOCKED|SHV_K_PLACHOLDER);
            let obj = if restrict == SHV_K_LOCKED {
                FlagHashValue::ReadOnly(obj)
            } else if restrict == SHV_K_PLACHOLDER {
                FlagHashValue::Placeholder
            } else if restrict == SHV_K_LOCKED|SHV_K_PLACHOLDER {
                FlagHashValue::Placeholder // FIXME: why are both these bits set?
            } else {
                FlagHashValue::Value(obj)
            };

            let flagged_key = flag & (SHV_K_UTF8|SHV_K_WASUTF8) != 0;
            match self.read_hash_key( flagged_key ) {
                Ok(key) => result.insert(key ,obj),
                Err( InternalError::InvalidUtf8(bytes ) ) if self.allow_byte_hashes && ! flagged_key => {
                    downgraded_at = Some( (i,bytes,obj) );
                    break;
                },
                Err( InternalError::InvalidUtf8(_) ) => return Err( ThawError::InvalidUtf8),
                Err( InternalError::Public(err) ) => return Err( err )
            };
        }
        if let Some( (pos,key,value) ) = downgraded_at {
            let mut result : HashMap<Vec<u8>,FlagHashValue> = result.into_iter().map(|(k,v)| (k.into_bytes(),v) ).collect();
            result.insert( key, value );
            for _ in pos+1 .. length {
                let obj = self.retrieve( None, false )?;

                let flag = if is_flag_hash { read_single_byte(&mut self.reader)? } else { 0 };
                if flag & SHV_K_ISSV == SHV_K_ISSV {
                    return Err( ThawError::SvKeysUnsupported );
                }

                let restrict = flag & (SHV_K_LOCKED|SHV_K_PLACHOLDER);
                let obj = if restrict == SHV_K_LOCKED {
                    FlagHashValue::ReadOnly(obj)
                } else if restrict == SHV_K_PLACHOLDER {
                    FlagHashValue::Placeholder
                } else if restrict == SHV_K_LOCKED|SHV_K_PLACHOLDER {
                    FlagHashValue::Placeholder
                } else {
                    FlagHashValue::Value(obj)
                };

                result.insert( self.read_u32_and_bytes()?, obj );
            }

            pv_result.replace( PerlValue::FlagHashByte(result) );
        } else {
            pv_result.replace(PerlValue::FlagHash(result) );
        }
        Ok(pv_result)
    }

    fn unwrap_ref_blessed<E : FnOnce() -> ThawError,R,F : FnOnce(PerlValueRc,String) -> Result<R,ThawError>>(v : PerlValueRc, err : E, f : F) -> Result<R,ThawError> {
        let v_b = v.borrow();
        if let PerlValue::Ref(rv) = v_b.deref() {
            let rv_b = rv.borrow();
            if let PerlValue::Blessed(inner, class) = rv_b.deref() {
                f(inner.clone(),class.clone())
            } else {
                Err( err() )
            }
        } else {
            Err( err() )
        }
    }

    #[allow(clippy::unit_arg)]
    fn retrieve(&mut self, mut cname : Option<&str>, leak_ok : bool) -> Result<PerlValueRc,ThawError> {
        let marker : TypeMarkers = TypeMarkers::try_from( read_single_byte(&mut self.reader)? )?;

        // FIXME: This leaks a depth if this function early-returns (mainly on errors)
        if let Some(res) = self.remaining_depth.checked_sub(1) {
            self.remaining_depth = res;
        } else {
            return Err( ThawError::MaxDepthReached );
        }

        let rv = match marker {
            TypeMarkers::SxUndef => {
                let v = self.wrap_and_stash_ok(PerlValue::Undef )?;
                if let Some(class) = cname.take() {
                    Ok( PerlValue::wrap( PerlValue::Blessed(v,class.to_string()) ) )
                } else {
                    Ok( v )
                }
            },
            TypeMarkers::SxSvUndef => {
                let v = self.wrap_and_stash_ok(PerlValue::Undef )?;
                if let Some(class) = cname.take() {
                    Ok( PerlValue::wrap( PerlValue::Blessed(v,class.to_string()) ) )
                } else {
                    Ok( v )
                }
            },

            TypeMarkers::SxScalar => {
                let length = read_single_byte(&mut self.reader)?;

                if length as usize > self.max_unit_scalar_bytes {
                    return Err(ThawError::UnitSizeExceeded);
                }

                if let Some(v) = self.allowed_remaining_scalar_bytes.checked_sub(length as usize) {
                    self.allowed_remaining_scalar_bytes = v;
                } else {
                    return Err(ThawError::DeserializeSizeExceeded)
                }
                let mut buf: Vec<u8> = vec![0_u8; length as usize];
                self.reader.read_exact(&mut buf)?;
                self.wrap_and_stash_ok(PerlValue::scalar_or_bytes(buf, false, self.upgrade_unflagged_utf8) )
            },
            TypeMarkers::SxUtf8Str => {
                let length = read_single_byte(&mut self.reader)?;

                if length as usize > self.max_unit_scalar_bytes {
                    return Err(ThawError::UnitSizeExceeded);
                }

                if let Some(v) = self.allowed_remaining_scalar_bytes.checked_sub(length as usize) {
                    self.allowed_remaining_scalar_bytes = v;
                } else {
                    return Err(ThawError::DeserializeSizeExceeded)
                }
                let mut buf: Vec<u8> = vec![0_u8; length as usize];
                self.reader.read_exact(&mut buf)?;
                self.wrap_and_stash_ok(PerlValue::scalar_or_bytes(buf, true, self.upgrade_unflagged_utf8) )
            },

            TypeMarkers::SxLScalar => {
                let buf = self.read_u32_and_bytes()?;
                self.wrap_and_stash_ok(PerlValue::scalar_or_bytes(buf, false, self.upgrade_unflagged_utf8) )
            },
            TypeMarkers::SxLUtf8Str => {
                let buf = self.read_u32_and_bytes()?;
                self.wrap_and_stash_ok(PerlValue::scalar_or_bytes(buf, true, self.upgrade_unflagged_utf8) )
            },

            TypeMarkers::SxByte => {
                let pv = PerlValue::IV( i64::from( read_single_byte(&mut self.reader)? ) - 128 );
                self.wrap_and_stash_ok(pv)
            },
            TypeMarkers::SxNetInt => {
                let pv = PerlValue::IV( i64::from( self.read_i32()? ) );
                self.wrap_and_stash_ok(pv)
            }

            TypeMarkers::SxArray => {
                let length = self.read_u32()? as usize;
                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);

                if let Some(v) = self.allowed_remaining_array_elements.checked_sub(length) {
                    self.allowed_remaining_array_elements = v;
                } else {
                    return Err(ThawError::DeserializeSizeExceeded)
                }

                let mut result : Vec<PerlValueRc> = Vec::with_capacity(length );
                for _ in 0 .. length {
                    result.push( self.retrieve( None, false )? );
                }

                pv_result.replace( PerlValue::Array(result) );

                Ok( pv_result )
            },

            TypeMarkers::SxHash => self.retrieve_hash(false),
            TypeMarkers::SxFlagHash => self.retrieve_hash(true),

            TypeMarkers::SxRef => {
                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);

                let v = self.retrieve(None, false)?;

                if self.strip_refs {
                    pv_result.replace( PerlValue::Ref(v.clone()) );
                    Ok( v )
                } else {
                    pv_result.replace( PerlValue::Ref(v) );
                    Ok( pv_result )
                }
            },
            TypeMarkers::SxWeakRef => {
                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash( &pv_result);

                let result = self.retrieve( cname, true )?;
                pv_result.replace( PerlValue::WeakRef( Rc::downgrade( &result ) ) );

                Ok( pv_result )
            },

            TypeMarkers::SxObject => {
                let tag = self.read_i32()?;
                let v = self.seen.get(usize::try_from( tag ).map_err(|_| ThawError::ObjectOutOfRange)? )
                    .ok_or(ThawError::ObjectOutOfRange)?.clone();
                if ! leak_ok {
                    let v_b = v.borrow(); // we need to borrow this for a second, if we can
                    if let PerlValue::Dummy = &*v_b {
                        return Err(ThawError::WouldLeak);
                    }
                }
                Ok( v )
            }
            TypeMarkers::SxLObject => Err(ThawError::OverLongUnsupported),

            TypeMarkers::SxBless => {
                if ! self.allow_blessed {
                    return Err( ThawError::BlessedDisabled );
                }

                let len = read_single_byte(&mut self.reader)?;
                let bytes = if len & 0x80 == 0x80 {
                    let long_len = usize::try_from( self.read_i32()? ).map_err(|_| ThawError::CorruptedLength)?;
                    self.read_bytes(long_len)?
                } else {
                    self.read_bytes( len as usize )?
                };
                let class = String::from_utf8( bytes ).map_err(|_| ThawError::InvalidUtf8 )?;

                self.retrieve(Some(&class), false)
            }
            TypeMarkers::SxTiedScalar => {
                if ! self.allow_tied {
                    return Err( ThawError::TiedDisabled );
                }

                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);
                let sv = self.retrieve( None, false )?;

                let rv = Self::unwrap_ref_blessed(sv, || ThawError::InvalidTied, |v,class| Ok( PerlValue::TiedScalar(v,class) ) )?;
                pv_result.replace( rv );
                Ok( pv_result )
            },
            TypeMarkers::SxTiedArray => {
                if ! self.allow_tied {
                    return Err( ThawError::TiedDisabled );
                }

                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);
                let sv = self.retrieve( None, false )?;

                let rv = Self::unwrap_ref_blessed(sv, || ThawError::InvalidTied, |v,class| Ok( PerlValue::TiedArray(v,class) ) )?;
                pv_result.replace( rv );
                Ok( pv_result )
            },
            TypeMarkers::SxTiedHash => {
                if ! self.allow_tied {
                    return Err( ThawError::TiedDisabled );
                }

                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);
                let sv = self.retrieve( None, false )?;

                let rv = Self::unwrap_ref_blessed(sv, || ThawError::InvalidTied, |v,class| Ok( PerlValue::TiedHash(v,class) ) )?;
                pv_result.replace( rv );
                Ok( pv_result )
            },

            TypeMarkers::SxTiedIdx => {
                if ! self.allow_tied {
                    return Err( ThawError::TiedDisabled );
                }

                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);
                let sv = self.retrieve( None, false )?;

                let idx = usize::try_from( self.read_i32()? ).map_err(|_| ThawError::CorruptedLength)?;

                let rv = Self::unwrap_ref_blessed(sv, || ThawError::InvalidTied,
                                                  |v,class| Ok( PerlValue::TiedArrayIdx(v,class,idx) ) )?;
                pv_result.replace( rv );

                Ok( pv_result )
            },

            TypeMarkers::SxTiedKey => {
                if ! self.allow_tied {
                    return Err( ThawError::TiedDisabled );
                }

                let pv_result = PerlValue::wrap( PerlValue::Dummy );
                self.stash(&pv_result);
                let sv = self.retrieve( None, false )?;

                let key = self.retrieve(None, false)?;

                let rv = Self::unwrap_ref_blessed(sv, || ThawError::InvalidTied,
                                                  |v,class| Ok( PerlValue::TiedHashKey(v,class,key) ) )?;
                pv_result.replace( rv );

                Ok( pv_result )
            },

            m => Err( ThawError::UnsupportedMarker( format!("{:?}",m) ) ),
            /*TypeMarkers::SxInteger => {}
            TypeMarkers::SxDouble => {}
            TypeMarkers::SxHook => {}
            TypeMarkers::SxOverload => {}
            TypeMarkers::SxCode => {}
            TypeMarkers::SxWeakOverload => {}
            TypeMarkers::SxVString => {}
            TypeMarkers::SxLvString => {}
            TypeMarkers::SxSvUndefElem => {}
            TypeMarkers::SxRegexp => {}
            TypeMarkers::SxLast => {}*/
        }?;

        self.remaining_depth += 1;

        if let Some(class) = cname.take() {
            Ok( PerlValue::wrap( PerlValue::Blessed(rv,class.to_string()) ) )
        } else {
            Ok( rv )
        }
    }
}

pub fn thaw<T : io::Read>(reader : T, settings : &ThawSettings) -> Result<PerlValueRc,ThawError> {
    let mut ctx = ThawContext::new(reader,settings )?;
    ctx.retrieve( None, false )
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;
    use std::io::Cursor;
    use crate::markers::TypeMarkers;
    use crate::{PerlValue, PerlValueRc, ThawError, ThawSettings};
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::ops::Deref;
    use std::collections::HashMap;
    use crate::perl_value::FlagHashValue;

    #[test]
    fn it_works() {
        assert_eq!(TypeMarkers::try_from(0).unwrap() , TypeMarkers::SxObject);
    }

    fn wrap(pv : PerlValue) -> PerlValueRc {
        Rc::new(RefCell::new( pv ))
    }

    #[test]
    fn thaw_scalar() {
        static DATA : [u8; 7] = [0x05,0x0b,0x0a,0x03,0x66,0x6f,0x6f];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Scalar( "foo".to_string() ) )
        );
    }

    #[test]
    fn thaw_unflagged_scalar() {
        static DATA : [u8; 6] = [0x05,0x0b,0x0a,0x02,0xc3,0xa6];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::ScalarBytes( vec![0xc3,0xa6] ) )
        );

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic().and_upgrade_unflagged_utf8() ).expect("it broke"),
            wrap( PerlValue::Scalar( "æ".to_string() ) )
        );
    }

    #[test]
    fn thaw_flagged_scalar() {
        static DATA : [u8; 6] = [0x05,0x0b,0x17,0x02,0xc3,0xa6];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Scalar( "æ".to_string() ) )
        );
    }

    #[test]
    fn thaw_long_scalar() {
        static DATA : [u8; 263] = [
            0x05,0x0b,0x01,0x00,0x00,0x01,0x00,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,
            0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,
            0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,
            0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,
            0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,
            0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,
            0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,
            0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,
            0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,
            0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,
            0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,
            0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,
            0x32,0x33,0x34,0x31,0x32,0x33,0x34,0x31,0x32,0x33,0x34];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Scalar( "1234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234123412341234".to_string() ) )
        );
    }

    #[test]
    fn thaw_flagged_long_scalar() {
        static DATA : [u8; 265] = [
            0x05,0x0b,0x18,0x00,0x00,0x01,0x02,0xc3,0xa6,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,
            0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,
            0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,
            0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,
            0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,
            0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,
            0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,
            0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,
            0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,
            0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,
            0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,
            0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,
            0x64,0x62,0x65,0x65,0x66,0x64,0x65,0x61,0x64,0x62,0x65,0x65,0x66];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Scalar( "ædeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef".to_string() ) )
        );
    }

    #[test]
    fn thaw_array() {
        static DATA : [u8; 12] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x01,0x0a,0x03,0x66,0x6f,0x6f];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Array( vec![ wrap(  PerlValue::Scalar( "foo".to_string() ) ) ] ) )
        );
    }

    #[test]
    fn thaw_net_integer() {
        static DATA: [u8; 7] = [0x05, 0x0b, 0x09, 0x00, 0x00, 0xff, 0xff];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::IV( 65535 ) )
        );
    }

    #[test]
    fn thaw_hash() {
        static DATA : [u8; 24] = [0x05,0x0b,0x03,0x00,0x00,0x00,0x02,0x08,0x81,0x00,0x00,0x00,0x02,0x68,0x69,0x08,0x82,0x00,0x00,0x00,0x03,0x62,0x79,0x65];

        let mut hash : HashMap<String,PerlValueRc> = HashMap::new();
        hash.insert("hi".to_string(), wrap( PerlValue::IV(1) ) );
        hash.insert("bye".to_string(), wrap( PerlValue::IV(2) ) );

        let parsed = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!( parsed, wrap( PerlValue::Hash(hash) ) );
    }

    #[test]
    fn thaw_hash_unflagged_utf8_key() {
        static DATA : [u8; 16] = [0x05,0x0b,0x03,0x00,0x00,0x00,0x01,0x0a,0x01,0x61,0x00,0x00,0x00,0x02,0xc3,0xa6];

        let err = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect_err("it broke");
        if let ThawError::InvalidUtf8 = err {
            // good
        } else {
            panic!("got a different error {:?}",err);
        }

        let settings = ThawSettings::without_magic().and_with_byte_hashes();
        let result = crate::thaw(&mut Cursor::new(DATA), &settings ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::HashByte( [(vec![0xc3,0xa6],wrap(PerlValue::Scalar("a".to_string())))].iter().cloned().collect() ) )
        );

        let settings = ThawSettings::without_magic().and_upgrade_unflagged_utf8();
        let result = crate::thaw(&mut Cursor::new(DATA), &settings ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::Hash( [("æ".to_string(),wrap(PerlValue::Scalar("a".to_string())))].iter().cloned().collect() ) )
        );
    }

    #[test]
    fn thaw_hash_flagged_utf8_key() {
        static DATA : [u8; 18] = [0x05,0x0b,0x19,0x00,0x00,0x00,0x00,0x01,0x0a,0x01,0x61,0x02,0x00,0x00,0x00,0x02,0xc3,0xa6];

        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::Hash( [("æ".to_string(),wrap(PerlValue::Scalar("a".to_string())))].iter().cloned().collect() ) )
        );
    }

    #[test]
    fn thaw_hash_bad_utf8_key() {
        static DATA : [u8; 15] = [0x05,0x0b,0x03,0x00,0x00,0x00,0x01,0x08,0x81,0x00,0x00,0x00,0x02,0xa6,0xc3];

        let err = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect_err("it broke");
        if let ThawError::InvalidUtf8 = err {
            // good
        } else {
            panic!("got a different error {:?}",err);
        }

        let settings = ThawSettings::without_magic().and_with_byte_hashes();
        let result = crate::thaw(&mut Cursor::new(DATA), &settings ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::HashByte( [(vec![166,195],wrap(PerlValue::IV(1)))].iter().cloned().collect() ) )
        );
    }

    #[test]
    fn thaw_restricted_hash() {
        static DATA : [u8; 37] = [0x05,0x0b,0x19,0x01,0x00,0x00,0x00,0x03,0x08,0x81,0x00,0x00,0x00,0x00,0x03,0x66,0x6f,0x6f,0x0e,0x14,0x00,0x00,0x00,0x03,0x62,0x61,0x7a,0x08,0x82,0x04,0x00,0x00,0x00,0x03,0x62,0x61,0x72];

        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::FlagHash( [
                ("foo".to_string(),FlagHashValue::Value(wrap(PerlValue::IV(1)))),
                ("bar".to_string(),FlagHashValue::ReadOnly(wrap(PerlValue::IV(2)))),
                ("baz".to_string(),FlagHashValue::Placeholder)
            ].iter().cloned().collect() ) )
        );
    }

    #[test]
    fn thaw_array_with_ref() {
        static DATA : [u8; 13] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x01,0x04,0x0a,0x03,0x66,0x6f,0x6f];

        assert_eq!(
            crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke"),
            wrap( PerlValue::Array( vec![ wrap( PerlValue::Ref( wrap(  PerlValue::Scalar( "foo".to_string() ) ) ) ) ] ) )
        );
    }

    #[test]
    fn thaw_array_with_double_ref() {
        static DATA : [u8; 19] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x02,0x04,0x0a,0x03,0x66,0x6f,0x6f,0x04,0x00,0x00,0x00,0x00,0x02];

        let foo = wrap( PerlValue::Ref( wrap(  PerlValue::Scalar( "foo".to_string() ) ) ) );
        let res = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        let res_b = res.borrow();
        let arr = if let PerlValue::Array(arr) = res_b.deref() { arr } else { panic!("didn't get array") };

        assert_eq!(
            *arr,
            vec![ foo.clone(), foo.clone() ]
        );

        let arr0_b = arr[0].borrow();
        let ref0_ptr = if let PerlValue::Ref( rv ) = arr0_b.deref() { rv } else { panic!("next slot wasn't a ref") };

        let arr1_b = arr[1].borrow();
        let ref1_ptr = if let PerlValue::Ref( rv ) = arr1_b.deref() { rv } else { panic!("next slot wasn't a ref") };

        assert!( Rc::ptr_eq( &ref0_ptr, &ref1_ptr ) );
    }

    #[test]
    fn thaw_array_with_ref_to_self() {
        static DATA : [u8; 15] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x02,0x08,0x81,0x04,0x00,0x00,0x00,0x00,0x00];

        let err = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect_err("it didn't break");
        if let ThawError::WouldLeak = err {
            // good
        } else {
            panic!("got a different error {:?}",err);
        }
    }

    #[test]
    fn thaw_array_with_weak_ref_to_self() {
        static DATA : [u8; 15] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x02,0x08,0x81,0x1b,0x00,0x00,0x00,0x00,0x00];

        let res = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        let res_b = res.borrow();
        let arr = if let PerlValue::Array(arr) = res_b.deref() { arr } else { panic!("didn't get array") };

        assert_eq!( arr.len(), 2 );
        assert_eq!( arr[0], wrap( PerlValue::IV(1) ) );

        let arr1_b = arr[1].borrow();
        let ref_ptr = if let PerlValue::WeakRef( rv ) = arr1_b.deref() { rv } else { panic!("next slot wasn't a weak ref") };
        assert!( Rc::ptr_eq(&res, &ref_ptr.upgrade().expect("weak ref missing") ) );
    }

    #[test]
    fn thaw_tied_scalar() {
        static DATA : [u8; 21] = [0x05,0x0b,0x0d,0x04,0x11,0x0e,0x54,0x69,0x65,0x3a,0x3a,0x53,0x74,0x64,0x53,0x63,0x61,0x6c,0x61,0x72,0x05];

        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::TiedScalar( wrap( PerlValue::Undef ), "Tie::StdScalar".to_string() ) )
        );
    }

    #[test]
    fn thaw_tied_array() {
        static DATA : [u8; 24] = [0x05,0x0b,0x0b,0x04,0x11,0x0d,0x54,0x69,0x65,0x3a,0x3a,0x53,0x74,0x64,0x41,0x72,0x72,0x61,0x79,0x02,0x00,0x00,0x00,0x00];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::TiedArray( wrap( PerlValue::Array(Vec::new()) ), "Tie::StdArray".to_string() ) )
        );
    }

    #[test]
    fn thaw_tied_array_element() {
        static DATA : [u8; 28] = [0x05,0x0b,0x16,0x04,0x11,0x0d,0x54,0x69,0x65,0x3a,0x3a,0x53,0x74,0x64,0x41,0x72,0x72,0x61,0x79,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::TiedArrayIdx( wrap( PerlValue::Array(Vec::new()) ), "Tie::StdArray".to_string(), 0 ) )
        );
    }

    #[test]
    fn thaw_tied_hash() {
        static DATA : [u8; 23] = [0x05,0x0b,0x0c,0x04,0x11,0x0c,0x54,0x69,0x65,0x3a,0x3a,0x53,0x74,0x64,0x48,0x61,0x73,0x68,0x03,0x00,0x00,0x00,0x00];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::TiedHash( wrap( PerlValue::Hash(HashMap::new()) ), "Tie::StdHash".to_string() ) )
        );
    }

    #[test]
    fn thaw_tied_hash_key() {
        static DATA : [u8; 28] = [0x05,0x0b,0x15,0x04,0x11,0x0c,0x54,0x69,0x65,0x3a,0x3a,0x53,0x74,0x64,0x48,0x61,0x73,0x68,0x03,0x00,0x00,0x00,0x00,0x0a,0x03,0x66,0x6f,0x6f];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        assert_eq!(
            result,
            wrap( PerlValue::TiedHashKey( wrap( PerlValue::Hash(HashMap::new()) ), "Tie::StdHash".to_string(), wrap( PerlValue::Scalar("foo".to_string())) ) )
        );
    }

    #[test]
    fn thaw_two_blessed() {
        static DATA : [u8; 30] = [0x05,0x0b,0x02,0x00,0x00,0x00,0x02,0x04,0x11,0x03,0x46,0x6f,0x6f,0x02,0x00,0x00,0x00,0x01,0x08,0x81,0x04,0x12,0x00,0x02,0x00,0x00,0x00,0x01,0x08,0x82];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        println!("{:?}",result);
    }

    #[test]
    fn thaw_yes() { // handcrafted, as I can't figure out how to make Perl give this to us
        static DATA : [u8; 3] = [0x05,0x0b,0x0f];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        println!("{:?}",result);
    }

    #[test]
    fn thaw_no() { // handcrafted, as I can't figure out how to make Perl give this to us
        static DATA : [u8; 3] = [0x05,0x0b,0x10];
        let result = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect("it broke");
        println!("{:?}",result);
    }

    #[test]
    fn thaw_native_byte_order() {
        static DATA : [u8; 24] = [0x04,0x0a,0x08,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x04,0x08,0x08,0x08,0x06,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00];

        let err = crate::thaw(&mut Cursor::new(DATA), &ThawSettings::without_magic() ).expect_err("it didn't break");
        if let ThawError::NativeByteOrderUnsupported = err {
            // good
        } else {
            panic!("got a different error {:?}",err);
        }
    }
}
