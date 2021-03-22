use std::convert::TryFrom;
use std::fmt;

// we need both the trait and the macro
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug)]
pub struct InvalidMarkerError(pub u8);

impl fmt::Display for InvalidMarkerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid marker {:#x}", self.0)
    }
}

#[repr(u8)]
#[allow(clippy::enum_variant_names)] // we're matching the Perl prefix
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, FromPrimitive, Eq, PartialEq)]
pub(crate) enum TypeMarkers {
    SxObject = 0,
    SxLScalar = 1,
    SxArray = 2,
    SxHash = 3,
    SxRef = 4,
    SxUndef = 5,
    SxInteger = 6,
    SxDouble = 7,
    SxByte = 8,
    SxNetInt = 9,
    SxScalar = 10,
    SxTiedArray = 11,
    SxTiedHash = 12,
    SxTiedScalar = 13,
    SxSvUndef = 14,
    SxSvYes = 15,
    SxSvNo = 16,
    SxBless = 17,
    SxIxBless = 18,
    SxHook = 19,
    SxOverload = 20,
    SxTiedKey = 21,
    SxTiedIdx = 22,
    SxUtf8Str = 23,
    SxLUtf8Str = 24,
    SxFlagHash = 25,
    SxCode = 26,
    SxWeakRef = 27,
    SxWeakOverload = 28,
    SxVString = 29,
    SxLVString = 30,
    SxSvUndefElem = 31,
    SxRegexp = 32,
    SxLObject = 33,
    // SxLast = 34, // SX_LAST isn't a marker

    /*
    // "old" pre-0.6,
    Old6SxItem = 'i' as u8,
    Old6SxItUndef = 'I' as u8,
    Old6SxKey = 'k' as u8,
    Old6SxValue = 'v' as u8,
    Old6SxVlUndef = 'V' as u8,

    // "old" pre-0.7,
    Old7SxClass = 'b' as u8,
    Old7SxLgClass = 'B' as u8,
    Old7SxStored = 'X' as u8,*/
}
impl TryFrom<u8> for TypeMarkers {
    type Error = InvalidMarkerError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        FromPrimitive::from_u8(value).ok_or(InvalidMarkerError(value))
    }
}
