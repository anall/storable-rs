use crate::ThawError;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VString<
    ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
    BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
> {
    string: ST,
    data: BT,
}
impl<
        ST: AsRef<str> + PartialEq + Eq + Hash + Debug,
        BT: AsRef<[u8]> + PartialEq + Eq + Hash + Debug,
    > VString<ST, BT>
{
    pub(crate) fn from_raw_bits(string: ST, data: BT) -> Result<Self, ThawError> {
        Ok(Self { string, data })
    }
}
