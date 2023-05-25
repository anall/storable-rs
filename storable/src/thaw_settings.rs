#[allow(clippy::struct_excessive_bools)]
pub struct ThawSettings {
    pub(crate) with_magic: bool,

    pub(crate) allowed_remaining_string_bytes: usize,
    pub(crate) allowed_remaining_array_elements: usize,
    pub(crate) allowed_remaining_hash_keys: usize,

    pub(crate) max_unit_string_bytes: usize,
    pub(crate) max_unit_array_elements: usize,
    pub(crate) max_unit_hash_keys: usize,

    pub(crate) max_depth: usize,

    pub(crate) allow_byte_hashes: bool,
    pub(crate) upgrade_unflagged_utf8: bool,
    pub(crate) downgrade_restricted_hashes: bool,
    pub(crate) strip_refs: bool,
}
impl ThawSettings {
    #[must_use]
    fn default() -> Self {
        // Not `Default::default` for visibility
        Self {
            with_magic: false,

            allowed_remaining_string_bytes: usize::MAX,
            allowed_remaining_array_elements: usize::MAX,
            allowed_remaining_hash_keys: usize::MAX,

            max_unit_string_bytes: i32::MAX as usize,
            max_unit_array_elements: i32::MAX as usize,
            max_unit_hash_keys: i32::MAX as usize,

            max_depth: 1024,

            allow_byte_hashes: false,
            upgrade_unflagged_utf8: false,
            downgrade_restricted_hashes: false,
            strip_refs: false,
        }
    }

    #[must_use]
    pub fn with_magic() -> Self {
        Self {
            with_magic: true,
            ..ThawSettings::default()
        }
    }
    #[must_use]
    pub fn without_magic() -> Self {
        Self {
            with_magic: false,
            ..ThawSettings::default()
        }
    }

    /// Adjust settings to set the maximum allowed recursion depth
    #[must_use]
    pub fn and_max_depth(mut self, val: usize) -> Self {
        self.max_depth = val;
        self
    }

    /// Adjust settings to set the maximum number of total bytes allowed to be consumed by (string) values, including hash keys
    #[must_use]
    pub fn and_allowed_string_bytes(mut self, val: usize) -> Self {
        self.allowed_remaining_string_bytes = val;
        self
    }

    /// Adjust settings to set the maximum number of total allowed array elements
    #[must_use]
    pub fn and_allowed_array_elements(mut self, val: usize) -> Self {
        self.allowed_remaining_array_elements = val;
        self
    }

    /// Adjust settings to set the maximum number of total allowed hash keys
    #[must_use]
    pub fn and_allowed_hash_keys(mut self, val: usize) -> Self {
        self.allowed_remaining_hash_keys = val;
        self
    }

    /// Adjust settings to set the maximum number of bytes allowed to be consumed by a single (string) values, including hash keys
    #[must_use]
    pub fn and_max_unit_string_bytes(mut self, val: usize) -> Self {
        self.max_unit_string_bytes = val;
        self
    }

    /// Adjust settings to set the maximum number of elements allowed in a single array
    #[must_use]
    pub fn and_max_unit_array_elements(mut self, val: usize) -> Self {
        self.max_unit_array_elements = val;
        self
    }

    /// Adjust settings to set the maximum number of keys allowed in a single hash
    #[must_use]
    pub fn and_max_unit_hash_keys(mut self, val: usize) -> Self {
        self.max_unit_hash_keys = val;
        self
    }

    /// Adjust settings to allow hashes with invalid-utf8 keys
    #[must_use]
    pub fn and_with_byte_hashes(mut self) -> Self {
        self.allow_byte_hashes = true;
        self
    }

    /// Adjust settings to upgrade unflagged utf8 (assuming the value is valid utf8 bytestream)
    #[must_use]
    pub fn and_upgrade_unflagged_utf8(mut self) -> Self {
        self.upgrade_unflagged_utf8 = true;
        self
    }

    /// Adjust settings to downgrade restricted hashes to normal hashes
    #[must_use]
    pub fn and_downgrade_restricted_hashes(mut self) -> Self {
        self.downgrade_restricted_hashes = true;
        self
    }

    /// Adjust settings to strip refs, which can make it easier to deal with complex data structures
    #[must_use]
    pub fn and_strip_refs(mut self) -> Self {
        self.strip_refs = true;
        self
    }
}
