#![no_main]
use libfuzzer_sys::fuzz_target;
use storable::{ThawSettings,ThawError,thaw};
use std::io::Cursor;


fuzz_target!(|data: &[u8]| {
    let config = ThawSettings::without_magic()
        .and_allowed_scalar_bytes(1024)
        .and_allowed_array_elements(512)
        .and_allowed_hash_keys(512);

    if let Err(e) = thaw( &mut Cursor::new(data), &config ) {
        match e {
            _ => {},
        }
    }
});
