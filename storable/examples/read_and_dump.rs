use std::env;
use std::fs::File;
use storable::{thaw_from_io, ThawSettings};

fn main() {
    let args: Vec<String> = env::args().collect();

    for path in &args[1..] {
        let mut file = File::open(path).expect("failed to open file");

        let settings = ThawSettings::without_magic().and_strip_refs();

        let d = thaw_from_io(&mut file, &settings);
        match d {
            Ok(v) => println!("{}: {:#?}", path, &v.as_ref().borrow()),
            Err(e) => println!("{}: {:?}", path, e),
        }
    }
}
