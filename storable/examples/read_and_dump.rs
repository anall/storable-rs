use storable::{thaw, ThawSettings};
use std::io::Cursor;
use std::env;
use std::fs::File;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}",args);

    let mut file = File::open(&args[1]).expect("failed to open file");

    let settings = ThawSettings::without_magic().and_strip_refs();

    let d = thaw(&mut file, &settings );
    println!("{:#?}",d);
}