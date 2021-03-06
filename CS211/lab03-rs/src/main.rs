mod huffman;

use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::collections::HashMap;

fn main() {
    for file in env::args().skip(1) {
        println!("\n\tFile: {}", file);
        let mut f = match File::open(file) {
            Ok(f) => f,
            Err(..) => err("Could not open file(s)."),
        };
        let mut s = String::new();
        f.read_to_string(&mut s).expect("Could not read file to string.");
        // TODO: The tree generation algorithm is a huge bottleneck
        let huff_tree = huffman::gen_tree(&s);
        let mut codes = HashMap::<char, String>::new();
        huffman::gen_codes(&huff_tree, &mut codes, "");

        // Sort by code length
        let mut sorted: Vec<_> = codes.iter().collect();
        sorted.sort_by(|a, b| a.1.len().cmp(&b.1.len()));
        for (c,code) in sorted {
            println!("{:?}: {}", c, code);
        }
    }
}

fn err(msg: &str) -> ! {
    println!("Error: {}", msg);
    std::process::exit(1);
}

