/* Wrote a tail recursive function instead */

use std::io;

fn main() {
    let mut n = String::new();
    io::stdin().read_line(&mut n).expect("Failure: read_line `n`");

    match n.trim().parse() {
        Ok(m)   => println!("{}.0", tail_fac(m)),
        Err(..) => panic!("No parse on `n`")
    };
}

fn tail_fac(n: u64) -> u64 {
    fn _fac(a: u64, b: u64) -> u64 {
        match (a, b) {
            (0, b) => b,
            _      => _fac(a - 1, a * b)
        }
    }
    _fac(n, 1)
}

