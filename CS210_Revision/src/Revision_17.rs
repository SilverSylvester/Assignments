use std::io;

fn main() {
    let mut s1 = String::new();
    let mut s2 = String::new();
    io::stdin().read_line(&mut s1).expect("Failure: read_line");
    io::stdin().read_line(&mut s2).expect("Failure: read_line");

    let a: u64 = match s1.trim().parse() {
        Ok(n) => n,
        Err(..) => panic!("Failure: no parse")
    }; /* This section was failing due to a trailing '\n',
        * hence the trimming. */

    let b: u64 = match s2.trim().parse() {
        Ok(n) => n,
        Err(..) => panic!("Failure: no parse")
    };

    println!(gcd(a,b));
}

fn gcd(a: u64, b: u64) -> u64 {
    match b {
        0 => a,
        _ => gcd(b, a % b)
    }
}

