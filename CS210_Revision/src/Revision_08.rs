use std::io;
use std::collections::BinaryHeap;

fn main() {
    let mut ns = String::new();
    io::stdin().read_line(&mut ns).expect("Failure: read_line `n`");

    let ns: Vec<i16> = ns
        .split_whitespace().map(|n| n.parse().unwrap())
        .collect();

    /* push() to BinaryHeap will perform max-heap priority insert */
    let mut pq = BinaryHeap::new();
    for n in ns { pq.push(n); }

    for n in pq.into_sorted_vec() {
        println!("{}", n);
    }
}

