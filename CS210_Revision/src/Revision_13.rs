use std::io;

fn main() {
    let mut n = String::new();
    io::stdin().read_line(&mut n).expect("Failure: read_line");

    match n.parse::<u32>() {
        Ok(i) => println!("{:.3}", 1f32 - birthday(i)),
        Err(..) => println!("Attempted to parse: \"{}\"", n)
    };
}

fn birthday(n: u32) -> f32 {
    if (n < 2) {
        1f32
    }
    else {
        (366f32 - (n as f32)) / 365f32 * birthday(n - 1)
    }
}

