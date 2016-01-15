use std::io;

fn main() {
    let mut s = String::new();
    io::stdin().read_line(&mut s).expect("Failure: read_line");
    s = s.to_lowercase();
                
    if is_palindrome(&s) {
        println!("TRUE");                
    }
    else {
        println!("FALSE");
    }
}

fn is_palindrome(s: &str) -> bool {
    let rev_s = s.chars().rev().take(s.len() / 2);
    let s = s.chars().take(s.len() / 2);
    s.zip(rev_s).all(|(a,b)| {a == b})
}

