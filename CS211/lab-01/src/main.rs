extern crate rand;

use rand::distributions::{IndependentSample, Range, Normal};

fn main() {
    let mut rng = rand::thread_rng();
    let mut triangles = 0;
    let tcs = 1_000_000;

    // Here we'll assume the first break, 'p', is the largest. To make it
    // as simple as possible, we'll explicitly declare each side of the
    // triangle and directly verify whether it forms a valid triangle or
    // not. In a sense, this is the 'dumbest' solution to the problem, but
    // has the benefit that we can be effectively certain that it will
    // generate a reasonably accurate answer.

    // The only invariant range
    let p_range = Range::new(0.5, 1_f64);

    for _ in 0..tcs {
        let p = p_range.ind_sample(&mut rng);
        let q_range = Range::new(0_f64, p);
        let q = q_range.ind_sample(&mut rng);
        let s1 = 1_f64 - p; let s2 = p - q; let s3 = q;
        if s1 + s2 > s3 && s2 + s3 > s1 && s3 + s1 > s2 {
            triangles += 1;
        }
    }

    let approx_ans = triangles as f64 / tcs as f64;
    let exact_ans = 4_f64.ln() - 1_f64;

    println!("Monte Carlo estimate: {}", approx_ans);
    println!("Actual value: {}", exact_ans);
    println!("Difference: {}\n", (approx_ans - exact_ans).abs());
}

