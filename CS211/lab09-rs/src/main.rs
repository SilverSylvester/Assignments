extern crate rand;

use rand::{ thread_rng, Rng };
use std::process::Command;

fn main() {
    // Test cases
    let tcs = 1_000_000;

    /* --- UNINTERRUPTED ROLL --- */
    
    let mut roll_lengths: usize = 0;
    for _ in 0..tcs {
        roll_lengths += roll(6, 6).len();
    }
    
    println!("Uninterrupted roll: {}", roll_lengths as f32 / tcs as f32);

    /* --- INTERRUPTED ROLL --- */

    println!("Progress: -------------------|");

    roll_lengths = 0;
    let mut counter = 0;
    while counter < 100 {
        match interrupted_roll(6, 6) {
            // All conditions were satisfied, so we record
            // the result.
            Some(rolls) => {
                roll_lengths += rolls.len();
                counter += 1;
                progress(counter + 1, 100);
            },
            // One or more conditions were not satisfied,
            // so we do not record the result.
            None => continue,
        };
    }

    println!("Interrupted roll: {}", roll_lengths as f32 / counter as f32);
}

/// Rolls a 'die_max'-sided die until the value 'until'
/// is reached.
fn roll(die_max: usize, until: usize) -> Vec<usize> {
    if until < 1 || until > die_max {
        panic!("Unreachable value!");
    }
    
    let mut rolls: Vec<usize> = Vec::new();
    let mut rng = thread_rng();

    loop {
        // Shadows roll function
        let roll = rng.gen_range(0, die_max) + 1;
        rolls.push(roll);
        if roll == until {
            break;
        }
    }
    rolls
}

/// Returns an Option depending on whether the roll satisfies
/// all necessary criteria.
fn interrupted_roll(die_max: usize, until: usize) -> Option<Vec<usize>> {
    if until < 1 || until > die_max {
        panic!("Unreachable value!")
    }
    
    let mut rolls: Vec<usize> = Vec::new();
    let mut rng = thread_rng();
    let (mut sneezed, mut snap) = (false, false);
    
    let sneeze_prob = 1.0/40000.0;
    let snap_prob = 1.0/100.0;
    
    // We continuously roll the die until a certain desired value
    // is reached.
    loop {
        // If you sneeze, mark sneezed as true
        if rng.gen_range(0.0, 1.0) < sneeze_prob {
            sneezed = true;
        }
        // If you recieve a snap, mark snap as true
        if rng.gen_range(0.0, 1.0) < snap_prob {
            snap = true;
        }
        // Roll die (shadows roll function)
        let roll = rng.gen_range(0, die_max) + 1;
        
        // Add the result of the roll to the vector
        rolls.push(roll);
        
        // Check if the roll matches the desired value
        if roll == until {
            break;
        }
    }
    // Only record the results if you sneezed and recieved a snap
    if sneezed && snap { Some(rolls) }
    else { None }
}

fn print_same_line(msg: &str) {
    Command::new("echo")
            .arg("-ne")
            .arg(&("\r".to_string() + msg))
            .status()
            .unwrap_or_else(|_| {
                panic!("Failed to execute process")
            });
}

fn continue_line(msg: &str) {
    Command::new("echo")
            .arg("-ne")
            .arg(msg)
            .status()
            .unwrap_or_else(|_| {
                panic!("Failed to execute process")
            });
}

fn progress(current: usize, finished: usize) {
    if current <= finished {
        let prog = 20_f32 * current as f32 / finished as f32;
        let percentage = 100_f32 * current as f32 / finished as f32;
        print_same_line(&(percentage.to_string() + "%\t  "));
        for _ in 0..(prog.floor() as usize) {
            continue_line("#");
        }
    }
    else {
        println!("");
    }
}

