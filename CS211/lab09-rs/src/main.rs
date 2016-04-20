extern crate rand;

use rand::{ thread_rng, Rng };
use rand::distributions::{ Range, IndependentSample };

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

    roll_lengths = 0;
    let mut counter = 0;
    for _ in 0..tcs {
        match interrupted_roll(6, 6) {
            // All conditions were satisfied, so we record
            // the result.
            Some(rolls) => {
                roll_lengths += rolls.len();
                counter += 1;
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
    
    // The given number of test cases is not enough to properly
    // handle these extremely low probabilities, reduce them
    // for more consistent results.
    let sneeze_range = Range::new(0, 40000);
    let snap_range = Range::new(0, 100);
    
    // We continuously roll the die until a certain desired value
    // is reached.
    loop {
        // If you sneeze, mark sneezed as true
        if sneeze_range.ind_sample(&mut rng) == 0 {
            sneezed = true;
        }
        // If you recieve a snap, mark snap as true
        else if snap_range.ind_sample(&mut rng) == 0 {
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

