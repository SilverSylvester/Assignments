use std::io::{self, BufReader};
use std::io::prelude::*;
use std::fs::File;

// Yet again, *massive* issues with the HashMap speed.
use std::collections::HashMap;
use std::collections::BinaryHeap;

// Needed for implementations for wrapper type
use std::cmp::Ordering;
use std::cmp::Ordering::{Less, Equal, Greater};

/* TODO: Try to time the proportion of runtime spent generating
 * the HashMaps, I bet it's quite a lot. */

/// This length comparator wrapper is necessary for custom ordering.
#[derive(Eq, PartialEq)]
struct LengthComparator {
    string: String,
}

/// Implementation for Ord (this may appear to sort from lowest to
/// highest, but since BinaryHeap implements a max heap, the order
/// in which elements will be popped off is highest to lowest.
impl Ord for LengthComparator {
    fn cmp(&self, other: &LengthComparator) -> Ordering {
        match self.string.len().cmp(&other.string.len()) {
            Less => Less,
            Equal => Equal,
            Greater => Greater,
        }
    }
}

/// Obligatory PartialOrd implementation (as a result of defining Ord)
/// Luckily we can just defer it to the implemetation of Ord.
impl PartialOrd for LengthComparator {
    fn partial_cmp(&self, other: &LengthComparator) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    println!("Enter character set:");
    
    // 'chars' is the set of characters we want to match with items
    // in the dictionary.
    let mut chars = String::new();
    io::stdin().read_line(&mut chars).expect("Couldn't parse input");

    // Matching on f, file won't exist on Windows/Mac
    let f = match File::open("/usr/share/dict/words") {
        Ok(f) => f,
        Err(..) => panic!("Couldn't open dictionary."),
    };

    // The buffered reader will allow us to efficiently iterate over the
    // lines in the file f.
    let dict = BufReader::new(f);

    // Generating character counts for char_counter. HashMaps are incredibly
    // slow in Rust so this takes a lot longer than it needs to.
    let char_counter = counts(&chars);

    // Instantiating binary heap to act as a priority queue
    let mut valid_words = BinaryHeap::new();

    // This needs to be defined in this scope so that anything 'word' owns
    // can be carried out of the scope of the for loop (including the entire
    // contents of 'valid_words')
    let mut word: String;

    for line in dict.lines() {
        // Notational convenience, and convincing the borrow checker that
        // my code is acceptable. Unwrapping is fine since we know each
        // line exists.
        word = line.unwrap();
        
        // Assume we can form the word until we can prove otherwise.
        let mut can_form = true;
        
        // Generate character counts for the word in the dictionary (again,
        // this is a bottleneck)
        let word_counter = counts(&word);
        
        for (c,freq) in &word_counter {
            // We try to get the value in char_counter associated with the
            // key 'c'
            match char_counter.get(c) {
                // If that character isn't in our list, we clearly can't
                // form the word, so we break out of the loop.
                None => {
                    can_form = false;
                    break;
                },
                // If we *do* have that character, we need to check if we 
                // have enough of that character to form the word. If not,
                // we break out of the loop.
                Some(val) => if val - freq < 0 {
                    can_form = false;
                    break
                },
            };
        }

        // If we pass the above tests, we can make the word, so we add it
        // to the binary heap wrapped in our custom comparator. If not, we
        // skip this step and continue the loop.
        if can_form {
            valid_words.push(LengthComparator{ string: word });
        }
    }

    println!("Top ten suggestions:");
    // Print first ten matches, or as much as exists in 'valid_words'
    for _ in 0..10 {
        match valid_words.pop() {
            Some(LengthComparator { string: word }) => println!("{}", word),
            None => break,
        };
    }
}

/// Gets character counts for input string. Very slow due to
/// cryptographically secure hashing functions (which we clearly
/// don't need but are stuck with).
fn counts(s: &str) -> HashMap<char, i16> {
    let mut counter = HashMap::new();
    for c in s.chars() {
        *counter.entry(c).or_insert(0) += 1;
    }
    counter
}

