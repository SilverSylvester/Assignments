/// Implementation of a binary search tree

/// To test this, run:
///
/// ```
/// cat <(cat ~/Documents/Rust/input001.txt) <(echo "`some number`") | ./tree
///
/// ```
///
/// The file should be a single line containing space separated u32's
/// (although it may work if there's other stuff in there, should just
/// read to the first newline)

use std::io;

#[derive(Debug,PartialEq)]
struct Node<'a> {
    data: &'a u32,
    l: Option<Box<Node<'a>>>,
    r: Option<Box<Node<'a>>>,
}

impl<'a> Node<'a> {
    /// In-order insertion of u32
    pub fn insert(&mut self, data: &'a u32) {
        if self.data == data {
            return // This implementation doesn't accept duplicates
        }
        let dir_node = /* direction node */
            if data < self.data {
                &mut self.l
            }
            else {
                &mut self.r
            };

        match dir_node {
            &mut Some(ref mut subnode) => subnode.insert(data),
            &mut None => {
                let new_node = Node { data: data, l: None, r: None };
                let boxed_node = Some(Box::new(new_node));
                *dir_node = boxed_node;
            }
        }
    }

    /// Finds node containing certain data. Returns an Option in case the
    /// data does not exist.
    pub fn find(&self, data: &'a u32) -> Option<&Node<'a>> {
        if data == self.data { Some(self) }
        else if data < self.data {
            match self.l {
                Some(ref subnode) => subnode.find(data),
                None => None,
            }
        }
        else {
            match self.r {
                Some(ref subnode) => subnode.find(data),
                None => None,
            }
        }
    }

    /// Finds the minimum node in a tree
    pub fn min(&self) -> Option<&Node<'a>> {
        match self.l {
            Some(ref subnode) => subnode.min(),
            None => Some(self),
        }
    }

    pub fn delete(&mut self, data: &'a u32) {
        // Find node
        // ...
        // Profit!
    }

    // TODO: Find an ugly hack or find out what's happening here
    /*
    pub fn is_btree(&self) -> bool {
        let node = self;

        // This works fine
        match node.l { _ => {} };

        // Why is it that matching on two values causes issues,
        // but matching on only one doesn't?

        match (node.l, node.r) {
            (Some(ref sn1), Some(ref sn2)) =>
                if sn1.data < node.data && node.data < sn2.data {
                    sn1.is_btree() && sn2.is_btree()
                }
                else { false },
            (Some(ref sn1), None) =>
                if sn1.data < node.data {
                    sn1.is_btree()
                }
                else { false },
            (None, Some(ref sn2)) =>
                if node.data < sn2.data {
                    sn2.is_btree()
                }
                else { false },
            (None, None) => true,
        }
    }
    */

    /* ! UNFINISHED ! */
    /*
    pub fn delete(&mut self, data: &'a u32) {
        let mut cn = self.find(data);
        match cn {
            Some(ref subnode) =>
                match (subnode.l, subnode.r) {
                    
                },
            None => panic!("Node does not exist!"),
        }
    }
    */

    /* TODO:
     * Create functions:
     *      1. Delete node
     *      2. is_btree (to check validity of delete function)
     *      3. tree_sort (rather than just printing in order)
     */

    /// Prints binary tree in order.
    pub fn print_in_order(&self) {
        match self.l {
            Some(ref subnode) => subnode.print_in_order(),
            None => {},
        };
        println!("{} ", self.data);
        match self.r {
            Some(ref subnode) => subnode.print_in_order(),
            None => {},
        };
    }
}

fn main() {
    // Usage: input a string of space separated u32 (that's unsigned, so
    // no negative numbers).
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Oh. This is awkward.");
    
    let input: Vec<u32> = input
        .trim().split_whitespace()
        .map(|n| n.parse().unwrap())
        .collect();

    let mut x = Node { data: &input[0], l: None, r: None };
    for i in &input {
        x.insert(&i);
    }

    // assert!(x.is_btree());

    println!("Raw printing:\n{:?}", x);

    println!("Minimum value: {}", x.min().unwrap().data);

    // This should (obviously) have the effect of sorting the list.
    println!("Printing tree in order ...");
    x.print_in_order();
    println!("");

    // Find the subtree beginning with `n` and print it in-order from
    // there.
    let mut n = String::new();
    io::stdin().read_line(&mut n).expect("Failure on read: `n` (line 103)");
    let n: u32 = match n.trim().parse() {
        Ok(n) => n,
        Err(..) => panic!("Failure on parse: {}", n),
    };

    println!("Printing subtree with {} as the root ...", n);
    
    match x.find(&n) {
        Some(subnode) => println!("{:?}", subnode),
        None => {},
    };

    match x.find(&n) {
        Some(subnode) => subnode.print_in_order(),
        None => println!("Could not find {}", n),
    };
}

