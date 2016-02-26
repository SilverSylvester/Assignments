use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::cmp::Ordering::{Less, Equal, Greater};

// Each Huffman node has a weight (which is the sum of the two children's),
// and either a leaf with the character in it, or another HuffTreeBranch.

// TODO: Maybe try to use less confusing names. That, or make the data
// structure less clumsy.
pub struct HuffNode {
    weight: usize,
    data: HuffTreeData,
}

enum HuffTreeData {
    Tree(HuffTreeBranch),
    Leaf(char),
}

struct HuffTreeBranch {
    l: Box<HuffNode>,
    r: Box<HuffNode>,
}

// Thanks Reddit, didn't know you could have empty implementations:
// https://www.reddit.com/r/rust/comments/2ue9jj/how_do_i_give_my_own_types_eq/
impl Eq for HuffNode {}

// Need this so that the Binary heap treats low frequency as high
// priority (BinaryHeap is a *max* heap, not a min heap)
impl Ord for HuffNode {
    fn cmp(&self, other: &HuffNode) -> Ordering {
        match self.weight.cmp(&other.weight) {
            Less => Greater,
            Equal => Equal,
            Greater => Less,
        }
    }
}

impl PartialOrd for HuffNode {
    fn partial_cmp(&self, other: &HuffNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for HuffNode {
    fn eq(&self, other: &HuffNode) -> bool {
        self.weight == other.weight
    }
}

/// Generates Huffman tree
pub fn gen_tree(input: &str) -> HuffNode {
    
    // I'm pretty much translating this wiki page into Rust:
    // https://en.wikipedia.org/wiki/Huffman_coding
    
    // Create a leaf for each symbol and add it to the priority queue
    // So need a list of characters and frequencies. Sounds like a job
    // for a HashMap.

    let mut hm = HashMap::new();
    for c in input.chars() {
        *hm.entry(c).or_insert(0) += 1;
    }

    // Now we have a HashMap full of (key, val) pairs, which translate
    // to (char, weight) pairs, specifically. Now to create a leaf for
    // each symbol, which we push onto a binary heap via the comparison
    // function we supplied.
    
    let mut q = BinaryHeap::new();
    for (c,w) in hm.iter() {
        let leaf = HuffNode { weight: *w, data: HuffTreeData::Leaf(*c) };
        q.push(leaf);
    }

    // While there is more than one node in the queue:
    while q.len() > 1 {
        // Remove the two nodes of highest priority (lowest weight)
        // from the queue.
        let n1 = q.pop().unwrap(); let n2 = q.pop().unwrap();
        // Unwrapping is fine here since we know the queue has more than
        // one element in it.

        // Create a new internal node with these two nodes as children
        // and with weight equal to the sum of the two nodes' weights.
        let node = HuffNode {
            weight: n1.weight + n2.weight,
            data: HuffTreeData::Tree(HuffTreeBranch {
                l: Box::new(n1),
                r: Box::new(n2),
            })
        };
        // Add the new node to the queue.
        q.push(node);
    }

    // The remaining node is the root node, and the tree is complete.
    // So we simply need to return this node.

    q.pop().unwrap()
}

/// Recursive code generation. Stores values in a HashMap.
pub fn gen_codes(huffman_tree: &HuffNode, codes: &mut HashMap<char, String>, code_str: &str) {
    match huffman_tree.data {
        HuffTreeData::Tree(ref huff_tree_branch) => {
            gen_codes(&huff_tree_branch.l, codes, &format!("{}0", code_str));
            gen_codes(&huff_tree_branch.r, codes, &format!("{}1", code_str));
        }
        HuffTreeData::Leaf(c) => {
            codes.insert(c, code_str.to_string());
        }
    };
}

