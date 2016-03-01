""" Huffman encoding """

import heapq
import sys

class HuffNode(object):  # pylint: disable=too-few-public-methods
    """ Definition of the HuffNode """
    def __init__(self, data=None):
        # All hail duck typing!
        self.data = data
        self.left = None
        self.right = None

def gen_tree(string):
    """ Generates Huffman tree """
    # First, generate (char, weight) pairs
    hm = {}  # pylint: disable=invalid-name
    for char in string:
        if char in hm:
            hm[char] += 1
        else:
            hm[char] = 1

    # Then create a list of leaves
    q = []  # pylint: disable=invalid-name
    for char, weight in hm.iteritems():
        heapq.heappush(q, (weight, HuffNode(char)))

    # While there's more than one node in the queue ...
    while len(q) > 1:
        # Pop the two nodes with lowest weight
        (w1, n1) = heapq.heappop(q)  # pylint: disable=invalid-name
        (w2, n2) = heapq.heappop(q)  # pylint: disable=invalid-name

        # Generate new node with the two above nodes as children.
        huffnode = HuffNode()
        huffnode.left = n1
        huffnode.right = n2

        # And push it back onto the queue with their combined weight
        heapq.heappush(q, (w1 + w2, huffnode))

    # Now just return the tree
    return heapq.heappop(q)[1]

def gen_codes(hufftree, codes, code_str=""):
    """ Generates the codes for a Huffman tree, stores them in
        the 'codes' dict ."""
    if hufftree.data is None:
        gen_codes(hufftree.left, codes, code_str + '0')
        gen_codes(hufftree.right, codes, code_str + '1')
    else:
        codes[hufftree.data] = code_str

def encode():
    """ Does nothing, yet. """
    return

def print_canonical(string):
    """ Prints canonical representation of bits """
    tree = gen_tree(string)
    codes = {}
    gen_codes(tree, codes)

# MAIN #

if __name__ == "__main__":
    for fname in sys.argv[1:]:
        with open(fname, 'r') as f:
            print "\n\t Filename:", fname
            TREE = gen_tree(f.read())
            CODES = {}
            gen_codes(TREE, CODES)
            in_order = \
                sorted(CODES.items(),
                       lambda x, y: cmp(len(x[1]), len(y[1])))
            for c, code in in_order:
                print repr(c) + ":", code

