#!/usr/bin/python

""" Binary trees! """

class Node(object):
    """ Single node class """
    def __init__(self, val):
        self.left = None
        self.right = None
        self.data = val

class Tree(object):
    """ Binary tree class """
    def __init__(self):
        self.root = None

    def insert(self, val):
        """ In-order tree insertion """
        if self.root is None:
            self.root = Node(val)
        else:
            self._insert(val, self.root)

    def _insert(self, val, node):
        """ Invisible helper method """
        if val < node.data:
            if node.left is not None:
                self._insert(val, node.left)
            else:
                node.left = Node(val)
        else:
            if node.right is not None:
                self._insert(val, node.right)
            else:
                node.right = Node(val)

    def print_order(self):
        """ Prints tree in-order """
        if self.root:
            self._print_order(self.root.left)
            print self.root.data
            self._print_order(self.root.right)

    def _print_order(self, node):
        """ (Private) prints tree in order """
        if node:
            self._print_order(node.left)
            print node.data
            self._print_order(node.right)

NS = Tree()
for i in map(int, raw_input().split(' ')):
    NS.insert(i)

# for i in [int(n) for i in raw_input().split(' ')]
#     ...

NS.print_order()

