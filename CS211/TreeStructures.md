Binary Trees
============

**Types of binary trees**:

- A **rooted** binary tree has a root node, and every node has at most two
children.
- A **full** binary tree is a rooted BT in which all interior nodes have
either 0 or 2 children.
- A **perfect** binary tree is a tree structure in which all interior nodes
have two children *and* all leaves have the same depth or level.
- A **balanced** binary tree has the minimum possible maximum depth for the
leaf nodes.
- A **degenerate** tree is where each parent node has only one associated
child node, effectively reducing the tree to a linked list.

An example in Java:

```java
class Node {
    int data;
    Node left;
    Node right;
}

```

Or in a real language:

```c
typedef struct Node
{
    int data;
    struct Node left;
    struct Node right;
} Node;

```

Or in Haskell:

```haskell
data Tree a = Null | Node a (Tree a) (Tree a)

```

\pagebreak

Finding a particular node in a tree
-----------------------------------

In Java:

```java
Node find(int key) {
    Node current = root;
    while (true) {
        if (key < current.data)
            current = current.left;
        else if (key > current.data)
            current = current.right;
        else return current;
    }
}

```

This is pretty much the same for any language.

Inserting a value into a tree
-----------------------------

In Haskell:

```haskell
insertT :: (Ord a) => a -> Tree a -> Tree a
insertT x Null = Node x Null Null
insertT x (Node a l r)
    | x < a     = Node a (insertT x l) r
    | x > a     = Node a l (insertT x r)
    | otherwise = Node a l r

```

In C (do this later)

Convert BT to ordered list
--------------------------

```java
void someOrder(Node root) {
    if (root != null) {
        someOrder(root.left);
        someOrder(root.right);
        System.out.println(root.data);
    }
}

```

