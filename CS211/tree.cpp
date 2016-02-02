#include <iostream>
#include <stdio.h>
#include <cstdlib>

struct Node
{
    int data;
    Node *l;
    Node *r;
};

class Tree
{
public:
    Tree();
    ~Tree();

    void insert(int);
    Node *search(int);
    void destroy_tree();
    void print_order();

private:
    void destroy_tree(Node *);
    void insert(int, Node *);
    Node *search(int, Node *);
    void print_order(Node *);
        
    Node *root;
};

Tree::Tree()
{
    root = NULL;
}

Tree::~Tree()
{
    destroy_tree();
}

void Tree::destroy_tree(Node *leaf)
{
    if (leaf != NULL) {
        destroy_tree(leaf->l);
        destroy_tree(leaf->r);
        delete leaf;
    }
}

void Tree::insert(int key, Node *leaf)
{
    if (key < leaf->data) {
        if (leaf->l != NULL)
            insert(key, leaf->l);
        else {
            leaf->l = new Node;
            leaf->l->data = key;
            leaf->l->l = NULL;
            leaf->l->r = NULL;
        }
    }
    else if (key > leaf->data) {
        if (leaf->r != NULL)
            insert(key, leaf->r);
        else {
            leaf->r = new Node;
            leaf->r->data = key;
            leaf->r->l = NULL;
            leaf->r->r = NULL;
        }
    }
}

Node *Tree::search(int key, Node *leaf)
{
    if (leaf != NULL) {
        if (key < leaf->data)
            return search(key, leaf->l);
        else if (key > leaf->data)
            return search(key, leaf->r);
        else
            return leaf;
    }
}

void Tree::insert(int key)
{
    if (this->root != NULL)
        insert(key, root);
    else {
        this->root = new Node;
        this->root->data = key;
        this->root->l = NULL;
        this->root->r = NULL;
    }
}

Node *Tree::search(int key)
{
    return search(key, this->root);
}

void Tree::print_order()
{
    if (this->root != NULL) {
        print_order(this->root);
    }
}

void Tree::print_order(Node *t)
{
    if (t != NULL) {
        print_order(t->l);
        std::cout << t->data << std::endl;
        print_order(t->r);
    }
}

void Tree::destroy_tree()
{
    destroy_tree(this->root);
}

int main()
{
    Tree *t = new Tree;
    std::cout << "Number of nodes: ";
    int n; std::cin >> n;
    std::cout << "Values to insert: ";
    for (int i = 0; i < n; i++) { 
        int tmp; std::cin >> tmp;
        t->insert(tmp);
    }

    t->print_order();
    delete t; // This works, for some reason.
}

