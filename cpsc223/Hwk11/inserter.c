#include "inserter.h"
// Opaque Inserter data type
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct node
{
    int data; // it can be used for both avl and treap -- treap is so much easier 
    struct element *list_ptr;
    struct node *left;
    struct node *right;
} Node;

Node *find_smaller(Node *root, const char *s);
void tree_add(Node **root, Node *n);
int find(Node *root, const char *s);

struct inserter
{
    Node *root;
    struct element **list_head; // update when no one is smaller
};

Inserter inserterCreate(struct element **head)
{
    time_t t;
    srand((unsigned)time(&t));
    Inserter i = calloc(1, sizeof(struct inserter));
    i->list_head = head;
    return i;
}

Node *create_node()
{
    Node *newnode = calloc(1, sizeof(Node));
    newnode->data = rand();
    return newnode;
}

void inserterAdd(Inserter i, const char *s)
{
    size_t len = strlen(s);
    char *copied = calloc(len + 1, 1);
    strcpy(copied, s);
    struct element *new_list_node = calloc(1, sizeof(struct element));
    new_list_node->string = copied;

    if (find(i->root, s) == 1)
    {
        free(new_list_node);
        return;
    }

    Node *smaller = find_smaller(i->root, copied);
    if (smaller == 0)
    {
        struct element *oldlisthead = *(i->list_head);
        *(i->list_head) = new_list_node;
        new_list_node->next = oldlisthead;
    }
    else
    {
        struct element *oldlistnext = smaller->list_ptr->next;
        smaller->list_ptr->next = new_list_node;
        new_list_node->next = oldlistnext;
    }

    Node *newnode = create_node();
    newnode->list_ptr = new_list_node;

    tree_add(&i->root, newnode);
}

int compare_nodes(Node *a, Node *b)
{
    return strcmp(a->list_ptr->string, b->list_ptr->string);
}

int find(Node *root, const char *s)
{
    if (root == 0)
    {
        return 0;
    }
    Node *cur = root;

    while (cur != 0)
    {
        int cmp = strcmp(cur->list_ptr->string, s);
        if (cmp < 0)
        {
            cur = cur->right;
        }
        else if (cmp > 0)
        {
            cur = cur->left;
        }
        else
        {
            return 1;
        }
    }
    return 0;
}

Node *find_smaller(Node *root, const char *s)
{
    if (root == 0)
    {
        return 0;
    }
    Node *cur = root;
    Node *candidite = 0;

    while (cur != 0)
    {
        int cmp = strcmp(cur->list_ptr->string, s);
        if (cmp <= 0)
        {
            candidite = cur;
            cur = cur->right;
        }
        else
        {
            cur = cur->left;
        }
    }
    return candidite;
}

// if duplicate encountered returns 0
void right_rotate(Node **root)
{
    Node *cur = *root;
    Node *l = cur->left;
    Node *lr = l->right;
    l->right = cur;
    cur->left = lr;
    *root = l;
}

void left_rotate(Node **root)
{
    Node *cur = *root;
    Node *r = cur->right;
    Node *rl = r->left;
    r->left = cur;
    cur->right = rl;
    *root = r;
}
void tree_add(Node **root, Node *n)
{

    if (*root == 0)
    {
        *root = n;
    }
    int cmp = compare_nodes(*root, n);
    if (cmp < 0)
    {
        tree_add(&(*root)->right, n);
        int data = (*root)->right->data;
        if (data > (*root)->data)
        {
            left_rotate(root);
        }
    }
    else if (cmp > 0)
    {
        tree_add(&(*root)->left, n);
        int data = (*root)->left->data;
        if (data > (*root)->data)
        {
            right_rotate(root);
        }
    }

    return;
}

void treeDestroy(Node *n)
{
    if (n == 0)
    {
        return;
    }
    Node *l = n->left;
    Node *r = n->right;
    free(n);
    treeDestroy(l);
    treeDestroy(r);
}

void inserterDestroy(Inserter i)
{
    treeDestroy(i->root);
    free(i);
}
