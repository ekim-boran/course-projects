#include "buffer.h"
#include <stdio.h>
#include <stdlib.h>
typedef struct Node
{
    struct Node *next;
    struct Node *prev;
    char c;
} Node;

struct buffer
{
    Node *tail;
    Node **cursors;
    size_t number_of_cursors;
    size_t buffer_size;
};

// Make a buffer with n cursors.
Buffer
bufferCreate(size_t n)
{
    Buffer b = calloc(1, sizeof(struct buffer));
    b->number_of_cursors = n;
    b->cursors = (Node **)calloc(n, sizeof(Node *));
    b->tail = calloc(1, sizeof(Node));
    b->buffer_size = 0;
    for (int i = 0; i < n; i++)
    {
        b->cursors[i] = b->tail;
    }

    return b;
}

// Free all space used by a buffer.
void bufferDestroy(Buffer b)
{
    if (b->number_of_cursors != 0)
    {
        free(b->cursors);
    }
    Node *c = b->tail;
    while (c != 0)
    {
        Node *prev = c->prev;
        free(c);
        c = prev;
    }
    free(b);
}

// Insert a new non-null character before the i-th cursor position.
// All cursors continue to point to the same character
// they pointed to before.
// If i is out of range or c is '\0', has no effect.
void bufferInsert(Buffer b, size_t i, char c)
{
    if (i >= b->number_of_cursors || c == '\0')
    {
        return;
    }
    Node *ptr = b->cursors[i];
    Node *prev = ptr->prev;
    Node *newnode = calloc(1, sizeof(Node));
    newnode->c = c;
    newnode->prev = prev;
    newnode->next = ptr;
    ptr->prev = newnode;
    if (prev != 0)
    {
        prev->next = newnode;
    }
    b->buffer_size = b->buffer_size + 1;
}

// Advance cursor i one position.
// If i is out of range, or the i-th cursor
// already points to the final '\0',
// has no effect.
void bufferCursorForward(Buffer b, size_t i)
{
    if (i >= b->number_of_cursors)
    {
        return;
    }
    Node *ptr = b->cursors[i];
    if (ptr == b->tail)
    {
        return;
    }
    Node *next = ptr->next;
    b->cursors[i] = next;
}

// Move cursor i back one position.
// If i is out of range, or the i-th cursor
// already points to the first character,
// has no effect.
void bufferCursorBack(Buffer b, size_t i)
{
    if (i >= b->number_of_cursors)
    {
        return;
    }
    Node *ptr = b->cursors[i];
    Node *prev = ptr->prev;

    if (prev == 0)
    {
        return;
    }
    b->cursors[i] = prev;
}

// Return the number of cursors in a buffer
size_t bufferCursors(Buffer b)
{
    return b->number_of_cursors;
}

// Return the number of characters in a buffer,
// not including the final null.
size_t bufferSize(Buffer b)
{
    return b->buffer_size;
}

// Return the characters in a buffer
// as a null-terminated sequence of chars.
// Return value is malloc'd and should be freed by caller.
char *bufferContents(Buffer b)
{
    char *str = calloc(b->buffer_size + 1, 1);
    size_t index = b->buffer_size - 1;
    Node *c = b->tail->prev;
    while (c != 0)
    {
        str[index] = c->c;
        index--;
        c = c->prev;
    }
    str[b->buffer_size] = '\0';
    return str;
}
