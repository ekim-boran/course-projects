// interface for arrays keyed on fixed-length strings
// of lowercase letters
#include <ctype.h>

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include "wordArray.h"
struct wordArray
{
    int *buffer;
    int size;
};

// some constants for key elements
#define MIN_KEY_LETTER ('a')
#define MAX_KEY_LETTER ('z')
#define NUM_KEY_LETTERS (MAX_KEY_LETTER - MIN_KEY_LETTER + 1)

long long capacity(unsigned int n)
{
    long long size = 1;
    for (int i = 0; i < n; i++)
    {
        size *= NUM_KEY_LETTERS;
    }
    return size;
}

WordArray
wordArrayCreate(unsigned int n)
{
    unsigned int size = 1;
    for (int i = 0; i < n; i++)
    {
        size *= NUM_KEY_LETTERS;
    }
    int *buffer = (int *)calloc(size, sizeof(int));
    struct wordArray *wordarray = (struct wordArray *)malloc(sizeof(struct wordArray));
    wordarray->buffer = buffer;
    wordarray->size = n;
    return wordarray;
}

// free all space used by array
void wordArrayDestroy(WordArray w)
{
    free(w->buffer);
    free(w);
}

// get the key length n for an array
int wordArrayKeyLength(WordArray w)
{
    return w->size;
}

// Returns a pointer to location in word array
// indexed by string key.
//
// If key is the wrong length, or contains
// characters that are not between MIN_KEY and MAX_KEY,
// returns 0.

long long to_int(const char *key, int size)
{
    int index = 0;
    int multiplier = 1;
    for (int i = size - 1; i >= 0; i--)
    {
        index += (multiplier * (key[i] - MIN_KEY_LETTER));
        multiplier *= NUM_KEY_LETTERS;
    }
    return index;
}

int *wordArrayRef(WordArray w, const char *key)
{
    if (strlen(key) > w->size)
    {
        return 0;
    }
    long long x = to_int(key, w->size);
    return &(w->buffer[x]);
}

// returns malloc'd minimum key for w
// as a null-terminated string
char *wordArrayMinKey(WordArray w)
{
    char *m = malloc(w->size + 1);
    for (int i = 0; i < w->size; i++)
    {
        m[i] = 'a';
    }
    m[w->size] = 0;
    return m;
}

// cannot convert to long long first it can overflow
// do the artihmetic directly on string
int wordArrayIncKey(char *s)
{
    int size = strlen(s);
    if (size == 0)
    {
        return 1;
    }
    int carry = 1;
    for (int i = size - 1; i >= 0; i--)
    {
        char k = s[i] + carry;
        if (k > 'z')
        {
            k = 'a';
            carry = 1;
        }
        else
        {
            carry = 0;
        }
        s[i] = k;
    }

    return carry;
}

// print the contents of an array,
// with one key and value on each line, as in
//
// aa 0
// ab 0
// ac 0
// ...
//
void wordArrayPrint(WordArray w)
{
    char *key = wordArrayMinKey(w);
    do
    {
        printf("%s %d\n", key, *wordArrayRef(w, key));
    } while (wordArrayIncKey(key) == 0);
    free(key);
}
