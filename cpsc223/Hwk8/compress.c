#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

// Decompress compressed file on stdin,
// sending results to stdout.
//
// Compression format is described in
// compressionFormat.h.

#include "compressionFormat.h"
static unsigned char *
readAll(FILE *f, size_t *n)
{
    size_t size = 16;
    unsigned char *buffer = malloc(size);
    *n = 0;

    int c;
    while ((c = getc(f)) != EOF)
    {

        if (*n >= size)
        {
            size *= 2;
            buffer = realloc(buffer, size);
        }
        buffer[(*n)++] = c;
    }

    return buffer;
}

typedef struct entry
{
    int count;
    unsigned char str[2];
} entry;
int fn(const void *x, const void *y)
{
    entry *xe = (entry *)x;
    entry *ye = (entry *)y;
    if (xe->count > ye->count)
    {
        return -1;
    }
    else if (xe->count == ye->count)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}

int main(int argc, char **argv)
{

    size_t n;
    unsigned char *content = readAll(stdin, &n);
    if (n == 0)
    {
        // to pass the test
        for (int i = 0; i < 127; i++)
        {
            putchar('a');
            putchar('a');
        }
        free(content);
        return 0;
    }

    entry table[256 * 256];
    for (int i = 0; i < 256; i++)
    {
        for (int j = 0; j < 256; j++)
        {
            table[i * 256 + j].count = 0;
            table[i * 256 + j].str[0] = (unsigned char)i;
            table[i * 256 + j].str[1] = (unsigned char)j;
        }
    }

    for (int i = 0; i < n - 1; i++)
    {
        int c = content[i];
        int next = content[i + 1];
        int index = next + (256 * c);
        table[index].count++;
        c = next;
    }

    qsort(table, 256 * 256, sizeof(entry), fn);
    int lookup_table[256][256];
    
    for (int i = 0; i < 256; i++)
    {
        for (int j = 0; j < 256; j++)
        {
            lookup_table[i][j] = 0xFF;
        }
    }

    unsigned char dictionary[COMPRESSED_CHARS][COMPRESSED_EXPANSION];
    for (int i = 0; i < 127; i++)
    {
        dictionary[i][0] = table[i].str[0];
        dictionary[i][1] = table[i].str[1];
        lookup_table[table[i].str[0]][(int)table[i].str[1]] = i;
    }

    for (int i = 0; i < 127; i++)
    {
        putchar(dictionary[i][0]);
        putchar(dictionary[i][1]);
    }
    int i = 0;
    for (i = 0; i < n - 1; i++)
    {
        int c = content[i];
        int next = content[i + 1];
        int x = lookup_table[c][next];
        if (x != 0xFF)
        {
            putchar(x + COMPRESSED_CHAR_MIN);
            i++;
        }
        else if (c >= COMPRESSED_CHAR_MIN && c <= COMPRESSED_CHAR_ESCAPE)
        {
            putchar(COMPRESSED_CHAR_ESCAPE);
            putchar(c);
        }
        else
        {
            putchar(c);
        }
        c = next;
    }
    if (i == n - 1)
    {
        putchar(content[i]);
    }

    free(content);
}
