#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "pancake.h"

#define INITIAL_SIZE (256)  // initial buffer size
#define SIZE_MULTIPLIER (2) // how much to expand by

// Read sequence of unsigned ints from given FILE *
// using fscanf.
//
// Returns 0 on error.
//
// Otherwise returns a malloc'd array that the caller must free,
// and if argument n is nonzero, returns number of values read
// in *n.
//
static unsigned int *
readInts(FILE *f, size_t *n)
{
    size_t size = INITIAL_SIZE;
    size_t top = 0;
    unsigned int *a = malloc(sizeof(unsigned int) * size);
    unsigned int value;

    assert(a); // Linux malloc never fails, but let's try to be safe

    while (fscanf(f, "%u", &value) == 1)
    {
        if (top >= size)
        {
            size *= SIZE_MULTIPLIER;
            a = realloc(a, sizeof(unsigned int) * size);
            assert(a);
        }
        a[top++] = value;
    }

    fclose(f);

    if (n)
    {
        *n = top;
    }

    return a;
}

#define HASH_MULTIPLIER (11400714819323198485u)
#define HASH_SHIFT (32)

// return a hash function of array a
static unsigned long long
hash(const unsigned int *a, size_t n)
{
    unsigned long long h = 0;

    for (size_t i = 0; i < n; i++)
    {
        h = ((h >> HASH_SHIFT) ^ h) * HASH_MULTIPLIER + a[i];
    }

    return h;
}

int main(int argc, char **argv)
{

    if (argc != 1)
    {
        fprintf(stderr, "Usage: %s < memory-file\n", argv[0]);
        return 1;
    }

    size_t n;
    unsigned int *memory = readInts(stdin, &n);

    runPancakeProgram(memory);

    printf("%llx\n", hash(memory, n));

    free(memory);

    return 0;
}
